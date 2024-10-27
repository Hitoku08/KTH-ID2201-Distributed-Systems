-module(node3).
-export([start/1,start/2]).
-define(Stabilize,1000).
-define(timeout,5000).

start(Id)->
    start(Id, nil).
start(Id, Peer)->
    timer:start(),
    Pid=spawn(fun()-> init(Id, Peer) end),
    io:format("The pid of Node ~w is ~w~n",[Id,Pid]),
    Pid.

init(Id, Peer)->
    Predecessor = nil,
    {ok, Successor} = connect(Id, Peer),
    schedule_stabilize(),
    Store=[],
    io:format("Node ~w is initiated with pid of ~w and Successor of ~w~n",[Id,self(),Successor]),
    node(Id, Predecessor, Successor,Store,nil).

connect(Id, nil)->
    {ok, {Id,nil,self()}};
connect(_Id, Peer)->
    Qref = make_ref(),
    Peer ! {key, Qref, self()},
    receive
        {Qref, Skey}->
            %Monitor our Successor
            Sref=monitor(Peer),
            {ok,{Skey,Sref,Peer}}%Skey is the id of Successor; Peer is the Pid of successor.
    after ?timeout->
        io:format("Time out: no response~n",[])
    end.


node(Id, Predecessor, Successor, Store, Next)->
    receive
        {key,Qref,Peer}->%a peer needs to know our key;
            Peer ! {Qref,Id},
            %io:format("Peer ~w knows our key ~w~n",[Peer,Id]),
            node(Id, Predecessor, Successor,Store, Next);
        {notify,New}->%A new node informs us of its existence, and want to be our new Predeceoosr
            {Pred,Keep}=notify(New,Id,Predecessor,Store),
            node(Id, Pred, Successor,Keep, Next);
        {request, Peer}->%a peer needs to know our predecessor;
            request(Peer, Predecessor,Successor),
            node(Id, Predecessor, Successor, Store, Next);
        {status, Pred, Nx}->%our successor informs us about its predecessor. 
            %Pred is our Successor's Predecessor.Nx is our Successor's Successor
            %Succ = stabilize(Pred, Id, Successor),
            {Succ, Nxt} = stabilize(Pred, Nx, Id, Successor),
            node(Id, Predecessor, Succ,Store,Nxt);
        stabilize ->
            stabilize(Successor),%Send a meaasge to Our Successor. Spid ! {request,self()}.
            node(Id, Predecessor, Successor, Store, Next);
        %We can introduce a probe message to check if the ring is actually connected.
        probe->
            create_probe(Id, Successor),
            node(Id, Predecessor, Successor, Store, Next);      
        {probe, Id, Nodes, T}->
            remove_probe(T, Nodes),
            node(Id, Predecessor, Successor, Store, Next);
        {probe, Ref, Nodes, T}->
            forward_probe(Ref, T, Nodes, Id, Successor),
            node(Id, Predecessor, Successor, Store, Next);
       
        {add, Key, Value, Qref, Client}->
            Added = add(Key, Value, Qref, Client,
            Id, Predecessor, Successor, Store),
            node(Id, Predecessor, Successor, Added, Next);
        {lookup, Key, Qref, Client}->
            lookup(Key, Qref, Client, Id, Predecessor, Successor, Store),
            node(Id, Predecessor, Successor, Store, Next);
        {handover, Elements}->
            Merged = storage:merge(Store, Elements),
            node(Id, Predecessor, Successor, Merged, Next);

        {'DOWN', Ref, process, _, _}->
            io:format("A Node is down!!~n"),
            {Pred, Succ, Nxt} = down(Ref, Predecessor, Successor, Next),
            node(Id, Pred, Succ, Store, Nxt);

        status->
            Self={Id,self()},
            io:format("Self:~w, Successor:~w, Predecessor:~w~n",[Self,Successor,Predecessor]),
            node(Id, Predecessor, Successor, Store, Next);
        stop->
            ok

    end.

stabilize(Pred, Nx, Id, Successor)->% Pred is our successor’s current predecessor
    {Skey, Sref, Spid} = Successor,
    case Pred of
        nil-> 
            %If this is nil, we should inform it about our existence
            Spid ! {notify, {Id, self()}},
            {Successor,Nx};
        {Id, _,_}->
            % If it points back to us, we don’t have to do anything.
            {Successor,Nx};
        {Skey,_,_}->
            %If it is pointing to itself, we should, of course, notify it about our existence.
            Spid ! {notify, {Id, self()}},
            {Successor,Nx};
        {Xkey,_, Xpid}->
            case key:between(Xkey, Id, Skey) of
                true->
                    % adopt this node as our successor and run stabilization again. 
                    %stabilize(Pred,Id,Pred),
                    Xpid ! {notify,{Id,self()}},
                    drop(Sref),%Stop monitoring our Old Successor
                    Xref=monitor(Xpid),%Start to monitor the new Successor.
                    {{Xkey,Xref,Xpid},nill};
                false->
                    %If we should be between the nodes, we inform our successor of our existence.
                    Spid ! {notify, {Id, self()}},
                    {Successor,Nx}
            end
        end.

schedule_stabilize()->
    timer:send_interval(?Stabilize, self(), stabilize).

stabilize({_,_, Spid}) ->
    Spid ! {request, self()}.
                   

request(Peer, Predecessor, Successor) ->
    % case Predecessor of
    %     nil ->
    %         Peer ! {status, nil, Successor};
    %     {Pkey, Ppid} ->
    %         Peer ! {status, {Pkey, Ppid}, Successor}
    % end. 
    % %Why not just Peer ! {status,Predecessor}?
    Peer ! {status,Predecessor,Successor}.
   

%A new peer want to be our predecessor
notify({Nkey, Npid}, Id, Predecessor, Store)->
    Keep = handover(Id, Store, Nkey, Npid),
    case Predecessor of
        nil->
            Nref=monitor(Npid),
            {{Nkey,Nref,Npid},Keep};
        {Pkey,Pref,_}->
            case key:between(Nkey,Pkey,Id) of
                true->
                    % When a new node is adopted as a successor or predecessor, we need to
                    %  de-monitor the old node and monitor the new one.
                    drop(Pref),
                    Nref=monitor(Npid),
                    {{Nkey,Nref,Npid},Keep};
                false->
                    {Predecessor,Store}
            end
    end.

handover(Id, Store, Nkey, Npid)->
    {Keep,Rest} = storage:split(Nkey, Id, Store),
    Npid ! {handover, Rest},
    Keep.

create_probe(Id, Successor) ->
    Time = erlang:system_time(micro_seconds),
    Nodes = [self()],
    {_,_, Spid} = Successor,
    Spid ! {probe, Id, Nodes, Time}.
remove_probe(T0, Nodes) ->
    T1 = erlang:system_time(micro_seconds),
    DT = T1 - T0,    
    io:format("Time taken for probe are ~w in circle~w~n",[DT, Nodes]).
forward_probe(Ref, T, Nodes, _Id, Successor) ->
    Nodes2 = lists:append(Nodes, [self()]),
    {_,_, Spid} = Successor,
    Spid ! {probe, Ref, Nodes2, T}.

add(Key, Value, Qref, Client, Id, {Pkey, _}, {_, Spid}, Store)->
    case key:between(Key,Pkey,Id) of
        true->
            Client ! {Qref,ok},
            storage:add(Key,Value,Store);
        false->
            Spid ! {add, Key, Value, Qref, Client},
            Store%return Store itself
    end.

lookup(Key, Qref, Client, Id, {Pkey, _}, Successor, Store)->
    case key:between(Key,Pkey,Id) of
        true->
            Result = storage:lookup(Key, Store),
            io:format("Stored in Node:~w~n",[Id]),
            Client ! {Qref, Result};
        false->
            {_, Spid} = Successor,
            Spid ! {lookup, Key, Qref, Client}
    end.

%The impemented codes in node3
monitor(Pid)->
    erlang:monitor(process, Pid).

drop(nil)->
    ok;
drop(Ref)->
    erlang:demonitor(Ref, [flush]).

down(Ref, {_, Ref, _}, Successor, Next) ->
    {nil, Successor, Next};
down(Ref, Predecessor, {_, Ref, _}, {Nkey, _, Npid}) ->
    %monitor the node and make sure that we run the stabilizing procedure.
    self() ! stabilize,
    Nref = monitor(Npid),
    io:format("Monitoring Successor~w~n", [Nkey]),
    {Predecessor, {Nkey, Nref, Npid}, nil}.