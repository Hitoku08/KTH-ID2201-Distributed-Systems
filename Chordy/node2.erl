-module(node2).
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
    node(Id, Predecessor, Successor,Store).

connect(Id, nil)->
    {ok, {Id,self()}};
connect(_Id, Peer)->
    Qref = make_ref(),
    Peer ! {key, Qref, self()},
    receive
        {Qref, Skey}->
            {ok,{Skey,Peer}}%Skey is the id of Successor; Peer is the value of Successor, or rather, Pid of successor.
    after ?timeout->
        io:format("Time out: no response~n",[])
    end.


node(Id, Predecessor, Successor, Store)->
    receive
        {key,Qref,Peer}->%a peer needs to know our key;
            Peer ! {Qref,Id},
            io:format("Peer ~w knows our key ~w~n",[Peer,Id]),
            node(Id, Predecessor, Successor,Store);
        {notify,New}->%A new node informs us of its existence, and want to be our new Predeceoosr
            {Pred,Keep}=notify(New,Id,Predecessor,Store),
            node(Id, Pred, Successor,Keep);
        {request, Peer}->%a predecessor needs to know our predecessor;
            request(Peer, Predecessor),
            node(Id, Predecessor, Successor,Store);
        {status, Pred}->%our successor informs us about its predecessor. Pred is our Successor's Predecessor.
            Succ = stabilize(Pred, Id, Successor),
            node(Id, Predecessor, Succ,Store);
        stabilize ->
            stabilize(Successor),%Send a meaasge to Our Successor. Spid ! {request,self()}.
            node(Id, Predecessor, Successor, Store);
        %We can introduce a probe message to check if the ring is actually connected.
        probe->
            create_probe(Id, Successor),
            node(Id, Predecessor, Successor, Store);      
        {probe, Id, Nodes, T}->
            remove_probe(T, Nodes),
            node(Id, Predecessor, Successor, Store);
        {probe, Ref, Nodes, T}->
            forward_probe(Ref, T, Nodes, Id, Successor),
            node(Id, Predecessor, Successor, Store);
        
        {add, Key, Value, Qref, Client}->
            Added = add(Key, Value, Qref, Client,
            Id, Predecessor, Successor, Store),
            node(Id, Predecessor, Successor, Added);
        {lookup, Key, Qref, Client}->
            lookup(Key, Qref, Client, Id, Predecessor, Successor, Store),
            node(Id, Predecessor, Successor, Store);
        {handover, Elements}->
            Merged = storage:merge(Store, Elements),
            node(Id, Predecessor, Successor, Merged);
        
        status->
            Self={Id,self()},
            io:format("Self:~w, Successor:~w, Predecessor:~w~n",[Self,Successor,Predecessor]),
            node(Id, Predecessor, Successor, Store);
        stop->
            ok
    end.

stabilize(Pred, Id, Successor)->% Pred is our successor’s current predecessor
    {Skey, Spid} = Successor,
    case Pred of
        nil-> 
            %If this is nil, we should inform it about our existence
            Spid ! {notify, {Id, self()}},
            Successor;
        {Id, _}->
            % If it points back to us, we don’t have to do anything.
            Successor;
        {Skey,_}->
            %If it is pointing to itself, we should, of course, notify it about our existence.
            Spid ! {notify, {Id, self()}},
            Successor;
        {Xkey, Xpid}->
            case key:between(Xkey, Id, Skey) of
                true->
                    % adopt this node as our successor and run stabilization again. 
                    %stabilize(Pred,Id,Pred),
                    Xpid ! {notify,{Id,self()}},
                    Pred;
                false->
                    %If we should be between the nodes, we inform our successor of our existence.
                    Spid ! {notify, {Id, self()}},
                    Successor
            end
        end.

schedule_stabilize()->
    timer:send_interval(?Stabilize, self(), stabilize).

stabilize({_, Spid}) ->
    Spid ! {request, self()}.
                   
request(Peer, Predecessor) ->
    case Predecessor of
        nil ->
            Peer ! {status, nil};
        {Pkey, Ppid} ->
            Peer ! {status, {Pkey, Ppid}}
    end.
    %Why not just Peer ! {status,Predecessor}?

%A new peer want to be our predecessor
% notify({Nkey, Npid}, Id, Predecessor)->
%     case Predecessor of
%         nil->
%             {Nkey,Npid};
%         {Pkey,_}->
%             case key:between(Nkey,Pkey,Id) of
%                 true->
%                     {Nkey,Npid};
%                 false->
%                     Predecessor
%             end
%     end.

notify({Nkey, Npid}, Id, Predecessor, Store)->
    case Predecessor of
        nil->
            Keep = handover(Id, Store, Nkey, Npid),
            {{Nkey,Npid},Keep};
        {Pkey,_}->
            case key:between(Nkey,Pkey,Id) of
                true->
                    Keep = handover(Id, Store, Nkey, Npid),
                    {{Nkey,Npid},Keep};
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
    {_, Spid} = Successor,
    Spid ! {probe, Id, Nodes, Time}.

remove_probe(T0, Nodes) ->
    T1 = erlang:system_time(micro_seconds),
    DT = T1 - T0,    
    io:format("Time taken for probe are ~w in circle~w~n",[DT, Nodes]).

forward_probe(Ref, T, Nodes, _Id, Successor) ->
    Nodes2 = lists:append(Nodes, [self()]),
    {_, Spid} = Successor,
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


