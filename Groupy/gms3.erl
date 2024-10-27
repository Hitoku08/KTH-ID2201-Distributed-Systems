-module(gms3).
-export([leader/5,slave/7,start/1,start/2,init/3,init/4]).
-define(timeout, 5000). % 5 秒
-define(arghh,500).

%N is the sequence number of the next message (regular message or view) to be sent.
leader(Id, Master, N, Slaves, Group)->
    receive
        {mcast, Msg}->
            bcast(Id,{msg,N,Msg},Slaves),
            Master ! Msg,
            leader(Id,Master,N+1,Slaves,Group);
        %process identifier of the application layer, Wrk, and the process identifier of its group process.
        {join,Wrk,Peer}->
            Slaves2=lists:append(Slaves,[Peer]),
            Group2=lists:append(Group,[Wrk]),
            bcast(Id,{view,N,[self()|Slaves2],Group2},Slaves2),
            Master ! {view,Group2},
            leader(Id,Master,N+1,Slaves2,Group2);
        stop->
            ok
    end.

%N is the expected number of Slaves
slave(Id, Master, Leader, N, Last, Slaves, Group)->
    receive
        {mcast,Msg}->
            Leader ! {mcast,Msg},
            slave(Id, Master, Leader, N, Last, Slaves, Group);
        {join,Wrk,Peer}->
            %Wrk是Master的Pid
            Leader ! {join,Wrk,Peer},
            slave(Id, Master, Leader,N, Last, Slaves, Group);
        {msg,I,_} when I < N->%Old message
            slave(Id, Master, Leader, N, Last, Slaves, Group);
        {msg, N, Msg}->
            Master ! Msg,
            %The last message seen from Leader is {msg,I, Msg}.
            slave(Id, Master, Leader, N+1, {msg,N, Msg},Slaves, Group);
        {view, N, [Leader|Slaves2], Group2}->
            Master ! {view,Group2},
            slave(Id, Master, Leader,N+1,{view, N, [Leader|Slaves2], Group2}, Slaves2, Group2);
         {'DOWN', _Ref, process, Leader, _Reason}->
            %io:format("Leader ~p crashed",[Leader]),
            election(Id, Master, N, Last, Slaves, Group);
        stop->
            ok
    end.

%create a leader
start(Id)->
    Self=self(),
    Rnd = rand:uniform(1000),
    io:format("Pid of Master of Leader ~p is ~p~n",[Id,Self]),
    {ok, spawn_link(fun()-> init(Id, Rnd, Self) end)}.

init(Id, Rnd, Master)->
    io:format("Pid of Leader ~p is ~p~n",[Id,self()]),
    rand:seed(exsplus,{Rnd, Rnd, Rnd}),
    leader(Id, Master, 0, [], [Master]).

%create a slave
start(Id, Grp)->
    Self=self(),
    Rnd = rand:uniform(1000),
    io:format("Pid of Master of Slave ~p is ~p~n",[Id,Self]),
    {ok, spawn_link(fun()-> init(Id, Rnd, Grp, Self) end)}.

init(Id, Rnd, Grp, Master)->
    rand:seed(exsplus,{Rnd, Rnd, Rnd}),
    Self=self(),
    io:format("Pid of Slave ~p is ~p~n",[Id,Self]),
    Grp ! {join,Master,Self},
    receive
        {view, N, [Leader|Slaves], Group}->
            erlang:monitor(process, Leader),
            Master ! {view,Group},
            %Should N+1, Because already received a message with number N.
            slave(Id,Master,Leader,N+1, {view, N, [Leader|Slaves], Group}, Slaves,Group)
    after ?timeout->
        Master ! {error, "no reply from leader"}
    end.

election(Id, Master, N, Last, Slaves, [_|Group])->
    Self=self(),
    case Slaves of
        [Self|Rest]->
            %The next leader needs to resend the message. It is very important!
            bcast(Id, Last, Rest),
            bcast(Id, {view, N, Slaves, Group}, Rest),
            Master ! {view, Group},
            %shoule be N+1, because the already broadcasted the Last message.
            leader(Id, Master,N+1, Rest, Group);
        [Leader|Rest]->
            erlang:monitor(process, Leader),
            slave(Id, Master, Leader, N, Last, Rest, Group)
    end.

bcast(Id, Msg, Nodes) ->
    %io:format("Leader ~p broadcasting ~p to ~p~n", [Id, Msg, Nodes]),
    lists:foreach(fun(Node)-> Node ! Msg, crash(Id) end, Nodes).

crash(Id)->
    case rand:uniform(?arghh) of
        ?arghh->
            io:format("leader ~w: crash~n", [Id]),
            exit(no_luck);
        _->
            ok
    end.