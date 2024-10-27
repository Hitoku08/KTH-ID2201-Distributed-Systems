-module(gms2).
-export([leader/4,slave/5,start/1,start/2,init/3,init/4]).
-define(timeout, 5000). % 5 秒
-define(arghh,100).
 
leader(Id, Master, Slaves, Group)->
    receive
        {mcast, Msg}->
            bcast(Id,{msg,Msg},Slaves),
            Master ! Msg,
            leader(Id,Master,Slaves,Group);
        %process identifier of the application layer, Wrk, and the process identifier of its group process.
        {join,Wrk,Peer}->
            Slaves2=lists:append(Slaves,[Peer]),
            Group2=lists:append(Group,[Wrk]),
            bcast(Id,{view,[self()|Slaves2],Group2},Slaves2),
            Master ! {view,Group2},
            leader(Id,Master,Slaves2,Group2);
        stop->
            ok
    end.

slave(Id, Master, Leader, Slaves, Group)->
    receive
        {mcast,Msg}->
            Leader ! {mcast,Msg},
            slave(Id, Master, Leader, Slaves, Group);
        {join,Wrk,Peer}->
            Leader ! {join,Wrk,Peer},
            slave(Id, Master, Leader, Slaves, Group);
        {msg,Msg}->
            Master ! Msg,
            slave(Id, Master, Leader, Slaves, Group);
        {view, [Leader|Slaves2], Group2}->
            Master ! {view,Group2},
            slave(Id, Master, Leader, Slaves2, Group2);
        {'DOWN', _Ref, process, Leader, _Reason}->
            %io:format("Leader ~p crashed",[Leader]),
            election(Id, Master, Slaves, Group);
        stop->
            ok
    
    end.

start(Id)->
    Self=self(),
    Rnd = rand:uniform(1000),
    io:format("Pid of Master of Leader ~p is ~p~n",[Id,Self]),
    {ok, spawn_link(fun()-> init(Id, Rnd, Self) end)}.

init(Id, Rnd, Master)->
    io:format("Pid of Leader ~p is ~p~n",[Id,self()]),
    rand:seed(exsplus,{Rnd, Rnd, Rnd}),
    leader(Id, Master, [], [Master]).

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
        {view,[Leader|Slaves],Group}->
            %Slaves have to monitor the Leader when created.
            erlang:monitor(process, Leader),
            Master ! {view,Group},
            slave(Id,Master,Leader,Slaves,Group)
    after ?timeout->
        Master ! {error, "no reply from leader"}
    end.

election(Id, Master, Slaves, [_|Group])->
    Self=self(),
    case Slaves of
        [Self|Rest]->
            bcast(Id, {view, Slaves, Group}, Rest),
            Master ! {view, Group},
            leader(Id, Master, Rest, Group);
        [Leader|Rest]->
            erlang:monitor(process, Leader),
            slave(Id, Master, Leader, Rest, Group)
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

