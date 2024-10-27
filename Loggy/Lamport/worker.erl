-module(worker).
-export([start/5,stop/1,peers/2]).

start(Name,Logger,Seed,Sleep,Jitter)->
    spawn_link(fun()->init(Name,Logger,Seed,Sleep,Jitter) end).

stop(Worker)->
    Worker ! stop.

init(Name,Log,Seed,Sleep,Jitter)->
    rand:seed(exsplus, {Seed, Seed, Seed}),
    receive
        {peers,Peers}->
            loop(Name,Log,Peers,Sleep,Jitter,time:zero());
        stop->
            ok
    end.

peers(Wrk,Peers)->
    Wrk ! {peers,Peers}.

loop(Name, Log, Peers, Sleep, Jitter, Time)->
    Wait = rand:uniform(Sleep),
    receive
        {msg,Sender_Time,Msg}->
            New_Time=time:inc(Name,time:merge(Sender_Time,Time)),
            Log ! {log,Name,New_Time,{received,Msg}},
            loop(Name, Log, Peers, Sleep, Jitter,New_Time);
        stop->
            ok;
        Error->
            Log ! {log, Name, time, {error, Error}}
    after Wait->
        Selected=select(Peers),
        New_Time=time:inc(Name,Time),
        Message = {hello, rand:uniform(100)},
        Selected ! {msg, New_Time, Message},
        jitter(Jitter),
        Log ! {log, Name, New_Time, {sending, Message}},
        loop(Name, Log, Peers, Sleep, Jitter,New_Time)
    end.

select(Peers)->
    lists:nth(rand:uniform(length(Peers)), Peers).

jitter(0)-> ok;
jitter(Jitter)-> timer:sleep(rand:uniform(Jitter)).

