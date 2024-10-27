-module(my_logger).
-export([start/1,stop/1,process_queue/2]).

start(Nodes)->
    spawn_link(fun()->init(Nodes) end).

stop(Logger)->
    Logger ! stop.

init(Nodes)->
    Clock=time:clock(Nodes),
    loop(Clock,[]).

loop(Clock,Queue)->
    receive
        {log,From,Time,Msg}->
            New_Clock=lists:keysort(2,time:update(From,Time,Clock)),
            New_Queue=lists:keysort(2,[{From,Time,Msg} | Queue]),
            RemainingQueue = process_queue(New_Clock, New_Queue),
            loop(New_Clock,RemainingQueue);
        stop->
            ok
    end.

log(From,Time,Msg)->
    io:format("log: ~w ~w ~p~n", [Time, From, Msg]).

process_queue(Clock, Queue) ->
    case Queue of
        []->
            [];
        [{From, Time, Msg} | Rest]->
            case time:safe(Time,Clock) of
                true->
                    log(From,Time,Msg),
                    process_queue(Clock,Rest);
                false->
                    Queue
            end
    end.
