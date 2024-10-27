-module(vector_logger).
-export([start/1,stop/1,process_queue/2]).

start(Nodes)->
    spawn_link(fun()->init(Nodes) end).

stop(Logger)->
    Logger ! stop.

init(Nodes)->
    Clock=vector:clock(Nodes),
    loop(Clock,[]).

loop(Clock,Queue)->
    receive
        {log,From,Time,Msg}->
            New_Clock=vector:update(From,Time,Clock),
            New_Queue=lists:sort(fun({_,T1,_},{_,T2,_})->vector:leq(T1,T2) end,
            [{From,Time,Msg} | Queue]),
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
            case vector:safe(Time,Clock) of
                true->
                    log(From,Time,Msg),
                    process_queue(Clock,Rest);
                false->
                    Queue
            end
    end.
