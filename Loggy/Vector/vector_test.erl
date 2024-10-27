-module(vector_test).
-export([run/2]).

run(Sleep,Jitter)->
    Log = vector_logger:start([john, paul, ringo, george]),
    A = vector_worker:start(john, Log, 13, Sleep, Jitter),
    B = vector_worker:start(paul, Log, 23, Sleep, Jitter),
    C = vector_worker:start(ringo, Log, 36, Sleep, Jitter),
    D = vector_worker:start(george, Log, 49, Sleep, Jitter),
    vector_worker:peers(A, [B, C, D]),
    vector_worker:peers(B, [A, C, D]),
    vector_worker:peers(C, [A, B, D]),
    vector_worker:peers(D, [A, B, C]),
    timer:sleep(5000),
    vector_logger:stop(Log),
    vector_worker:stop(A),
    vector_worker:stop(B),
    vector_worker:stop(C),
    vector_worker:stop(D).