-module(key).
-export([generate/0,between/3]).

generate()->
    rand:uniform(1000000000).

% The between/3 function will check if a Key is between From and To or equal to To
between(Key, From, To) ->
    case {From, To} of
        {F, T} when F < T -> 
            F < Key andalso Key =< T;
        {F, T} when F > T -> %From is larger than To.
            F < Key orelse Key =< T;
        {F, T} when F == T -> %anything is in between
            true
    end.