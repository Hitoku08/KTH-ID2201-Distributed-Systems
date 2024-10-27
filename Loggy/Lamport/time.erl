-module(time).
-export([zero/0, inc/2, merge/2, leq/2,clock/1,update/3,safe/2]).

zero()->
    0.

inc(Name,T)->
    T+1.

merge(Ti,Tj)->
    max(Ti,Tj).

leq(Ti,Tj)->
    Ti=<Tj.

clock(Nodes)->
    lists:foldl(fun(Node,Acc)->[{Node,zero()} | Acc] end,[],Nodes).

update(Node,Time,Clock)->
    case lists:keyfind(Node,1,Clock) of 
        false->
            [{Node,Time}|Clock];
        {Node,_}->
            lists:keyreplace(Node,1,Clock,{Node,Time})
        end.

safe(Time,Clock)->
    %sort the Clock, and choose the first entry to compare
    [{_,T0} | _]=Clock,
    leq(Time,T0).
