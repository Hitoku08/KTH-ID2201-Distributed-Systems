-module(intf).
-export([new/0, add/4, remove/2, lookup/2, ref/2, name/2, list/1, broadcast/2]).

new()->
    [].

add(Name,Ref,Pid,Intf)->
    Added=[{Name,Ref,Pid} | Intf],
    Added.

remove(Name,Intf)->
    Removed=lists:keydelete(Name,1,Intf),
    Removed.

lookup(Name,Intf)->
    case lists:keyfind(Name,1,Intf) of
        false->notfound;
        {_,_,Pid}->{ok,Pid}
    end.

ref(Name,Intf)->
    case lists:keyfind(Name,1,Intf) of
        false->notfound;
        {_,Ref,_}->{ok,Ref}
    end.

name(Ref, Intf)->
    case lists:keyfind(Ref,2,Intf) of
        false->notfound;
        {Name,_,_}->{ok,Name}
    end.

list(Intf)->
    Names=lists:map(fun({Name,_,_})->Name end, Intf),
    Names.

broadcast(Message, Intf)->
    lists:foreach(fun({_,_,Pid})->Pid ! Message end,Intf).
