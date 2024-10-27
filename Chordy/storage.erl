-module(storage).
-export([create/0,add/3,lookup/2,split/3,merge/2]).

create()->
    [].

add(Key,Value,Store)->
    %[Store|{Key,Value}].
    lists:keystore(Key, 1, Store, {Key, Value}).

lookup(Key,Store)->
    lists:keyfind(Key,1,Store).


split(From,To,Store)->
    %  return a tuple {Updated, Rest} where the
    %  updated store only contains the key-value pairs requested and the rest
    %  are found in a list of key-value pairs;
    lists:partition(fun({Key,_Value}) -> key:between(Key,From,To) end, Store).

merge(Entries,Store)->
    Store++Entries.

