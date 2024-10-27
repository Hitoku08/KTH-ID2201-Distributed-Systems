-module(map).
-export([new/0,update/3,reachable/2,all_nodes/1]).

new()->
    [].

update(Node,Links,Map)->
    Temp=lists:keydelete(Node,1,Map),
    New_map=[{Node,Links} | Temp],
    New_map.

reachable(Node,Map)->
    case lists:keyfind(Node,1,Map) of
        false->[];
        {_,List}->List
    end.
   
    
all_nodes(Map)->
    AllNodes=lists:foldl(fun({Node,List},Acc)->[Node | List++Acc] end,[],Map),
    lists:usort(AllNodes).


