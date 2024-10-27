-module(dijkstra).
-export([entry/2,update/4,iterate/3,table/2,route/2]).

entry(Node,Sorted)->
    case lists:keyfind(Node,1,Sorted) of
        false->0;
        {_,Path,_}->Path
    end.

replace(Node,N,Gateway,Sorted)->
    Temp = [{Node,N,Gateway} | lists:keydelete(Node,1,Sorted)],
    lists:sort(fun({_,A,_},{_,B,_})->A=<B end,Temp).
    
update(Node,N,Gateway,Sorted)->
    Distance=entry(Node,Sorted),
    if Distance=<N ->
        Sorted;
    true ->
        replace(Node,N,Gateway,Sorted)
    end.


iterate([], _,Table)-> 
    Table;
iterate([{_,inf,_} | _],_,Table)->
    Table;
iterate(Sorted,Map,Table)->
    [{Node,N,Gateway} | T] = Sorted,
    Rch=map:reachable(Node,Map),
    New_Sorted=lists:foldl(fun(Ele,Acc)->update(Ele,N+1,Gateway,Acc) end, T, Rch),
    New_Table=[{Node,Gateway} | Table],
    iterate(New_Sorted,Map,New_Table).
    % case lists:keyfind(Node,1,Table) of
    %     false->
          
    %     _->Table
    % end.

    
  
table(Gateways,Map)->
    Gateway_Nodes=lists:map(fun(Node)->{Node,0,Node} end,Gateways),
    Non_Gateway_Nodes=lists:map(
        fun(Node)->{Node,inf,unknown} end,
        lists:subtract(map:all_nodes(Map),Gateways)
    ),
    Sorted=Gateway_Nodes++Non_Gateway_Nodes,
    iterate(Sorted,Map,[]).

route(Node,Table)->
    case lists:keyfind(Node,1,Table) of
        false->notfound;
        {_,unknown}->notfound;
        {_,Gateway}->{ok,Gateway}
    end.

    
        
    


    
    


        

