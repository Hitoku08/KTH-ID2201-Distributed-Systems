-module(routy).
-export([start/2,stop/1]).

start(Reg,Name)->
     register(Reg, spawn(fun()-> init(Name) end)).

stop(Node)->
    Node ! stop,
    unregister(Node).

init(Name)->
    Intf = intf:new(),
    Map = map:new(),
    Table = dijkstra:table(Intf, Map),
    Hist = hist:new(Name),
    router(Name, 0, Hist, Intf, Table, Map).

router(Name, N, Hist, Intf, Table, Map)->
    receive
        {add, Node, Pid}->
            io:format("successfully added~n"),
            Ref = erlang:monitor(process,Pid),
            Intf1 = intf:add(Node, Ref, Pid, Intf),
            router(Name, N, Hist, Intf1, Table, Map);
        {remove, Node}->
            {ok, Ref} = intf:ref(Node, Intf),
            erlang:demonitor(Ref),
            Intf1 = intf:remove(Node, Intf),
            router(Name, N, Hist, Intf1, Table, Map);
        {'DOWN', Ref, process, _, _}->
            {ok, Down} = intf:name(Ref, Intf),
            io:format("~w: exit recived from ~w~n", [Name, Down]),
            Intf1 = intf:remove(Down, Intf),
            router(Name, N, Hist, Intf1, Table, Map);
        {status, From}->
            From ! {status, {Name, N, Hist, Intf, Table, Map}},
            router(Name, N, Hist, Intf, Table, Map);

        {links, Node, R, Links}->
            %io:format("History of ~w:~w~n",[Name,Hist]),
            case hist:update(Node, R, Hist) of
                {new, Hist1}->
                    %io:format("New history of ~w:~w~n",[Name,Hist1]),
                    intf:broadcast({links, Node, R, Links}, Intf),
                    Map1 = map:update(Node, Links, Map),
                    router(Name, N, Hist1, Intf, Table, Map1);
                old->
                    %io:format("The history is old~n"),
                    router(Name, N, Hist, Intf, Table, Map)
            end;
        update->
            Table1 = dijkstra:table([Name | intf:list(Intf)], Map),
            %io:format("Successfully updated~n"),
            router(Name, N, Hist, Intf, Table1, Map);
        broadcast->
            Message = {links, Name, N, intf:list(Intf)},
            intf:broadcast(Message, Intf),
            io:format("Broadcasting from ~w, the message is ~w~n",[Name,Message]),
            router(Name, N+1, Hist, Intf, Table, Map);
        display->
            io:format("Name : ~w~nN : ~w~nHist : ~w~nIntf : ~w~nTable : ~w~nMap : ~w~n",
                [Name,N,Hist,Intf,Table,Map]),
            router(Name, N, Hist,Intf, Table, Map);

        %routing a  message
        {route, Name, From, Message}->
            io:format("~w: received message ~w from ~w~n", [Name, Message,From]),
            router(Name, N, Hist, Intf, Table, Map);
        {route, To, From, Message}->
            io:format("~w: routing message ~w from ~w to ~w~n", [Name, Message,From,To]),
            case dijkstra:route(To, Table) of
                {ok,Gw}->
                    %io:format("Gateway is ~w~n",[Gw]),
                    case intf:lookup(Gw,Intf) of
                        {ok,Pid}->
                            Pid ! {route,To,From,Message};
                        notfound->
                            ok
                    end;
                noufound->
                    ok
            end,
            router(Name, N, Hist, Intf, Table, Map);
        {send, To, Message}->
            self() ! {route, To, Name, Message},
            router(Name, N, Hist, Intf, Table, Map);

        stop->
            ok
    end.
