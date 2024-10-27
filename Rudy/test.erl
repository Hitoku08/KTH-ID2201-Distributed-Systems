-module(test).
-export([bench/2,spawn_bench/3]).

bench(Host, Port)->
  Start = erlang:system_time(micro_seconds),
  run(100, Host, Port),
  Finish = erlang:system_time(micro_seconds),
  Finish- Start.

run(N, Host, Port)->
  if
    N == 0->
      ok;
    true->
      request(Host, Port),
      run(N-1, Host, Port)
  end.

request(Host, Port)->
  Opt = [list, {active, false}, {reuseaddr, true}],
  {ok, Server} = gen_tcp:connect(Host, Port, Opt),
  gen_tcp:send(Server, http:get("foo")),
  Recv = gen_tcp:recv(Server, 0),
  case Recv of
    {ok, _}->
      ok;
    {error, Error}->
      io:format("test: error: ~w~n", [Error])
  end,
  gen_tcp:close(Server).

spawn_bench(Host, Port, NumProcesses) ->
  Pids = [spawn(fun() -> io:format("Process ~p: ~p microseconds~n", [self(), bench(Host, Port)]) end) || _ <- lists:seq(1, NumProcesses)],
  [erlang:monitor(process, Pid) || Pid <- Pids],
  wait_for_processes(Pids).

wait_for_processes(Pids) ->
  receive
    {'DOWN', _, process, Pid, _} ->
      io:format("Process ~p finished~n", [Pid]),
      RemainingPids = lists:delete(Pid, Pids),
      case RemainingPids of
        [] -> ok;
        _  -> wait_for_processes(RemainingPids)
      end
  end.