-module(rudy).
-export([init/1,start/1,stop/0]).

init(Port)->
  Opt = [list, {active, false}, {reuseaddr, true}],
  case gen_tcp:listen(Port, Opt) of
    {ok, Listen}->
      io:format("Listening on port ~p~n", [Port]),
      handler(Listen),
      gen_tcp:close(Listen),
      ok;
    {error,Error}->
      error
  end.

handler(Listen)->
  case gen_tcp:accept(Listen) of
    {ok,Client}->
      spawn(fun() -> request(Client) end),  % 创建一个新进程处理客户端连接
      handler(Listen);  % 继续接受新的连接
    {error,Error}->
      error
  end.

request(Client)->
  Recv = gen_tcp:recv(Client,0),
  case Recv of
    {ok,Str}->
      Request=http:parse_request(Str),
      Response=reply(Request),
      gen_tcp:send(Client,Response);
    {error,Error}->
      io:format("rudy:error~w~n",[Error])
  end,
  gen_tcp:close(Client).

reply({{get, URI, _}, Headers, Body}) ->
  timer:sleep(40),
  ResponseBody = "URI: " ++ URI ++ "\n" ++
    "Headers: " ++ headers_to_string(Headers) ++ "\n" ++
    "Body: " ++ Body,

  http:ok(ResponseBody).

headers_to_string([]) ->
  "";
headers_to_string([H|T]) ->
  H ++ "\n" ++ headers_to_string(T).


start(Port)->
  register(rudy,spawn(fun()->init(Port) end)).
stop()->
  exit(whereis(rudy),"time to die").
