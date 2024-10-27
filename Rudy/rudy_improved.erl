-module(rudy_improved).
-import(string,[concat/2]).
-export([init/1, start/1, stop/0]).

init(Port) ->
  Opt = [list, {active, false}, {reuseaddr, true}],
  case gen_tcp:listen(Port, Opt) of
    {ok, Listen} ->
      io:format("Listening on port ~p~n", [Port]),
      handler(Listen),
      gen_tcp:close(Listen),
      ok;
    {error, Error} ->
      io:format("Error: ~p~n", [Error]),
      error
  end.

handler(Listen) ->
  case gen_tcp:accept(Listen) of
    {ok, Client} ->
      spawn(fun() -> request(Client) end),
      handler(Listen);
    {error, Error} ->
      io:format("Error: ~p~n", [Error]),
      error
  end.

request(Client) ->
  Recv = gen_tcp:recv(Client, 0),
  case Recv of
    {ok, Str} ->
      Request = http:parse_request(Str),
      Response = reply(Request),
      gen_tcp:send(Client, Response);
    {error, Error} ->
      io:format("rudy: error ~w~n", [Error])
  end,
  gen_tcp:close(Client).

reply({{get, URI, _}, _Headers, _Body}) ->
  %Path = concat(["www", URI]),
  %io:format("Path is ~w~n",Path),
  case file:read_file("www"++URI) of
    {ok, Content} ->
      MimeType = get_mime_type(URI),
      Header = io_lib:format(
        "HTTP/1.1 200 OK\r\nContent-Type: ~s\r\nContent-Length: ~p\r\n\r\n",
        [MimeType, byte_size(Content)]
      ),
      [Header, Content];
    {error, _} ->
      send_response("404 Not Found", "File Not Found")
  end;
reply(_) ->
  send_response("405 Method Not Allowed", "Method Not Allowed").


send_response(Status, Message) ->
  BinMessage = erlang:list_to_binary(Message),
  Header = io_lib:format(
    "HTTP/1.1 ~s\r\nContent-Type: text/plain\r\nContent-Length: ~p\r\n\r\n",
    [Status, byte_size(BinMessage)]
  ),
  [Header, BinMessage].


get_mime_type(Path) ->
  case filename:extension(Path) of
    ".html" -> "text/html";
    ".css" -> "text/css";
    ".js" -> "application/javascript";
    ".png" -> "image/png";
    ".jpg" -> "image/jpeg";
    ".gif" -> "image/gif";
    _ -> "application/octet-stream"
  end.

start(Port) ->
  register(rudy_improved, spawn(fun() -> init(Port) end)).

stop() ->
  exit(whereis(rudy_improved), "time to die").
