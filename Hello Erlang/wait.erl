-module(wait).
-export([hello/0]).
hello()->
  receive
    X->io:format("aaa! suprise,a message: ~s~n",[X])
  end.