-module(test).
-export([area/2,fac/1,sum/1]).
area(X,Y)->X*Y.
fac(N)->
  if
    N==0->1;
    N>0->N*fac(N-1)
  end.
sum(L)->
  case L of
    []->0;
    [H|T]->H+sum(T)
  end.