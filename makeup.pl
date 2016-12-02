
:- dynamic prop/1.
:- dynamic n/1.
:- dynamic adj/1.

say(X):- s(X,[]).

% When property is not given.
s(X,Y):-
  det(X,Z),
  ap(Z,Y).

s(X,Y):-
  det(X,Z),
  propf(Z,A),
  ap(A,Y).

propf([P|T],R):-
  desc(T,R),
  assert(prop(P)).

desc(A,B):-
  prep(A,C),
  det(C,B).

ap([N|T],R):-
  asign(T,Adj),
  assert(n(N)),
  assert(adj(Adj)),
  R = [].

det([the|W],W).
prep([of|W],W).
asign([is|W],W).
