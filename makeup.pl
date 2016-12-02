
:- dynamic prop/1.
:- dynamic n/1.
:- dynamic adj/1.
:- dynamic thing/1.

execute(X,R):- s(X,[],R).

say(X):- s(X,[]).

% When property is not given.
s(X,Y,R):-
  det(X,Z),
  ap(Z,Y,P),
  thing(P),
  R = ['I know'],!.

% When user enters contradicting adjective
s(X,Y,R):-
  det(X,Z),
  ap(Z,Y,P),
  P = [_,Noun,_],
  thing([_,Noun,RealAdj]),
  R = ["No, it's", RealAdj],
  \+ thing(P),!.

s(X,Y,R):-
  det(X,Z),
  ap(Z,Y,P),
  assert(thing(P)),
  R = ['Ok'].

% When the property IS given
s(X,Y,R):-
  det(X,Z),
  propf(Z,A,P),
  ap(A,Y,P),
  thing(P),
  R = ['I know'],!.

  % When user enters contradicting adjective
s(X,Y,R):-
  det(X,Z),
  propf(Z,A,P),
  ap(A,Y,P),
  P = [_,Noun,_],
  thing([_,Noun,RealAdj]),
  R = ["No, it's", RealAdj],
  \+ thing(P),!.

s(X,Y,R):-
  det(X,Z),
  propf(Z,A,P),
  ap(A,Y,P),
  assert(thing(P)),
  R = ['Ok'].

% This predicate represents a property phrase,
% which consists of a property followed by a
% prepostiional phrase, i.e. "...color of the...".
propf([P|T],R,[P,_,_]):-
  desc(T,R),
  assert(prop(P)).

% This predicate represents a 'describer',
% or a prepostiional phrase, i.e. "...of the...".
desc(X,Y):-
  prep(X,Z),
  det(Z,Y).

% This predicate represents an assignment phrase
% i.e. "is <adjective>". Used to represent an adjective
% being assigned to a noun.
ap([N|T],R,[_,N,Adj]):-
  asign(T,Adj),
  assert(n(N)),
  assert(adj(Adj)),
  R = [].

det([the|W],W).
prep([of|W],W).
asign([is|W],W).