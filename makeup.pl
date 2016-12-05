
:- dynamic prop/1.
:- dynamic n/1.
:- dynamic adj/1.
:- dynamic thing/1.

execute(X,R):- s(X,[],R).

say(X):- s(X,[]).

% This predicate represents a query from
% the user.
s(X,Y,R):-
  qp(X,Z),
  propf(Z,[Noun|Y],[Prop|_]),
  R = ["I don't know"],
  \+ thing([Prop,Noun,_]),!.


s(X,Y,R):-
  qp(X,Z),
  propf(Z,[Noun|Y],[Prop|_]),
  thing([Prop,Noun,Adj]),
  R = ["The",Noun,"is",Adj].
  % --------------------------------%

% ----When property is not given.----%

% If the thing is known.
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

% If no contradiciton, assert thing.
s(X,Y,R):-
  det(X,Z),
  ap(Z,Y,P),
  assert(thing(P)),
  R = ['Ok'].
  % ---------------------------------%

%--- When the property IS given------%

% If thing is known.
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

% If no contradiciotn, assert thing with property name.
s(X,Y,R):-
  det(X,Z),
  propf(Z,A,P),
  ap(A,Y,P),
  assert(thing(P)),
  R = ['Ok'].
  % ----------------------------------%

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

qp(X,Y):-
  query(X,Z),
  asign(Z,V),
  det(V,Y).

det([the|W],W).
prep([of|W],W).
asign([is|W],W).
query(['What'|W],W).
query([what|W],W).
