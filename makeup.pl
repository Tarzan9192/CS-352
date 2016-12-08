
:- dynamic prop/1.
:- dynamic n/1.
:- dynamic adj/1.
:- dynamic thing/1.

size(big).
size(huge).
size(large).
size(medium).
size(small).
size(little).
size(tiny).

color(red).
color(yellow).
color(blue).
color(green).
color(purple).
color(orange).
color(black).
color(white).
color(gold).
color(silver).

length(inches).
length(yards).
length(feet).
length(centimeters).
length(meters).
length(kilometers).

capacity(liters).
capacity(mililiters).

execute(X,R):- s(X,[],R).

% Not used.
say(X):- s(X,[]).

% This predicate represents a query from
% the user.
s(X,Y,R):-
  qp(X,Z),
  propf(Z,[Noun|Y],[Prop|_]),
  R = ["I don't know"],
  \+ thing([Prop,Noun,_]),!.

% This predicate is for when prolog knows about the
% thing being queried.
s(X,Y,R):-
  qp(X,Z),
  propf(Z,[Noun|Y],[Prop|_]),
  thing([Prop,Noun,Adj]),
  R = ["The",Prop,"of the",Noun,"is",Adj].
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
  P = [Prop,Noun,_],
  thing([Prop,Noun,RealAdj]),
  R = ["No, the",Prop,"of the",Noun,"is",RealAdj],
  \+ thing(P),!.

s(X,Y,R):-
  det(X,Z),
  ap(Z,Y,P),
  assert(thing(P)),
  R = ['Ok'],!.

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
  P = [Prop,Noun,_],
  thing([Prop,Noun,RealAdj]),
  R = ["No, the",Prop,"of the",Noun,"is",RealAdj],
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
%
% The last argument is an accumulator, which will
% accumulate the definition of the thing as [Property,Noun,Adjective].
propf([Prop|T],R,[Prop,_,_]):-
  desc(T,R).

% This predicate represents a 'describer',
% or a prepostiional phrase, i.e. "...of the...".
desc(X,Y):-
  prep(X,Z),
  det(Z,Y).

% This predicate represents an assignment phrase
% i.e. "<noun> is <adjective>". Used to represent an adjective
% being assigned to a noun.

% Check for 'color' property
ap([N|T],R,[P,N,Adj]):-
  asign(T,Adj),
  var(P),
  Adj = [A|_],
  color(A),
  P = color,
  R = [],!.

% Check for 'size' property
ap([N|T],R,[P,N,Adj]):-
  asign(T,Adj),
  var(P),
  Adj = [A|_],
  size(A),
  P = size,
  R = [],!.

% Check for 'length' property
ap([N|T],R,[P,N,Adj]):-
  asign(T,Adj),
  var(P),
  Adj = [_,A|_],
  length(A),
  P = length,
  R = [],!.

% Check for 'capacity' property
ap([N|T],R,[P,N,Adj]):-
  asign(T,Adj),
  var(P),
  Adj = [_,A|_],
  capacity(A),
  P = capacity,
  R = [],!.

% Default, property is uninstantiated.
% I can add more recognition capabilities
% by initializing more property predicates and adding more
% recogniziotn predicates here, thus allowing the
% program to recognize more types of properties.
ap([N|T],R,[_,N,Adj]):-
  asign(T,Adj),
  R = [].

% This predicate represents a 'query phrase',
% i.e. "What is the..."
qp(X,Y):-
  query(X,Z),
  asign(Z,V),
  det(V,Y).

det([the|W],W).
prep([of|W],W).
asign([is|W],W).
query(['What'|W],W).
query([what|W],W).
