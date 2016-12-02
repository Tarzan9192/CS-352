%%---Example from section1 of 'Learn Prolog Now!'---%%
/*
loves(vincent, mia).
loves(marsellus, mia).
loves(pumpkin, honey_bunny).
loves(honey_bunny, pumpkin).

jealous(X,Y):- loves(X,Z), loves(Y,Z).
*/

%%---My own testing code---%%
/*
musicPlaying.
likesNoise(Z):-
likesQuiet(josh).


%This needs negation, currently doesn't work!
happy(X):- likesQuiet(X),
*/

%%---Example from unit 2 of 'Learn Prolog Now!'---%%
/*
vertical(line(point(X,Y), point(X,Z))).
horizontal(line(point(X,Y), point(Z,Y))).
*/

%%---Exercise 2.2 of 'Learn Prolog Now!'---%%
% wizard(harry).
% house_elf(dobby).
% witch(hermoine).
% witch('McGonagall').
% witch(rita_skeeter).
%
% magic(X):- house_elf(X).
% magic(X):- wizard(X).
% magic:- witch(X).

%%---Exercise 2.3 from 'Learn Prolog Now!---'%%
% THIS CODE COULD BE USEFUL FOR PROJECT!!!
% word(determiner,a).
% word(determiner,every).
% word(noun,criminal).
% word(noun,'big kahuna burger').
% word(verb,eats).
% word(verb,likes).
%
% sentence(Word1, Word2, Word3, Word4, Word5):-
%   word(determiner, Word1),
%   word(noun, Word2),
%   word(verb, Word3),
%   word(determiner, Word4),
%   word(noun, Word5).

% ---THIS COULD BE USEFUL FOR PROJECT!!!---
% s(Z):- np(X), vp(Y), append(X,Y,Z).
% np(Z):- det(X), n(Y), append(X,Y,Z).
% vp(Z):- v(X), np(Y), append(X,Y,Z).
% vp(Z):- v(Z).
% det([a]).
% det([every]).
% n([criminal]).
% n([big,kahuna,burger]).
% v([eats]).
% v([likes]).

%%---Excercise 2.4 from 'Learn Prolog Now!'---%%
% word(astante, a,s,t,a,n,t,e).
% word(astoria, a,s,t,o,r,i,a).
% word(baratto, b,a,r,a,t,t,o).
% word(cobalto, c,o,b,a,l,t,o).
% word(pistola, p,i,s,t,o,l,a).
% word(statale, s,t,a,t,a,l,e).
%
% crossword(Word1, Word2, Word3, Word4, Word5, Word6):-

% ---EXAMPLE 2.4 FROM 'Learn Prolog Now!'---
% add(0,Y,Y).
% add(succ(X),Y,succ(Z)):- add(X,Y,Z).

% ---EXCERCISE 3.2 FROM 'Learn Prolog Now!'---
% directlyIn(natasha, irina).
% directlyIn(olga, natasha).
% directlyIn(katrina, olga).
%
% in(X,Y):- directlyIn(X,Y).
% in(X,Y):- directlyIn(Z,Y),
%           in(X,Z).

% EXCERCISE 3.3 FROM 'Learn Prolog Now!'---
% directTrain(saarbruecken,dudweiler).
% directTrain(forbach,saarbruecken).
% directTrain(freyming,forbach).
% directTrain(stAvold,freyming).
% directTrain(fahlquemont,stAvold).
% directTrain(metz,fahlquemont).
% directTrain(nancy,metz).
%
% travelFromTo(X,Y):- directTrain(X,Y).
% travelFromTo(X,Y):- directTrain(Z,Y),
%                     travelFromTo(X,Z).

% ---EXCERCISE 3.4 FROM 'Learn Prolog Now!'---
% greater_than(succ(X),0).
% greater_than(succ(X),succ(Y)):-greater_than(X,Y).

% ---EXCERCISE 3.5 FROM 'Learn Prolog Now!'---
% swap(leaf(X), leaf(X)).
% swap(tree(B1,B2), tree(B1Swapped, B2Swapped)):-
%   swap(B1, B1Swapped),
%   swap(B2, B2Swapped).

% ---3.4 PRACTICAL SESSION---
% numeral(succ(X)):- numeral(X).
% numeral(0).

% connected(1,2).
% connected(3,4).
% connected(5,6).
% connected(7,8).
% connected(9,10).
% connected(12,13).
% connected(13,14).
% connected(15,16).
% connected(17,18).
% connected(19,20).
% connected(4,1).
% connected(6,3).
% connected(4,7).
% connected(6,11).
% connected(14,9).
% connected(11,15).
% connected(16,12).
% connected(14,17).
% connected(16,19).
%
% path(X,Y):- connected(X,Y).
% path(X,Y):- connected(X,Z),
%             path(Z,Y).

byCar(auckland,hamilton).
byCar(hamilton,raglan).
byCar(valmont,saarbruecken).
byCar(valmont,metz).

byTrain(metz,frankfurt).
byTrain(saarbruecken,frankfurt).
byTrain(metz,paris).
byTrain(saarbruecken,paris).

byPlane(frankfurt,bangkok).
byPlane(frankfurt,singapore).
byPlane(paris,losAngeles).
byPlane(bangkok,auckland).
byPlane(singapore,auckland).
byPlane(losAngeles,auckland).

% Base case
% travel(X,Y):-
%   byCar(X,Y);
%   byTrain(X,Y);
%   byPlane(X,Y).

% My first attempt. Can't tell the difference in approaches.
% travel(X,Y):-
%   travel(X,Z),
%   travel(Z,Y).

% travel(X,Y):-
%   byCar(X,Z),
%   travel(Z,Y).
% travel(X,Y):-
%   byPlane(X,Z),
%   travel(Z,Y).
% travel(X,Y):-
%   byTrain(X,Z),
%   travel(Z,Y).

% travel(X,Y,go(X,Y)):-
%   byCar(X,Y);
%   byPlane(X,Y);
%   byTrain(X,Y).

% Base Cases
travel(X,Y,go(byCar(X,Y))):-
  byCar(X,Y).
travel(X,Y,go(byTrain(X,Y))):-
  byTrain(X,Y).
travel(X,Y,go(byPlane(X,Y))):-
  byPlane(X,Y).


travel(X,Y,go(byCar(X,Z),G)):-
  byCar(X,Z),
  travel(Z,Y,G).

travel(X,Y,go(byTrain(X,Z),G)):-
  byTrain(X,Z),
  travel(Z,Y,G).

travel(X,Y,go(byPlane(X,Z),G)):-
  byPlane(X,Z),
  travel(Z,Y,G).

% travel(X,Y,go(X,Z,G)):-
%   travel(X,Z),
%   travel(Z,Y,G).

% ---WORKING WITH LISTS---%
% ------------------------%

% Check if an element is in list.
member(X,[X|_]).
member(X,[_|T]):- member(X,T).

getLast(X,[X|[]]).
getLast(X,[_|T]):- getLast(X,T).

a2b([],[]).
a2b([a|Ta],[b|Tb]):- a2b(Ta,Tb).

% length(count(X),[]).
% length()

second(X,[_,X|_]).

swap12(L1,L2):-
  L1 = [L1a,L1b|T],
  L2 = [L1b,L1a|T].

tran(eins,one).
tran(zwei,two).
tran(drei,three).
tran(vier,four).
tran(fuenf,five).
tran(sechs,six).
tran(sieben,seven).
tran(acht,eight).
tran(neun,nine).

% This predicate translates numbers between german
% and english.
listtran([],[]).
listtran([Gh|Gt],[Eh|Et]):-
  tran(Gh,Eh),
  listtran(Gt,Et).

% This predicate doubles the elements of
% the first list in the second list.
twice([],[]).
twice([H1|T1],[H1,H1|T2]):-
  twice(T1,T2).

% This predicate combines two lists sequentially
% into a third.

% Base cases
combine1([],[],[]).
combine1([],[H1|T1],[H1|T1]).
combine1([H1|T1],[],[H1|T1]).

combine1([H1|T1],[H2|T2],[H1,H2|T3]):-
  combine1(T1,T2,T3).

% combine2 predicate combines two lists into a third,
% adding the first two heads as a two element list to
% the third with each recursion.
% Base cases
combine2([],[],[]).
combine2([],[H1|T1],[H1|T1]).
combine2([H1|T1],[],[H1|T1]).

combine2([H1|T1],[H2|T2],[[H1,H2]|T3]):-
  combine2(T1,T2,T3).

% combine3 predicate combines two lists into a third,
% adding the heads of the first two lists together as
% a complext term to the third list with each recursion.
% Base cases
combine3([],[],[]).
combine3([],[H1|H2],[H1|H2]).
combine3([H1|H2],[],[H1|H2]).

combine3([H1|T1],[H2|T2],[j(H1,H2)|T3]):-
  combine3(T1,T2,T3).

% Example of using an accumulator to count the
% length of a list. 'A' is the accumulated value.
% 'L' is the length of the list.
accLen([_|T],A,L):-
  Anew is A+1,
  accLen(T,Anew,L).

accLen([],A,A).

% Helper predicate to call accLen with 'A'
% instanciated.
leng(List,Length):- accLen(List,0,Length).

% accMax will return the largest integer in
% the list given.
accMax([H|T],A,Max):-
  H > A,
  accMax(T,H,Max).

accMax([H|T],A,Max):-
  H =< A,
  accMax(T,A,Max).

accMax([],A,A).

% Helper predicate for accMax.
getMax(List,Max):-
  List = [H|_],
  accMax(List,H,Max).

accMin([H|T],A,Min):-
  H < A,
  accMin(T,H,Min).

accMin([H|T],A,Min):-
  H >= A,
  accMin(T,A,Min).

accMin([],A,A).

getMin(List,Min):-
  List = [H|_],
  accMin(List,H,Min).

% ---ARITHMETIC EXAMPLES---%
% -------------------------%

increment(X,Y):- Y is X+1.

sum(X,Y,Z):- Z is X+Y.

addOne([H1|T1],[H2|T2]):-
  H2 is H1+1,
  addOne(T1,T2).

addOne([],[]).

scalarMult(X,[H1|T1],[H2|T2]):-
  H2 is H1*X,
  scalarMult(X,T1,T2).

scalarMult(X,[],[]).

accDot([],[],A,A).

accDot([H1|T1],[H2|T2],A,R):-
  Anew is H1 * H2 + A,
  accDot(T1,T2,Anew,R).

dot(List1,List2,R):- accDot(List1,List2,0,R).

accRev([H|T],A,R):- accRev(T,[H|A],R).
accRev([],A,A).

revl(List1,Result):- accRev(List1,[],Result).

% ---EXAMPLES OF append/3 PREDICATE---%
%-------------------------------------%

prefix(P,L):- append(P,_,L).

suffix(S,L):- append(_,S,L).

sublist(SubL,L):- suffix(S,L), prefix(SubL,S).

doubled(List):- append(X,X,List).

palindrome(List):-
  revl(List,List).

% Using append predicate
% toptail(InList,OutList):-
%   append([_|OutList],[_],InList).

% Using reverse predicate
toptail([_|T],OutList):-
  revl(T,[_|RTail]),
  revl(RTail,OutList).

last(List,X):- revl(List,[X|_]).

% using recursion
% last([X],X).
% last([_|T],X):- last(T,X).

swapfl(List1,List2):-
  append([A|_],[_],List1),
  append([_|_],[A],List2).

% ---EXERCISE 6.6 FROM 'Learn Prolog Now!'---%

zebra(N):-
  Street = [House1,House2,House3],
  member(house(red,_,_),Street),
  member(house(blue,_,_),Street),
  member(house(green,_,_),Street),
  member(house(red,english,_),Street),
  member(house(_,spanish,jaguar),Street),
  sublist([house(_,_,snail),house(_,japanese,_)],Street),
  sublist([house(_,_,snail),house(blue,_,_)],Street),
  member(house(_,N,zebra),Street).
