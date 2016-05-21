import Parser
import Data.List

{- 
CS352 Spring 2016 
-}

{-
The purpose of this project is to implement a tautology verifyer in the Łukasiewicz logic.
This project is composed of 3 largely independent parts and a 4th part that brings 
it all together:
1)    Implementation of the operators of the logic in Haksell
2)    Implementation of a parser for expressions in the logic returning expression trees
3)    Implementation of an evaluator for the expression trees
4)    Implementation of the verifyer

The rest of this file contains the skeleton of the solution. Most data definitions and some of the functions
are already coded for you. You will have to code all the functions marked TODO. At the end of each section
you'll find a few functions whose name start with the word test. These functions will test your implementation. 
You should execute them, they take no parameter and return True if the test passes.
Add your implementation at the places indicated by the word TODO. You can also add helper functions if 
yout need to, but DO NOT make any other changes to the file. In particular do not elminate any of the 
comments and most importantly DO NOT make any change the code of the test functions. 
Points will be taken away for tampering with those functions.

Don't forget to define the type for all the functions you code!!! Points will be taken out for missing type declarations.

When you are done, submit your file via email. The name of your file must be your last name-first name and the .hs extension,
e.g. Doe-Jane.hs. Points will be taken out for misnamed files. 
-}

{-
Łukasiewicz logics can range over real numbers in the interval [0..1]. We'll limit ourselves to the 3-valued logic and we'll
use the constructors C, U and I for 1, .5 and 0. Intuitively C stands for absolute Certainty (a probability of 1); 
I stands for absolute Impossibility (a probability of 0); and U stands for uncertainty (a probability of .5, half way between 0 and 1).

There are 4 unary operations: Negation(~), Possible(?), Sure(!), Unknown(%) 
Since Haskell does not allow custom symbols as prefixes we'll use the following function names
negation for ~
possible for ? (It is possible that ...)
sure for ! (It is certain that ...)
unknown for % (It is unknown if ...)

whith the following Truth Table:
 x     ~ x   ? x   ! x   % x
 C      I     C     C     I
 U      U     C     I     C
 I      C     I     I     I

and 4 binary operations: Disjunction, Conjuction, Implication and equivalence.

<|> for disjuction
<&> for conjunction
--> for implication
<-> for equivalence
The Truth Table for the binary operators is:
 x    y    x <|> y     x <&> Y     x --> Y
 C    I       C           I           I
 C    C       C           C           C
 C    U       C           U           M
 I    I       I           I           C
 I    C       C           I           C
 I    U       U           I           C
 U    I       U           I           M
 U    C       C           U           C
 U    U       U           U           C
 
The equivalence operator (<->) defined by biconditional exchange, that is x <-> y
 is equivalent to x --> y and y --> x
-}

-- P A R T 1   Ł U K A S I E W I C Z   L O G I C

-- We use Łukasiewicz for the datatype, with the constructor C I and U 
data Lukasiewicz = C | I | U
    deriving (Eq,  Show, Ord)

--TODO: Implement the negation function
negation :: Lukasiewicz -> Lukasiewicz
negation x | x == C = I
           | x == U = U
           | x == I = C

--TODO: Implement the possible function
possible :: Lukasiewicz -> Lukasiewicz
possible x | x == C = C
           | x == U = C
           | x == I = I

--TODO: Implement the sure function
sure :: Lukasiewicz -> Lukasiewicz
sure x | x == C = C
       | x == U = I
       | x == I = I

--TODO: Implement the unknown function
unknown :: Lukasiewicz -> Lukasiewicz
unknown x | x == C = I
          | x == U = C
          | x == I = I


-- The implication operator has the lowest precedence infix
infix 1 --> 
--TODO Implement the infix operator --> (implication)
(-->) :: Lukasiewicz -> Lukasiewicz -> Lukasiewicz
(-->) C I = I
(-->) C C = C
(-->) C U = U
(-->) I I = C
(-->) I C = C
(-->) I U = C
(-->) U I = U
(-->) U C = C
(-->) U U = C


-- The Or operator is just above equivalence in precedence
infix 2 <|> 
--TODO Implement the infix operator <|> (Or)
(<|>) :: Lukasiewicz -> Lukasiewicz -> Lukasiewicz
(<|>) C I = C
(<|>) C C = C
(<|>) C U = C
(<|>) I I = I
(<|>) I C = C
(<|>) I U = U 
(<|>) U I = U
(<|>) U C = C
(<|>) U U = U

-- The And operator is just above Or in precedence
infix 3 <&> 
--TODO Implement the infix operator <&> (And)
(<&>) :: Lukasiewicz -> Lukasiewicz -> Lukasiewicz
(<&>) C I = I
(<&>) C C = C
(<&>) C U = U
(<&>) I I = I
(<&>) I C = I
(<&>) I U = I
(<&>) U I = I
(<&>) U C = U
(<&>) U U = U

-- The equivalence operator has the lowest precedence infix:
infix 1 <-> 
--TODO Implement the infix operator <-> (equivalence)
(<->) :: Lukasiewicz -> Lukasiewicz -> Lukasiewicz
(<->) C I = I
(<->) C C = C
(<->) C U = U
(<->) I I = C
(<->) I C = I
(<->) I U = U
(<->) U I = U
(<->) U C = U
(<->) U U = C

-- This completes part 1. You can use the following functions to test your implementation
testNeg :: Bool 
testNeg = (negation C == I) && (negation U == U) && (negation I == C)
                      
testPos :: Bool       
testPos = (possible C == C) && (possible U == C) && (possible I == I)
                      
testCer :: Bool       
testCer = (sure C == C) && (sure U == I) && (sure I == I)
                      
testUnk :: Bool       
testUnk = (unknown C == I) && (unknown U == C) && (unknown I == I)

testEquiv :: Bool
testEquiv = ((C <-> I) == I)    
         && ((C <-> C) == C)        
         && ((C <-> U) == U)    
         && ((I <-> I) == C)    
         && ((I <-> C) == I)    
         && ((I <-> U) == U)    
         && ((U <-> I) == U)    
         && ((U <-> C) == U)    
         && ((U <-> U) == C)    

testImply :: Bool
testImply = ((C --> I) == I)    
         && ((C --> C) == C)        
         && ((C --> U) == U)    
         && ((I --> I) == C)    
         && ((I --> C) == C)    
         && ((I --> U) == C)    
         && ((U --> I) == U)    
         && ((U --> C) == C)    
         && ((U --> U) == C)    

testOr :: Bool
testOr = ((C <|> I) == C)    
      && ((C <|> C) == C)        
      && ((C <|> U) == C)    
      && ((I <|> I) == I)    
      && ((I <|> C) == C)    
      && ((I <|> U) == U)    
      && ((U <|> I) == U)    
      && ((U <|> C) == C)    
      && ((U <|> U) == U)    

testAnd :: Bool
testAnd = ((C <&> I) == I)    
        && ((C <&> C) == C)        
        && ((C <&> U) == U)    
        && ((I <&> I) == I)    
        && ((I <&> C) == I)    
        && ((I <&> U) == I)    
        && ((U <&> I) == I)    
        && ((U <&> C) == U)    
        && ((U <&> U) == U)    

testPart1 = testNeg && testPos && testCer && testUnk && testEquiv && testImply && testOr && testAnd


{- P A R T 2 : Implementation of a parser for Łukasiewicz expressions
--TODO Define the type LExpTree, using the constructors L, V, N, Q, S, K, A, O, E, I

L for Lukasiewicz literals (i.e. C, I or U)
V for Variables (string) 
N for a node representing the prefix neg
Q for a node representing the prefix pos
S for a node representing the prefix cer
K for a node representing the prefix unk
A for a node representing the infix And
O for a node representing the infix Or
E for a node representing the infix Equivalence
M for a node representing the infix Implication
It will be convenient to have the new type derive from the classes Show and Eq
-}
data LExpTree = L Lukasiewicz
              | V String
              | N LExpTree
              | Q LExpTree
              | S LExpTree
              | K LExpTree
              | A LExpTree LExpTree
              | E LExpTree LExpTree
              | M LExpTree LExpTree
   deriving (Show, Eq)


{-Grammar:

We use the following grammar for the Łukasiewicz expressions

lukExp :: lukOpd ( '<->' lukExp | '-->' lukExp| e )
lukOpd :: lukTerm ( '<|>' lukOpd | e )
lukTerm :: lukFact ( '<&>' lukTerm | e)
lukFact :: '~' lukPrim | '?' lukPrim | '!' lukPrim | '%' lukPrim | lukPrim
lukPrim :: lukVar | lukLit | '('lukExp')'
lukVar :: lowercase (Alphanumeric)*
lukLit :: C | I | U

This is similar to the grammar we've seen in class. Note the symbols ~ ? ! and % is used for prefixes. 
As in Haskell, identifiers will start with a lowercase letter, followed by any number of letters 
and digits. You do not have to allow the single quote characters in identifiers.
-}

{- TODO: Implement a Parser tExp for the grammar above that generates an expression tree.
The type "Parser a" is defined in the file Parser.hs. You can (and probably should) 
make use of the functions in that file. They are the same as those we have seen in class.
Hint: you will need to define a function for each of the rules of the grammar.
-}
lukLit :: Parser LExpTree
lukLit = P (\s -> case parse ((symbol "C") +++ (symbol "I") +++ (symbol "U")) s of
                     Nothing -> Nothing
                     Just ("C", t) -> Just (L C, t)
                     Just ("I", t) -> Just (L I, t)
                     Just ("U", t) -> Just (L U, t))  

lukVar = P (\s -> case parse (token (string s)) s of
                       Nothing -> Nothing
                       Just(v, t) -> Just( V s, t))


-- TODO: Implement a function parseT that takes a string as input 
-- and returns a Łukasiewicz logic expression tree (LExpTree)
--parseT :: String -> LExpTree

-- This completes part 2. You can use the following functions to test your implementation

testLit :: Bool
testLit = lt == (L C) && lf == (L I) && lm == (L U)  
           where Just (lt, _) = parse lukLit " C " 
                 Just (lf, _) = parse lukLit " I "
                 Just (lm, _) = parse lukLit " U "  

testVar :: Bool
testVar = id == (V "id") where Just (id, _) = parse lukVar " id "
{-
testPrim :: Bool
testPrim = lt == (L C) && lf == (L I) && lm == (L U) && pv == (V "id2")  && pe == (L C)
            where Just (lt, "") = parse lukPrim " C " 
                  Just (lf, "") = parse lukPrim " I "
                  Just (lm, "") = parse lukPrim " U "  
                  Just (pv, "") = parse lukPrim " id2 "
                  Just (pe, "") = parse lukPrim " ( C ) "

testFact :: Bool
testFact = lt == (L C) && lf == (L I) && lm == (L U) && pv == (V "id2")  && pe == (L C)
          && fnt == N (L C) && fnf == N (L I) && fnm == N(L U) && fnv == N(V "id2")  && fnet == N(L C)
          && fqt == Q (L C) && fqf == Q (L I) && fqm == Q(L U) && fqv == Q(V "id2")  && fqet == Q(L C)
          && fst == S (L C) && fsf == S (L I) && fsm == S(L U) && fsv == S(V "id2")  && fset == S(L C)
          && fkt == K (L C) && fkf == K (L I) && fkm == K(L U) && fkv == K(V "id2")  && fket == K(L C)
            where Just (lt, "") = parse lukFact " C " 
                  Just (lf, "") = parse lukFact " I "
                  Just (lm, "") = parse lukFact " U "  
                  Just (pv, "") = parse lukFact " id2 "
                  Just (pe, "") = parse lukFact " ( C ) "
                  Just (fnt, "") = parse lukFact "~ C " 
                  Just (fnf, "") = parse lukFact "~ I "
                  Just (fnm, "") = parse lukFact "~ U "  
                  Just (fnv, "") = parse lukFact "~ id2 "
                  Just (fnet, "") = parse lukFact " ~( C ) "
                  Just (fqt, "") = parse lukFact "? C " 
                  Just (fqf, "") = parse lukFact "? I "
                  Just (fqm, "") = parse lukFact "? U "  
                  Just (fqv, "") = parse lukFact "? id2 "
                  Just (fqet, "") = parse lukFact " ?( C ) "
                  Just (fst, "") = parse lukFact "! C " 
                  Just (fsf, "") = parse lukFact "! I "
                  Just (fsm, "") = parse lukFact "! U "  
                  Just (fsv, "") = parse lukFact "! id2 "
                  Just (fset, "") = parse lukFact " !( C ) "
                  Just (fkt, "") = parse lukFact "% C " 
                  Just (fkf, "") = parse lukFact "% I "
                  Just (fkm, "") = parse lukFact "% U "  
                  Just (fkv, "") = parse lukFact "% id2 "
                  Just (fket, "") = parse lukFact " %( C ) "

testTerm :: Bool
testTerm = lt == (L C) && lf == (L I) && lm == (L U) && pv == (V "id2")  && pe == (L C)
          && fnt == N (L C) && fnf == N (L I) && fnm == N(L U) && fnv == N(V "id2")  && fnet == N(L C)
          && alv == A (L C, V "var")
            where Just (lt, "") = parse lukTerm " C " 
                  Just (lf, "") = parse lukTerm " I "
                  Just (lm, "") = parse lukTerm " U "  
                  Just (pv, "") = parse lukTerm " id2 "
                  Just (pe, "") = parse lukTerm " ( C ) "
                  Just (fnt, "") = parse lukTerm "~ C " 
                  Just (fnf, "") = parse lukTerm "~ I "
                  Just (fnm, "") = parse lukTerm "~ U "  
                  Just (fnv, "") = parse lukTerm "~ id2 "
                  Just (fnet, "") = parse lukTerm " ~( C ) "
                  Just (alv, "") = parse lukTerm "C <&> var"

testOpd :: Bool
testOpd = lt == (L C) && lf == (L I) && lm == (L U) && pv == (V "id2")  && pe == (L C)
          && fnt == N (L C) && fnf == N (L I) && fnm == N(L U) && fnv == N(V "id2")  && fnet == N(L C)
          && alv == A (L C, V "var") && ovv == O (V "var", V "var2")
            where Just (lt, "") = parse lukExp " C " 
                  Just (lf, "") = parse lukExp " I "
                  Just (lm, "") = parse lukExp " U "  
                  Just (pv, "") = parse lukExp " id2 "
                  Just (pe, "") = parse lukExp " ( C ) "
                  Just (fnt, "") = parse lukExp "~ C " 
                  Just (fnf, "") = parse lukExp "~ I "
                  Just (fnm, "") = parse lukExp "~ U "  
                  Just (fnv, "") = parse lukExp "~ id2 "
                  Just (fnet, "") = parse lukExp " ~( C ) "
                  Just (alv, "") = parse lukExp "C <&> var"
                  Just (ovv, "") = parse lukExp "var <|> var2"

testExp :: Bool
testExp = lt == (L C) && lf == (L I) && lm == (L U) && pv == (V "id2")  && pe == (L C)
          && fnt == N (L C) && fnf == N (L I) && fnm == N(L U) && fnv == N(V "id2")  && fnet == N(L C)
          && alv == A (L C, V "var") && ovv == O (V "var", V "var2")
          && ce == O (V "v",O (N (V "v"),E (V "v",M (L U,L C))))
            where Just (lt, "") = parse lukExp " C " 
                  Just (lf, "") = parse lukExp " I "
                  Just (lm, "") = parse lukExp " U "  
                  Just (pv, "") = parse lukExp " id2 "
                  Just (pe, "") = parse lukExp " ( C ) "
                  Just (fnt, "") = parse lukExp "~ C " 
                  Just (fnf, "") = parse lukExp "~ I "
                  Just (fnm, "") = parse lukExp "~ U "  
                  Just (fnv, "") = parse lukExp "~ id2 "
                  Just (fnet, "") = parse lukExp " ~( C ) "
                  Just (alv, "") = parse lukExp "C <&> var"
                  Just (ovv, "") = parse lukExp "var <|> var2"
                  Just (vel, "") = parse lukExp "v<->U"
                  Just (liv, "") = parse lukExp "C --> q"
                  Just (ce, "") = parse lukExp "v <|> ~v <|> (v<->U-->C)"

testPart2 :: Bool
testPart2 = testLit && testVar && testPrim && testFact && testTerm && testOpd && testExp
-}--REMOVE THIS!!!---
{-
-- P A R T 3: Create an evaluator for LExpTree's 

{- Since our expressions will include free variables we need to use a dictionary to 
lookup the value assigned to those variables. The dictionary consists of a list of 
pairs (String, Value) and is defined as follows:
-}
type Dict = [(String, Lukasiewicz)] 

{-TODO Create a function lk that takes a dictionary and a string and returns
  the value associated with the string in the dictionary.
  Note: You do not have to worry about the efficiency of the search, the number
  of variables will be limited.  
-}
--WHAT IS BASE CASE????
lk :: Dict -> String -> Lukasiewicz
lk ((a,b):xs) str | (fst (a,b)) == str = snd (a,b)
                  | otherwise = lk xs str


{- TODO create a function evalT that takes a dictionary, and an expression tree
and returns the value of the expresion given the value assigned to the variables
in the dictionary.
-}
evalT :: Dict -> LExpTree -> Lukasiewicz
evalT parseT LExpree


-- This completes part 3. You can use the following functions to test your implementation

testDict :: Dict
testDict = [("vC", C), ("vI", I), ("vU", U)]

testLk :: Bool
testLk =  ((lk testDict "vC") == C)
       && ((lk testDict "vI") == I)
       && ((lk testDict "vU") == U)
{----REMOVE THIS!!!---
testEvalT :: Bool
testEvalT = evalT testDict (O (V "vC",O (N (V "vC"),E (V "vC",L U)))) == C
          && evalT testDict (O (V "vI",O (N (V "vI"),E (V "vI",L U)))) == C
          && evalT testDict (O (V "vU",O (N (V "vU"),E (V "vU",L U)))) == C
          && evalT testDict (O (V "vU",N (V "vU"))) == U
          && evalT testDict (A (L I, V "vU")) == I 
          && evalT testDict (O (V "vC",O (N (V "vI"),E (V "vU",M (L U,L C))))) == C 

testPart3 :: Bool
testPart3 = testLk && testEvalT

-- P A R T 4: Tautology Prover
{- The first thing we need to do is get a list of all the free variables
that occur in the expression.
TODO: Create a function varList that takes as input a Łukasiewicz logic expression tree (LExpTree)
and retuns a list of all the variable names (strings) contained in the tree.
-}
 
{- Next we need to generate a dictionary for all the possible combinations of values
that can be assigned to the variables.
TODO: create a function dictList that takes a list of variable names (strings) and returns 
a list of all the possible dictionaries associating values in the Łukasiewicz logic to each 
of the variables.
Hint: This is a recursive process. For a single variable, there will be 3 dictionaries, 
one for each of the 3 literal values in Łukasiewicz logic. For 2 variables there will be
9 dictionaries, associating all 3 possible values of the first variable to the 3 possible
values of the second variable, and so forth.
-}


{- 
Now we can evaluate a Łukasiewicz logic expression against all possible 
combinations of values of its variables.
TODO: create a function allCases that takes an expression tree (LExpTree) as input and
returns in a list of the results (Łukasiewicz logic literals) of evaluating the expression tree 
to all possible combination of values assigned to the variables (dictionaries)
-}

{- Finally for the tautology checker.
TODO: create a function isTautology that takes a string, parses it
and return True if the expression contained in the string is a tautology. I.e. if the 
evaluation of the expression returns C regardless of the value assigned to the variables.
-}


{- This completes part 4 and the project. You can use the test functions below 
to test your work
-}
testVarList :: Bool
testVarList = varList (O (V "vC",O (N (V "vU"),E (V "vI",L U)))) == ["vC", "vU", "vI"]
            && varList (O (L C,O (N (L I),E (L U,L U)))) == []

sortedDictList :: Ord a => [[a]] -> [[a]]
sortedDictList xss = sort [sort d | d <- xss]

testDictList :: Bool
testDictList = dictList [] == [[]] 
                && sortedDictList (dictList ["vC", "vU", "vI"])
              == sortedDictList 
                 [[("vC",C),("vU",C),("vI",C)],[("vC",I),("vU",C),("vI",C)],[("vC",U),("vU",C),("vI",C)]
                 ,[("vC",C),("vU",I),("vI",C)],[("vC",I),("vU",I),("vI",C)],[("vC",U),("vU",I),("vI",C)]
                 ,[("vC",C),("vU",U),("vI",C)],[("vC",I),("vU",U),("vI",C)],[("vC",U),("vU",U),("vI",C)]
                 ,[("vC",C),("vU",C),("vI",I)],[("vC",I),("vU",C),("vI",I)],[("vC",U),("vU",C),("vI",I)]
                 ,[("vC",C),("vU",I),("vI",I)],[("vC",I),("vU",I),("vI",I)],[("vC",U),("vU",I),("vI",I)]
                 ,[("vC",C),("vU",U),("vI",I)],[("vC",I),("vU",U),("vI",I)],[("vC",U),("vU",U),("vI",I)]
                 ,[("vC",C),("vU",C),("vI",U)],[("vC",I),("vU",C),("vI",U)],[("vC",U),("vU",C),("vI",U)]
                 ,[("vC",C),("vU",I),("vI",U)],[("vC",I),("vU",I),("vI",U)],[("vC",U),("vU",I),("vI",U)]
                 ,[("vC",C),("vU",U),("vI",U)],[("vC",I),("vU",U),("vI",U)],[("vC",U),("vU",U),("vI",U)]]


testTautology :: Bool
testTautology = isTautology "v <|> ~v <|> (v <-> U)"
              && not (isTautology "v <|> ~v ")
              && isTautology "(v <&> t <-> t <&> v) <&> (w <|> x <-> x <|> w) <&> (z <-> z)"
              && isTautology "~(x <|> y) <-> (~x <&> ~y)"
              && isTautology "~(x <&> y) <-> (~x <|> ~y)"
              && isTautology "v <|> ~v <|> (v<->U-->I)"

testPart4 :: Bool
testPart4 = testVarList && testDictList && testTautology

testAll :: Bool
testAll = testPart1 && testPart2 && testPart3 && testPart4
-}--REMOVE THIS!!!---
-}
