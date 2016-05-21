import Parser

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

data Lukasiewicz = C | I | U
    deriving (Eq,  Show, Ord)


--lukLit :: C | I | U
--lukLit :: Maybe (String, String) 
lukLit = P (\s -> case parse ((symbol "C") +++ (symbol "I") +++ (symbol "U")) s of
                     Nothing -> Nothing
                     Just ("C", t) -> Just (L C, t)
                     Just ("I", t) -> Just (L I, t)
                     Just ("U", t) -> Just (L U, t))                              
            

testLit :: Bool
testLit = lt == (L C) && lf == (L I) && lm == (L U)  
           where Just (lt, _) = parse lukLit " C " 
                 Just (lf, _) = parse lukLit " I "
                 Just (lm, _) = parse lukLit " U "


                 


