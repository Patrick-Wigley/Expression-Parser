-- DATA-STRUCTURE 
data ParseTree a = 
                    Num String
                    | Mul (ParseTree a) (ParseTree a)
                    | Div (ParseTree a) (ParseTree a)
                    | Add (ParseTree a) (ParseTree a)
                    | Sub (ParseTree a) (ParseTree a)
                    | Pow (ParseTree a) (ParseTree a)
                    | PrenBreak String (ParseTree a) (ParseTree a)
                    
                    | ERROR
                 
    deriving (Show, Eq)
-- END OF DATA-STRUCTURE


-- TOOLS
-- String -> Index -> Counter -> RETURNS (Left side, Right side)
splitStrAtIndex :: String -> Int -> Int -> String -> (String, String)

splitStrAtIndex "" indexSplitAt counter accumalator = ("", accumalator) -- Reached end of str
splitStrAtIndex (c:cs) indexSplitAt counter accumalator
                                    | counter == indexSplitAt = (cs, accumalator)
                                    | otherwise = splitStrAtIndex cs indexSplitAt (counter+1) (accumalator ++ [c])

getCharAtIndex :: String -> Int -> Int -> String
getCharAtIndex "" _ _ = ""
getCharAtIndex (c:cs) indexLookingFor counter 
                                | counter /= indexLookingFor = getCharAtIndex cs indexLookingFor (counter+1) 
                                | otherwise = [c]

chopCharsOfStr :: String -> Int -> Int -> String
chopCharsOfStr "" _ _ = "" -- EndOfString
chopCharsOfStr (c:cs) chopAmount counter 
                                    | counter /= chopAmount-1 = chopCharsOfStr cs chopAmount (counter+1) -- Continue Chopping
                                    | otherwise = cs    -- Finished chopping - return result
                                    
-- END OF TOOLS



-- NEGATIVE SYMBOL CONVERTER 
-- Used to to seperate values for subtraction operator & negatating operator
-- (CONVERTS NEGATE '-' TO '~')
changeNegativeDenoter :: String -> String -> String
changeNegativeDenoter (c:nc:cs) ""  
                                    | (c == '-')    = '~' : changeNegativeDenoter ((nc:cs)) ([c])                                       -- -1-2
                                    | otherwise     = c : changeNegativeDenoter (nc:cs) ([c])
changeNegativeDenoter (c:nc:cs) prevChars
                                | (nc == '-' && (charIsOperator c)) = c : '~' : changeNegativeDenoter (cs) (prevChars ++ [c] ++ [nc])   -- 1--2
                                | otherwise =                         c : changeNegativeDenoter (nc:cs) (prevChars ++ [c])
changeNegativeDenoter (c:cs) _ = [c]

-- For converting it back 
convertNegateValBack :: String -> String
convertNegateValBack (c:cs) 
                            | c == '~'  = '-' : (convertNegateValBack cs)
                            | otherwise = c : (convertNegateValBack cs)
convertNegateValBack s = s 
-- END OF NEGATIVE SYMBOL CONVERTER


-- MAIN PROGRAM 
-- DESCRIPTION LOGIC:
-- Firstly converts negated symbols from '-' to '~' for sepration 
-- LOOP BELOW:
-- Go through expression and find lowest precedence, once found split into two sub-expressions (branches) - do same for LEFT & RIGHT sub-expressions RECURSIVELY
-- IF Number, return number
-- 2*4+3 -> ADD (MUL 2 4) 3 
-- END LOOP


-- take lowest prec operators and left and right content of it as sub-branches
-- will be O(n^2) but f it 

-- search for every operator (going lowest prec to highest)
findOperator :: Char -> String -> Int -> Int
-- Operator -> InStr -> counter -> RETURNS its index | -1 (not found)
findOperator op "" counter = -1 --
findOperator op (c:cs) counter
                        | c == op = counter
                        | otherwise = findOperator op cs (counter+1)

charIsOperator :: Char -> Bool
charIsOperator c 
                | c == '^' || c == '*' || c == '/' || c == '-' || c == '+'  = True
                | otherwise                                                 = False


-- When parenBreak occurs, this ensures there is correct constructor usage 
-- Acts as an extra layer to 'createParseTree' for parenthesis' processing
getParseTreeOpConstructor :: String -> String -> String -> ParseTree String
getParseTreeOpConstructor op lt rt 
                            | op == "^" = Pow (createParseTree lt) (createParseTree rt)
                            | op == "*" = Mul (createParseTree lt) (createParseTree rt)
                            | op == "/" = Div (createParseTree lt) (createParseTree rt)
                            | op == "-" = Sub (createParseTree lt) (createParseTree rt)
                            | op == "+" = Add (createParseTree lt) (createParseTree rt)
                            | otherwise = ERROR

-- This parsing trick effectively splits branches by operator looking lowest precedence to highest
createParseTree :: String -> ParseTree String
createParseTree s -- bool for "is searching for corresponding open bracket"? 
                        | s == "" = ERROR
                                                    -- "1+(9-5)*3-9"

                                                             -- Unlike the closenPren, dont need to search for the corresponding closed paran as that would've been found before this
                        | closedPrenOpLocation /= -1 = if (fst splitBranches) /= "" then (getParseTreeOpConstructor operatorAfterClosedPren (snd splitBranchesAtClosedPren) (chopCharsOfStr (fst splitBranches) 1 0))       -- Then split at operator to right of paren
                                                       else createParseTree (snd splitBranches)                                    -- else need to try and split on left side of parens
                        | openPrenOpLocation /= -1 = if (operatorBeforeOpenedPren) /= "" then (getParseTreeOpConstructor operatorBeforeOpenedPren (snd splitBranchesAtOpenPren) (fst splitBranchesAtOpenPren))         -- Then split at operator to left of paren
                                                     else createParseTree (fst splitBranches)    
                        
                        | addOpLocation /= -1 = Add (createParseTree (snd splitBranches)) (createParseTree (fst splitBranches))         -- Found addition in expression
                        | subOpLocation /= -1 = Sub (createParseTree (snd splitBranches)) (createParseTree (fst splitBranches))                                                                                                                                -- Found subtract in expression
                        | mulOpLocation /= -1 = Mul (createParseTree (snd splitBranches)) (createParseTree (fst splitBranches))                                                                                                                                -- Found multiply in expression
                        | divOpLocation /= -1 = Div (createParseTree (snd splitBranches)) (createParseTree (fst splitBranches))                                                                                                                                -- Found divide in expression
                        | powOpLocation /= -1 = Pow (createParseTree (snd splitBranches)) (createParseTree (fst splitBranches))                                                                                                                                -- Found power in expression
                        | otherwise           = Num (convertNegateValBack s)      -- Is Number
  

                        where 
                            closedPrenOpLocation = (findOperator ')' s 0) -- Could search this one in reverse to keep precedence of parenthesis'?                                                                 
                            openPrenOpLocation = (findOperator '(' s 0)   -- Then here do if inbalance of ( to ) then dont search for above search here instead 

                            operatorAfterClosedPren = (getCharAtIndex s (closedPrenOpLocation+1) 0)
                            operatorBeforeOpenedPren = (getCharAtIndex s (openPrenOpLocation-1) 0)

                            splitBranchesAtClosedPren = (splitStrAtIndex s (closedPrenOpLocation) 0 "") 
                            splitBranchesAtOpenPren = (splitStrAtIndex s (openPrenOpLocation-1) 0 "") 


                            addOpLocation = (findOperator '+' s 0)
                            subOpLocation = (findOperator '-' s 0)
                            mulOpLocation = (findOperator '*' s 0)
                            divOpLocation = (findOperator '/' s 0)
                            powOpLocation = (findOperator '^' s 0)


                            -- Clear this all up after
                            splitBranches = if (closedPrenOpLocation /= -1) then (splitStrAtIndex s (closedPrenOpLocation) 0 "") 
                                            else if (openPrenOpLocation /= -1) then (splitStrAtIndex s (openPrenOpLocation) 0 "") 

                                            else if (addOpLocation /= -1) then (splitStrAtIndex s addOpLocation 0 "")
                                            else if (subOpLocation /= -1) then (splitStrAtIndex s subOpLocation 0 "")
                                            else if (mulOpLocation /= -1) then (splitStrAtIndex s mulOpLocation 0 "")
                                            else if (divOpLocation /= -1) then (splitStrAtIndex s divOpLocation 0 "")
                                            else if (powOpLocation /= -1) then (splitStrAtIndex s powOpLocation 0 "")

                                            else ("VOID", "VOID")


-- GETS PARSED TREE
parse :: String -> ParseTree String
parse s = createParseTree (changeNegativeDenoter s "")

-- Expressions TO SAMPLE 
largeExpression = "1^9-9+7/3*2+1^8"
mediumExpression = "1^6-3*2"
smallExpression = "2+4*3"

negatedExpression = "1^6-3*-2"
decimalExpression = "1.2*9-3^2.9"


-- Note for evaluator - If finish implementing parenthesis', make sure to change the PrenBreak usage to its correct operator
parenthesisExpression1 = "2*(5+2)"
sameExpressionNoBrackets = "2*5+2"

parenthesisExpression2 = "(5+2)*1"

brokenParenthesisExpression = "1+(9-5)*3-9"

-- Program USAGE EXAMPLE
usageExample = evaluateParseTree (createParseTree "9+1*2")

-- EVALUTATOR 
-- MAKE SAFE
strToDouble :: String -> Double
strToDouble s = read s :: Double

evaluateParseTree:: ParseTree String -> Double
evaluateParseTree (Add lt rt) = (evaluateParseTree lt) + (evaluateParseTree rt)
evaluateParseTree (Sub lt rt) = (evaluateParseTree lt) - (evaluateParseTree rt)
evaluateParseTree (Div lt rt) = (evaluateParseTree lt) / (evaluateParseTree rt)
evaluateParseTree (Mul lt rt) = (evaluateParseTree lt) * (evaluateParseTree rt)
evaluateParseTree (Pow lt rt) = (evaluateParseTree lt) ** (evaluateParseTree rt)
evaluateParseTree (Num n)     = strToDouble (n)
                    