module Eval where

import Tokens
import Grammar
import System.IO
import Data.List

-- Reads the file from tableRef into "contents"
-- Select With Nothing Else
eval :: SelectStatement -> IO ()
eval (SELECT whatToSelect (SimpleTableRef tableRef) Nothing Nothing) = do
    fileHandle <- openFile (tableRef ++ ".csv") ReadMode
    contents <- hGetContents fileHandle
    let contentsCleaned = filter ((\x -> x /= 'r')) contents

    let selectOutput = select contentsCleaned whatToSelect

    putStrLn selectOutput
    hClose fileHandle

-- Select With Where statement
eval (SELECT whatToSelect (SimpleTableRef tableRef) (Just cond) Nothing) = do
    fileHandle <- openFile (tableRef ++ ".csv") ReadMode
    contents <- hGetContents fileHandle
    let contentsCleaned = filter ((\x -> x /= 'r')) contents

    let whereOutput = whereStatement contentsCleaned cond
    let wheredContents = select contents (intArrayToRowNums whereOutput)
    let removeWeird = filter ((\x -> x /= '\r')) wheredContents
    let splitN = splitBy '\n' removeWeird
    let selectContent = joinWith '\n' [select row whatToSelect | row <- splitN]

    putStrLn (show selectContent)
    hClose fileHandle

eval (SELECT whatToSelect (SimpleTableRef tableRef) Nothing (Just order)) = do
    fileHandle <- openFile (tableRef ++ ".csv") ReadMode
    contents <- hGetContents fileHandle
    let contentsCleaned = filter ((\x -> x /= '\r')) contents

    let selectContent = select contentsCleaned whatToSelect
    let orderOutput = orderStatement selectContent order

    putStrLn (show orderOutput)
    hClose fileHandle

eval (SELECT whatToSelect (SimpleTableRef tableRef) (Just cond) (Just order)) = do
    fileHandle <- openFile (tableRef ++ ".csv") ReadMode
    contents <- hGetContents fileHandle
    let contentsCleaned = filter ((\x -> x /= 'r')) contents

    let whereOutput = whereStatement contentsCleaned cond
    let wheredContents = select contents (intArrayToRowNums whereOutput)
    let removeWeird = filter ((\x -> x /= '\r')) wheredContents
    let splitN = splitBy '\n' removeWeird
    let selectContent = joinWith '\n' [select row whatToSelect | row <- splitN]

    let orderOutput = orderStatement selectContent order
    putStrLn (show orderOutput)
    hClose fileHandle

-- File contents, what to select, outputs what is needed
select :: String -> SelectList -> String
select contents (SelectAll) = contents
select contents (SelectRowNum rowNum) = (getRowFrom contents rowNum)
select contents (SelectRowNumAnd rowNum next) =(getRowFrom contents rowNum) ++ ['\n'] ++ select contents next
select contents (SelectColNum colNum) =(getColFrom contents colNum)
select contents (SelectColNumAnd colNum next) = (getColFrom contents colNum) ++ [','] ++ select contents next

-- Returns list of row nums that match
whereStatement :: String -> Condition -> [Int]
whereStatement contents (Equals v1 v2) =result v1 v2
    where
        result (RowNum n) (RowNum m) = keepMatching (splitBy ',' (getRowFrom contents n)) (splitBy ',' (getRowFrom contents m))
        result (RowNum n) (ColNum m) = keepMatching (splitBy ',' (getRowFrom contents n)) (splitBy ',' (getColFrom contents m))
        result (ColNum n) (RowNum m) = keepMatching (splitBy ',' (getColFrom contents n)) (splitBy ',' (getRowFrom contents m))
        result (ColNum n) (ColNum m) = keepMatching (splitBy ',' (getColFrom contents n)) (splitBy ',' (getColFrom contents m))

--                                                                                          || HELPER FUNCS ||

-- What to split with, what is getting split, the split
splitBy :: Char -> String -> [String]
splitBy splitChar [] = []
splitBy splitChar input = 
    prefix : case suffix of
        [] -> []
        (_:rest) -> splitBy splitChar rest
    where
        (prefix, suffix) = break (== splitChar) input

swapRowAndCol :: [String] -> [String]
swapRowAndCol input = joined
    where 
        splitUp = map (splitBy ',') input
        transposed = transpose splitUp
        joined = map (joinWith ',') transposed


joinWith :: Char -> [String] -> String
joinWith c = foldr (\x acc -> x ++ if null acc then "" else [c] ++ acc) ""

getIntFromRowOrCol :: RowOrCol -> Int
getIntFromRowOrCol (RowNum n) = n
getIntFromRowOrCol (ColNum n) = n

-- Contents -> RowNum To get
getRowFrom :: String -> Int -> String
getRowFrom contents rowNum = (splitBy '\n' contents)!!rowNum

--Ditto above
getColFrom :: String -> Int -> String
getColFrom contents colNum = swapRowAndCol((splitBy '\n' contents))!!colNum

keepMatching :: [String] -> [String] -> [Int]
keepMatching xs ys = removeMaybe
    where
        zipped = zip xs ys
        matched = [elemIndex (x,y) zipped | (x,y) <- zipped, x == y]
        removeMaybe = [x | (Just x) <- (filter isJust matched)]

orderStatement contents (ASC) = result
    where
        splited = splitBy '\n' contents
        result = joinWith '\n' (sort splited)
orderStatement contents (DSC) = result
    where
        splited = splitBy '\n' contents
        result = joinWith '\n' (reverse (sort splited))
        


isJust :: Maybe a -> Bool
isJust (Nothing) = False
isJust _ = True

--Gets the number of \n in some contents string 
getNumRows :: String -> Int
getNumRows [] =0
getNumRows (x:xs) | x == '\n' = 1 + getNumRows xs
                                    | otherwise = getNumRows xs


intArrayToRowNums :: [Int] -> SelectList
intArrayToRowNums (n:[]) = SelectRowNum n
intArrayToRowNums (n:ns) = SelectRowNumAnd n (intArrayToRowNums ns) 