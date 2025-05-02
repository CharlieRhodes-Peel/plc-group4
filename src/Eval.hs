module Eval where

import Tokens
import Grammar
import System.IO
import Data.List
import Data.Text (unpack, pack, strip)

-- Reads the file from tableRef into "contents"
-- Select With Nothing Else
eval :: SelectStatement -> IO ()

eval (SELECT whatToSelect fromStatement optWhere optOrder) = do
    --Open First files
    let filesToOpen = unpackFrom fromStatement

    fileHandle1 <- openFile (fst (filesToOpen!!0) ++ ".csv") ReadMode
    contents1 <- hGetContents fileHandle1
    let cleaned1 =cleanInput contents1 

    -- JOINS
    (afterJoin, fileHandle2) <- if (length filesToOpen > 1)
        then do
            let join = (filesToOpen!!1)
            fileHandle2 <- openFile (fst join ++ ".csv") ReadMode
            contents2 <- hGetContents fileHandle2
            let cleanedJoin = cleanInput contents2
            let joinType = snd join
            let result = joinStatement joinType cleaned1 cleanedJoin
            return (result, Just fileHandle2)
        else do
            return (cleaned1, Nothing)
    
    --WHERE
    let afterWhere = case optWhere of
                                            Nothing -> afterJoin
                                            Just whereCond -> whereStatement afterJoin whereCond whatToSelect
    
    --ORDER
    let afterOrder = case optOrder of
                                            Nothing -> afterWhere
                                            Just order -> orderStatement afterWhere order

    -- 
    let finalOutput = select afterOrder whatToSelect
    putStrLn (finalOutput)

    -- Cleanup
    hClose fileHandle1
    case fileHandle2 of
        Just handle -> hClose handle
        Nothing -> return ()

-- Gets a list of all the filenames and what their join type is!
unpackFrom :: FromList -> [(String, Maybe JoinStatement)]
unpackFrom (SingleFrom (SimpleTableRef tableRef)) = [(tableRef, Nothing)]
unpackFrom (OptJoin (SimpleTableRef tableRef) (Just (CrossJoin tableRef2))) = [(tableRef, Nothing), (tableRef2, Just (CrossJoin tableRef2))]
unpackFrom (OptJoin (SimpleTableRef tableRef) (Just (InnerJoin tableRef2))) = [(tableRef, Nothing), (tableRef2, Just (InnerJoin tableRef2))]
unpackFrom (OptJoin (SimpleTableRef tableRef) (Just (OuterJoin tableRef2))) = [(tableRef, Nothing), (tableRef2, Just (OuterJoin tableRef2))]
unpackFrom (OptJoin (SimpleTableRef tableRef) Nothing) = [(tableRef, Nothing)]


-- File contents, what to select, outputs what is needed
select :: String -> SelectList -> String
select contents (SelectAll) = contents
select contents (SelectRowNum rowNum) = (getRowFrom contents rowNum)
select contents (SelectRowNumAnd rowNum next) =(getRowFrom contents rowNum) ++ ['\n'] ++ select contents next
select contents (SelectColNum colNum) =(getColFrom contents colNum)
select contents (SelectColNumAnd colNum next) = (getColFrom contents colNum) ++ [','] ++ select contents next

whereStatement :: String -> Condition -> SelectList -> String
whereStatement contents cond@(Equals v1 v2) whatToSelect = result
    where
        matchingRowNums = getMatchingRowNums contents cond
        wheredContents = select contents (intArrayToRowNums matchingRowNums)
        cleaned = cleanInput wheredContents
        splitN = splitBy '\n' cleaned
        result = joinWith '\n' [select row whatToSelect | row <- splitN]

whereStatement contents cond@(NotEquals v1 v2) whatToSelect = result
    where
        nonMatchingRowNums = getMatchingRowNums contents cond
        wheredContents = select contents (intArrayToRowNums nonMatchingRowNums)
        cleaned = cleanInput wheredContents
        splitN = splitBy '\n' cleaned
        result = joinWith '\n' [select row whatToSelect | row <- splitN]


-- Returns list of row nums that match
getMatchingRowNums :: String -> Condition -> [Int]
getMatchingRowNums contents (Equals v1 v2) =result v1 v2
    where
        result (RowNum n) (RowNum m) = keepMatching (splitBy ',' (getRowFrom contents n)) (splitBy ',' (getRowFrom contents m))
        result (RowNum n) (ColNum m) = keepMatching (splitBy ',' (getRowFrom contents n)) (splitBy ',' (getColFrom contents m))
        result (ColNum n) (RowNum m) = keepMatching (splitBy ',' (getColFrom contents n)) (splitBy ',' (getRowFrom contents m))
        result (ColNum n) (ColNum m) = keepMatching (splitBy ',' (getColFrom contents n)) (splitBy ',' (getColFrom contents m))

getMatchingRowNums contents (NotEquals v1 v2) = result v1 v2
    where
        result (RowNum n) (RowNum m) = keepNonMatching (splitBy ',' (getRowFrom contents n)) (splitBy ',' (getRowFrom contents m))
        result (RowNum n) (ColNum m) = keepNonMatching (splitBy ',' (getRowFrom contents n)) (splitBy ',' (getColFrom contents m))
        result (ColNum n) (RowNum m) = keepNonMatching (splitBy ',' (getColFrom contents n)) (splitBy ',' (getRowFrom contents m))
        result (ColNum n) (ColNum m) = keepNonMatching (splitBy ',' (getColFrom contents n)) (splitBy ',' (getColFrom contents m))


joinStatement :: (Maybe JoinStatement) -> String -> String -> String
joinStatement (Just (CrossJoin _)) content1 content2 =cartesianProduct content1 content2
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

keepNonMatching :: [String] -> [String] -> [Int]
keepNonMatching xs ys = removeMaybe
    where
        zipped = zip xs ys
        matched = [elemIndex (x,y) zipped | (x,y) <- zipped, x /= y]
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
--TODO: Add a SelectNull to Select statements to can pass around that nothing will be selected

cartesianProduct :: String -> String -> String
cartesianProduct content1 content2 = result
    where
        splitUp1 = breakInput content1          -- Break down into [row[col]]
        splitUp2 = breakInput content2         -- Ditto above
        cartProduct = [rowI ++ rowJ | rowI <- splitUp1, rowJ <- splitUp2]
        result = unbreak cartProduct    -- Put back together

breakInput :: String -> [[String]]
breakInput input = map (splitBy ',') (splitBy '\n' input)

unbreak :: [[String]] -> String
unbreak input = joinWith '\n' (map (joinWith ',') input)

-- Removes trailing and leading whitespaces from a string
trim :: String -> String 
trim = Data.Text.unpack . Data.Text.strip . Data.Text.pack

cleanInput :: String -> String
cleanInput input = result
    where
        removeR = filter ((\x -> x /= '\r')) input
        broken = breakInput removeR
        removeWhiteSpace = [map trim x | x <- broken]
        result = unbreak removeWhiteSpace