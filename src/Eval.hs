module Eval where

import Tokens
import Grammar
import System.IO
import Data.List
import Data.Text (unpack, pack, strip)
import Data.Char (isSpace)

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

    -- Essentially afterWhere includes the select statement in its process, so if the where statement has affected the code then we don't need to select
    if (afterWhere /= afterJoin) then 
        putStrLn (afterOrder)
    else
        let finalOutput = select afterOrder whatToSelect
        in putStrLn (finalOutput)

    -- Cleanup
    hClose fileHandle1
    case fileHandle2 of
        Just handle -> hClose handle
        Nothing -> return ()

-- Gets a list of all the filenames and what their join type is!
unpackFrom :: FromList -> [(String, Maybe JoinStatement)]
unpackFrom (SingleFrom (SimpleTableRef tableRef)) = [(tableRef, Nothing)]
unpackFrom (OptJoin (SimpleTableRef tableRef) (Just (CrossJoin tableRef2))) = [(tableRef, Nothing), (tableRef2, Just (CrossJoin tableRef2))]
unpackFrom (OptJoin (SimpleTableRef tableRef) Nothing) = [(tableRef, Nothing)]
unpackFrom (OptJoin (SimpleTableRef tableRef) (Just (InnerJoinOn tableRef2 cond))) = [(tableRef, Nothing), (tableRef2, Just (InnerJoinOn tableRef2 cond))]


-- File contents, what to select, outputs what is needed
select :: String -> SelectList -> String
-- All / Null
select contents (SelectAll) = contents
select contents (SelectNull) = ""
-- Row, Cols, With, Merge
select contents (SelectRowNum rowNum) = (getRowFrom contents rowNum)
select contents (SelectColNum colNum) =(getColFrom contents colNum)
select contents (SelectWith str) = joinWith '\n' [str | x <- [0..(getRowNums contents)]]
select contents (SelectMerge select1 select2) = merge contents select1 select2
-- Additionals
select contents (SelectRowNumAnd rowNum next) =(select contents (SelectRowNum rowNum)) ++ ['\n'] ++ select contents next
select contents (SelectColNumAnd colNum next) = zipCols (select contents (SelectColNum colNum)) (select contents next)
select contents (SelectWithAnd str next) = zipCols (select contents (SelectWith str)) (select contents next)
select contents (SelectMergeAnd select1 select2 next) = zipCols (select contents (SelectMerge select1 select2)) (select contents next)


whereStatement :: String -> Condition -> SelectList -> String
whereStatement contents cond whatToSelect = result
    where
        matchingRowNums = getMatchingRowNums contents cond
        wheredContents = select contents (intArrayToRowNums matchingRowNums)
        cleaned = cleanInput wheredContents
        splitN = splitBy '\n' cleaned
        result = joinWith '\n' [select row whatToSelect | row <- splitN]

zipCols :: String -> String -> String
zipCols col1 col2 = result
    where
        broken1 = splitBy '\n' col1
        broken2 = splitBy '\n' col2
        needsJoining = [l ++ "," ++ r | (l, r) <- (zip broken1 broken2)] 
        result = joinWith '\n' needsJoining


-- Returns list of row nums that match
getMatchingRowNums :: String -> Condition -> [Int]
-- == row/col with row/col
getMatchingRowNums contents (Equals v1 v2) =result v1 v2
    where
        result (RowNum n) (RowNum m) = keepThis (==) (splitBy ',' (getRowFrom contents n)) (splitBy ',' (getRowFrom contents m))
        result (RowNum n) (ColNum m) = keepThis (==) (splitBy ',' (getRowFrom contents n)) (splitBy '\n' (getColFrom contents m))
        result (ColNum n) (RowNum m) = keepThis (==) (splitBy '\n' (getColFrom contents n)) (splitBy ',' (getRowFrom contents m))
        result (ColNum n) (ColNum m) = keepThis (==) (splitBy '\n' (getColFrom contents n)) (splitBy '\n' (getColFrom contents m))
-- /= row/col with row/col
getMatchingRowNums contents (NotEquals v1 v2) = result v1 v2
    where
        result (RowNum n) (RowNum m) = keepThis (/=) (splitBy ',' (getRowFrom contents n)) (splitBy ',' (getRowFrom contents m))
        result (RowNum n) (ColNum m) = keepThis (/=) (splitBy ',' (getRowFrom contents n)) (splitBy '\n' (getColFrom contents m))
        result (ColNum n) (RowNum m) = keepThis (/=) (splitBy '\n' (getColFrom contents n)) (splitBy ',' (getRowFrom contents m))
        result (ColNum n) (ColNum m) = keepThis (/=) (splitBy '\n' (getColFrom contents n)) (splitBy '\n' (getColFrom contents m))

-- /= row/col with str
getMatchingRowNums contents (NotEqualTo v1 str) = result v1 str
    where
        result (RowNum n) str = keepStringThis (/=) (splitBy ',' (getRowFrom contents n)) str
        result (ColNum n) str = keepStringThis (/=) (splitBy '\n' (getColFrom contents n)) str

-- == row/col with str
getMatchingRowNums contents (EqualTo v1 str) = result v1 str
    where
        result (RowNum n) str = keepStringThis (==) (splitBy ',' (getRowFrom contents n)) str
        result (ColNum n) str = keepStringThis (==) (splitBy '\n' (getColFrom contents n)) str

getMatchingRowNums contents (EqualToNull v1) = result v1
    where
        result (RowNum n) = keepStringThis (==) (splitBy ',' (getRowFrom contents n)) ""
        result (ColNum n) = keepStringThis (==) (splitBy '\n' (getColFrom contents n)) ""

getMatchingRowNums contents (NotEqualToNull v1) = result v1
    where
        result (RowNum n) = keepStringThis (/=) (splitBy ',' (getRowFrom contents n)) ""
        result (ColNum n) = keepStringThis (/=) (splitBy '\n' (getColFrom contents n)) ""


joinStatement :: (Maybe JoinStatement) -> String -> String -> String
joinStatement (Just (CrossJoin _)) content1 content2 =cartesianProduct content1 content2
joinStatement (Just (InnerJoinOn _ cond)) content1 content2 = output
    where
        -- I'm going to be honest, god doesn't understand this code and tbh, neither do I. All I know is that it does what I want it to
        -- It's probably horribly horribly ineffiecent and inflexible but it will finish task 5, when I get to the additional stage I'll add some stuff (no i won't)
        unpacked = unpackJoinCond cond
        getColNum (ColNum n) = n
        table1Col = (getColFrom content1 (getColNum (snd (unpacked!!0)))) 
        table2Cols = splitBy '\n' (getColFrom content2 (getColNum (snd (unpacked!!1))))
        
        matchingRows = [getMatchingRowNums (table1Col) (EqualTo (snd (unpacked!!0)) col) | col <- table2Cols]
        instructions = map intArrayToRowNums matchingRows
        instruct = mergeInstructionList instructions
        table1Correct = select content1 instruct
        table1CorrectSplit = splitBy '\n' table1Correct
        addTable2 = joinRows table1CorrectSplit (splitBy '\n' content2)
        output = joinWith '\n' addTable2

merge :: String -> RowOrCol -> RowOrCol -> String
merge contents select1 select2 = output
    where
        instruct1 = unpackRowOrCol select1
        instruct2 = unpackRowOrCol select2

        selected1 = select contents instruct1
        selected2 = select contents instruct2
        -- This has to be a column lol
        merged = mergeStrings (splitBy '\n' selected1) (splitBy '\n' selected2)
        output = joinWith '\n' merged
        

unpackJoinCond :: Condition -> [(String, RowOrCol)]
unpackJoinCond (Equals (RefColNum tableRef colNum) (RefColNum tableRef2 colNum2)) = [(tableRef, (ColNum colNum)), (tableRef2, (ColNum colNum2))]
--                                                                                          || HELPER FUNCS ||

-- What to split with, what is getting split, the split
splitBy :: Char -> String -> [String]
splitBy _ "" = [""]
splitBy c s = splitByHelper c s "" []
    where
        splitByHelper _ "" acc result = result ++ [acc]
        splitByHelper c (x:xs) acc result | x ==c = splitByHelper c xs "" (result ++ [acc])
                                                                         | otherwise = splitByHelper c xs (acc ++ [x]) result

joinWith :: Char -> [String] -> String
joinWith _ [] = ""
joinWith _ [x] = x
joinWith c (x:xs) = x ++ [c] ++ joinWith c xs

swapRowAndCol :: [String] -> [String]
swapRowAndCol input = joined
    where 
        splitUp = map (splitBy ',') input
        transposed = transpose splitUp
        joined = map (joinWith '\n') transposed

getIntFromRowOrCol :: RowOrCol -> Int
getIntFromRowOrCol (RowNum n) = n
getIntFromRowOrCol (ColNum n) = n

-- Contents -> RowNum To get
getRowFrom :: String -> Int -> String
getRowFrom contents rowNum = (splitBy '\n' contents)!!rowNum

--Ditto above
getColFrom :: String -> Int -> String
getColFrom contents colNum = (swapRowAndCol((splitBy '\n' contents)))!!colNum

getRowNums :: String -> Int
getRowNums contents = length (splitBy '\n' contents)

keepThis op xs ys = matched
    where
        zipped = zip xs ys
        matched = [i | (i, (x,y)) <- zip [0..] zipped, (op x y)]

keepStringThis op xs str = removeMaybe
    where
        matched = [elemIndex x xs | x <- xs, (op x str)]
        removeMaybe = [x | (Just x) <- (filter isJust matched) ]

orderStatement contents (ASC) = result
    where
        splited = splitBy '\n' contents
        result = joinWith '\n' (sort splited)
orderStatement contents (DSC) = result
    where
        splited = splitBy '\n' contents
        result = joinWith '\n' (reverse (sort splited))
        
joinRows :: [String] -> [String] -> [String]
joinRows [] _ = []
joinRows _ [] = []
joinRows (x:xs) (y:ys) = (x ++ "," ++ y) : joinRows xs ys

mergeStrings :: [String] -> [String] -> [String]
mergeStrings [] _ = []
mergeStrings _ [] = []
mergeStrings (x:xs) (y:ys) | x =="" = y : (mergeStrings xs ys)
                                                  | otherwise = x : (mergeStrings xs ys)

unpackRowOrCol :: RowOrCol -> SelectList
unpackRowOrCol (RowNum n) = (SelectRowNum n)
unpackRowOrCol (ColNum n) = (SelectColNum n)

isJust :: Maybe a -> Bool
isJust (Nothing) = False
isJust _ = True

--Gets the number of \n in some contents string 
getNumRows :: String -> Int
getNumRows [] =0
getNumRows (x:xs) | x == '\n' = 1 + getNumRows xs
                                    | otherwise = getNumRows xs

mergeInstructionList :: [SelectList] -> SelectList
mergeInstructionList [] = SelectNull
mergeInstructionList ((SelectColNum n):[]) = (SelectColNum n)
mergeInstructionList ((SelectColNum n):rest) = (SelectColNumAnd n (mergeInstructionList rest)) 
mergeInstructionList ((SelectRowNum n):[]) = (SelectRowNum n)
mergeInstructionList ((SelectRowNum n):rest) = (SelectRowNumAnd n (mergeInstructionList rest)) 

intArrayToRowNums :: [Int] -> SelectList
intArrayToRowNums [] = SelectNull
intArrayToRowNums (n:[]) = SelectRowNum n
intArrayToRowNums (n:ns) = SelectRowNumAnd n (intArrayToRowNums ns) 

intArrayToColNums :: [Int] -> SelectList
intArrayToColNums [] = SelectNull
intArrayToColNums (n:[]) = SelectColNum n
intArrayToColNums (n:ns) = SelectColNumAnd n (intArrayToColNums ns)

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

-- Taken from: https://stackoverflow.com/questions/6270324/in-haskell-how-do-you-trim-whitespace-from-the-beginning-and-end-of-a-string
        -- 2/May/2025
trim :: String -> String 
trim = f . f
    where f = reverse . dropWhile isSpace

cleanInput :: String -> String
cleanInput input = result
    where
        removeR = filter ((\x -> x /= '\r')) input
        broken = breakInput removeR
        removeWhiteSpace = [map trim x | x <- broken]
        result = unbreak removeWhiteSpace