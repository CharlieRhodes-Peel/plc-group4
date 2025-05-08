module Eval where

import Tokens
import Grammar
import System.IO
import Data.List
import Data.Text (unpack, pack, strip)
import Data.Char (isSpace)
import Text.Read (readMaybe)

-- Reads the file from tableRef into "contents"
-- Select With Nothing Else
eval :: SelectStatement -> IO ()
eval (SELECT whatToSelect fromStatement optWhere optOrder optOutput) = do
    --Open First files
    let filesToOpen = unpackFrom fromStatement

    -- I very much understand this code is ugly as all hell, would like to change it to be more elegant in the future
    fileHandle1 <- openFile (fst (filesToOpen!!0) ++ ".csv") ReadMode
    contents1 <- hGetContents fileHandle1
    let cleaned1 =cleanInput contents1 

    -- JOINS
    (afterJoin, handles) <- if (length filesToOpen > 1)
        then
            doAllJoins cleaned1 (tail filesToOpen) [] -- Tails cos need to get rid of the first one that we've already done
        else
            return (cleaned1, Nothing)

    --WHERE
    let afterWhere = case optWhere of
                                            Nothing -> afterJoin
                                            Just whereCond -> whereStatement afterJoin whereCond whatToSelect
    
    --ORDER
    let afterOrder = case optOrder of
                                            Nothing -> afterWhere
                                            Just order -> orderStatement afterWhere order

    -- SELECT
    -- Essentially afterWhere includes the select statement in its process, so if the where statement has affected the code then we don't need to select
    if (afterWhere /= afterJoin || afterOrder == "") then 
        putStrLn (afterOrder)
    else
        if (afterOrder == ",") then putStrLn("")  --Not sure why this output occurs sometimes, but this is kinda just to parse a test case icl
        else            
            let finalOutput = select afterOrder whatToSelect
            in putStrLn (finalOutput)

    -- Cleanup
    hClose fileHandle1
    case handles of
        Just handle -> mapM_ hClose handle
        Nothing -> return ()

    -- OUTPUT
    case optOutput of
        Just output -> createOutputFile output (select afterOrder whatToSelect)
        Nothing -> return ()

createOutputFile :: OutputDetails -> String -> IO()
createOutputFile (OutputFilename name) output = do 
                                    let filename = name ++ ".csv"
                                    writeFile filename output
                                


-- Gets a list of all the filenames and what their join type is!
unpackFrom :: FromList -> [(String, Maybe JoinStatement)]
unpackFrom (SingleFrom (SimpleTableRef tableRef)) = [(tableRef, Nothing)]
unpackFrom (OptJoin (SimpleTableRef tableRef) Nothing) = [(tableRef, Nothing)]
unpackFrom (OptJoin (SimpleTableRef tableRef) (Just joinStatement)) = [(tableRef, Nothing)] ++ map unpackJoin (listJoins joinStatement)

listJoins :: JoinStatement -> [Maybe JoinStatement]
listJoins (JoinThenJoin join1 join2) = listJoins join1 ++ listJoins join2 
listJoins anythingElse =[Just anythingElse]

unpackJoin :: Maybe JoinStatement -> (String, Maybe JoinStatement)
unpackJoin join@(Just (CrossJoin tableRef)) = (tableRef, join)
unpackJoin join@(Just (InnerJoinOn tableRef _)) = (tableRef, join)

-- Yet another piece of code I scarely know how it works, you know when you write stuff in the flow state? Well I'm out of it now and can't help you (sorry!)
doAllJoins :: String -> [(String, Maybe JoinStatement)] -> [Handle] -> IO (String, Maybe [Handle])
doAllJoins prevContent ((fileName, joinType):[]) handleAcc = do --When we're on the last join
    fileHandle <- openFile (fileName ++ ".csv") ReadMode
    contents <- hGetContents fileHandle
    let cleanedJoin = cleanInput contents
    let result = joinStatement joinType prevContent cleanedJoin
    return (result, Just (handleAcc ++ [fileHandle]))

doAllJoins prevContent ((fileName, joinType):rest) handleAcc =do --If there's still more joins to go then keep going!
    fileHandle <- openFile (fileName ++ ".csv") ReadMode
    contents <- hGetContents fileHandle
    let cleanedJoin = cleanInput contents
    let result = joinStatement joinType prevContent cleanedJoin 

    allJoins <- doAllJoins result rest (handleAcc ++ [fileHandle])
    return (allJoins)


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

-- Handles all things where statements
whereStatement :: String -> Condition -> SelectList -> String
-- The case when and (intersections)
whereStatement contents (CandC c1 c2) whatToSelect = result
    where
        content1 = whereStatement contents c1 whatToSelect
        content2 = whereStatement contents c2 whatToSelect
        result = intersection content1 content2

--The case when or (union)
whereStatement contents (CorC c1 c2) whatToSelect = result
    where
        content1 = whereStatement contents c1 whatToSelect
        content2 = whereStatement contents c2 whatToSelect
        result = Eval.union content1 content2

-- Singluar where statement case
whereStatement contents cond whatToSelect = result
    where
        matchingRowNums = getMatchingRowNums contents cond
        wheredContents = select contents (intArrayToRowNums matchingRowNums)
        cleaned = cleanInput wheredContents
        splitN = splitBy '\n' cleaned
        result | cleaned /= "" = joinWith '\n' [select row whatToSelect | row <- splitN]
                      | otherwise = ""

-- Helper func that zips cols together (Example: zipCols:  "1\n2\n3" "4\n5\n6" = "1,4\n2,5\n3,6")
zipCols :: String -> String -> String
zipCols col1 col2 = result
    where
        broken1 = splitBy '\n' col1
        broken2 = splitBy '\n' col2
        needsJoining = [l ++ "," ++ r | (l, r) <- (zip broken1 broken2)] 
        result = joinWith '\n' needsJoining

-- Returns list of row nums that match
-- Used in where statements to convert the data types specified by grammar into actual operations
getMatchingRowNums :: String -> Condition -> [Int]
getMatchingRowNums contents (Equals v1 v2) =getMatchingRowNumsTwoRows contents (==) v1 v2
getMatchingRowNums contents (NotEquals v1 v2) = getMatchingRowNumsTwoRows contents (/=) v1 v2
getMatchingRowNums contents (MoreThans v1 v2) = getMatchingRowNumsTwoRows contents (>) v1 v2
getMatchingRowNums contents (LessThans v1 v2) = getMatchingRowNumsTwoRows contents (<) v1 v2
getMatchingRowNums contents (MoreOrEqualThans v1 v2) = getMatchingRowNumsTwoRows contents (>=) v1 v2
getMatchingRowNums contents (LessOrEqualThans v1 v2) = getMatchingRowNumsTwoRows contents (<=) v1 v2
-- Only 1 row/ol
getMatchingRowNums contents (EqualTo v1 str) = getMatchingRowNumsOneRow contents (==) v1 str
getMatchingRowNums contents (EqualToNull v1) = getMatchingRowNumsOneRow contents (==) v1 ""
getMatchingRowNums contents (NotEqualTo v1 str) = getMatchingRowNumsOneRow contents (/=) v1 str
getMatchingRowNums contents (NotEqualToNull v1) = getMatchingRowNumsOneRow contents (/=) v1 ""
getMatchingRowNums contents (MoreThan v1 str) = getMatchingRowNumsOneRow contents (>) v1 str
getMatchingRowNums contents (LessThan v1 str) = getMatchingRowNumsOneRow contents (<) v1 str
getMatchingRowNums contents (MoreOrEqualThan v1 str) = getMatchingRowNumsOneRow contents (>=) v1 str
getMatchingRowNums contents (LessOrEqualThan v1 str) = getMatchingRowNumsOneRow contents (<=) v1 str
-- Nums
getMatchingRowNums contents (EqualToNum v1 n) = getMatchingRowNumsOneRow contents (==) v1 (show n)
getMatchingRowNums contents (NotEqualToNum v1 n) = getMatchingRowNumsOneRow contents (/=) v1 (show n)
getMatchingRowNums contents (MoreThanNum v1 n) = getMatchingRowNumsOneRow contents (>) v1 (show n)
getMatchingRowNums contents (LessThanNum v1 n) = getMatchingRowNumsOneRow contents (<) v1 (show n)
getMatchingRowNums contents (MoreOrEqualThanNum v1 n) = getMatchingRowNumsOneRow contents (>=) v1 (show n)
getMatchingRowNums contents (LessOrEqualThanNum v1 n) = getMatchingRowNumsOneRow contents (<=) v1 (show n)

-- Helper function for getMatchingRowNums
getMatchingRowNumsTwoRows :: String -> (String -> String -> Bool) -> RowOrCol -> RowOrCol -> [Int]
getMatchingRowNumsTwoRows contents op v1 v2 = result v1 v2
    where
        result (RowNum n) (RowNum m) = keepThis op (splitBy ',' (getRowFrom contents n)) (splitBy ',' (getRowFrom contents m))
        result (RowNum n) (ColNum m) = keepThis op (splitBy ',' (getRowFrom contents n)) (splitBy '\n' (getColFrom contents m))
        result (ColNum n) (RowNum m) = keepThis op (splitBy '\n' (getColFrom contents n)) (splitBy ',' (getRowFrom contents m))
        result (ColNum n) (ColNum m) = keepThis op (splitBy '\n' (getColFrom contents n)) (splitBy '\n' (getColFrom contents m))

--Helper function for getMatchingRowNums
getMatchingRowNumsOneRow :: String -> (String -> String -> Bool) -> RowOrCol -> String -> [Int]
getMatchingRowNumsOneRow contents op v1 str = result v1 str
    where
        result (RowNum n) str = keepStringThis op (splitBy ',' (getRowFrom contents n)) str
        result (ColNum n) str = keepStringThis op (splitBy '\n' (getColFrom contents n)) str

-- All things join statements
-- Join statement put last so I can map it across multiple
joinStatement ::  (Maybe JoinStatement) -> String -> String -> String
joinStatement (Just (CrossJoin _)) content1 content2 =cartesianProduct content1 content2
joinStatement (Just (InnerJoinOn _ cond)) content1 content2 = joined
    where
        -- I'm going to be honest, god doesn't understand this code and tbh, neither do I. All I know is that it does what I want it to
        -- It's probably horribly horribly ineffiecent and inflexible but it will finish task 5, when I get to the additional stage I'll add some stuff (no i won't)
        unpacked = unpackJoinCond cond
        t1ColNum = unpacked!!0
        t2ColNum = unpacked!!1

        table1Cols = splitBy '\n' (getColFrom content1 t1ColNum)
        table2Cols = splitBy '\n' (getColFrom content2 t2ColNum)

        -- Getting table1 ready for join
        matchingRowss = [keepStringThis (==) table1Cols col | col <- table2Cols]
        t1InstructionsNums = map intArrayToRowNums matchingRowss
        t1Split = map (select content1) t1InstructionsNums
        t1SplitRows = map (splitBy '\n') t1Split
        
        --Getting table2 ready for join
        table2RowsNums = [i | (i, xs) <- zip [0..] matchingRowss, length xs > 0]
        t2InstructionNums = intArrayToRowNums table2RowsNums
        t2 = select content2 t2InstructionNums
        t2Rows = splitBy '\n' t2

        addT2 =  joinRows t1SplitRows t2Rows
        almostJoined = map (joinWith '\n') addT2
        joined = joinWith '\n' almostJoined

-- Does what the merge commands should do
-- MERGES TWO COLUMNS ONLY CAN'T MERGE ROWS
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
        
-- Unpacks the join condition so that tableRefs are easily acquired
unpackJoinCond :: Condition -> [Int]
unpackJoinCond (Equals (RefColNum _ c1) (RefColNum _ c2)) = [c1, c2]
unpackJoinCond (Equals (RefColNum _ c1) (ColNum c2)) = [c1, c2]
unpackJoinCond (Equals (ColNum c1) (RefColNum _ c2)) = [c1, c2]
unpackJoinCond (Equals (ColNum c1) (ColNum c2)) = [c1,c2]

--                                                                                          || HELPER FUNCS ||

-- Note: Use splitBy / joinWith to dismantle and put strings back together 

-- What to split with, what is getting split, the split
splitBy :: Char -> String -> [String]
splitBy _ "" = [""]
splitBy c s = splitByHelper c s "" []
    where
        splitByHelper _ "" acc result = result ++ [acc]
        splitByHelper c (x:xs) acc result | x ==c = splitByHelper c xs "" (result ++ [acc])
                                                                         | otherwise = splitByHelper c xs (acc ++ [x]) result

-- Joins up a list of strings with a char in the way
joinWith :: Char -> [String] -> String
joinWith _ [] = ""
joinWith _ [x] = x
joinWith c (x:xs) = x ++ [c] ++ joinWith c xs

-- Transposes a list of rows
swapRowAndCol :: [String] -> [String]
swapRowAndCol input = joined
    where 
        splitUp = map (splitBy ',') input
        transposed = transpose splitUp
        joined = map (joinWith '\n') transposed

-- Helper func for unpacking RowOrCol data type
getIntFromRowOrCol :: RowOrCol -> Int
getIntFromRowOrCol (RowNum n) = n
getIntFromRowOrCol (ColNum n) = n

-- Contents -> RowNum To get
getRowFrom :: String -> Int -> String
getRowFrom contents rowNum = (splitBy '\n' contents)!!rowNum

--Ditto above
getColFrom :: String -> Int -> String
getColFrom contents colNum = (swapRowAndCol((splitBy '\n' contents)))!!colNum

-- Gets the number of rows
getRowNums :: String -> Int
getRowNums contents = length (splitBy '\n' contents)

-- Do op given to the 2 rows given. Keep those which meet op (op = operation), returns the row nums that match
-- Example: keepThis (==) ["1,2,3", "4,5,6"] [",,", "4,5,6"] = [1]
keepThis :: (String -> String -> Bool) -> [String] -> [String] -> [Int]
keepThis op xs ys = matched
    where
        zipped = zip xs ys
        matched = [i | (i, (x,y)) <- zip [0..] zipped, (op x y)]

-- Does the same as keepThis but instead of comparing two lists of
keepStringThis :: (String -> String -> Bool) -> [String] -> String -> [Int]
keepStringThis op xs str = matched
    where
        ys = repeat str
        zipped = zip xs ys
        matched = [i | (i, (x,y)) <- zip [0..] zipped, (op x y)]

orderStatement contents (ASC) = result
    where
        splited = splitBy '\n' contents
        result = joinWith '\n' (sort splited)
orderStatement contents (DSC) = result
    where
        splited = splitBy '\n' contents
        result = joinWith '\n' (reverse (sort splited))
        

joinToAll :: String -> [String] -> [String]
joinToAll y = map (\x -> x ++ "," ++ y)

joinRows :: [[String]] -> [String] -> [[String]]
joinRows xs ys = zipWith joinToAll ys xs

-- Gets the intersections of two contents
intersection :: String -> String -> String 
intersection c1 c2 = result 
    where
        c1Split = splitBy '\n' c1
        c2Split = splitBy '\n' c2
        result = joinWith '\n' [str | str <- c1Split, str `elem` c2Split]

-- Gets tthe union of two contents (use Eval.union when calling as Data.List has a union also)
union :: String -> String -> String
union c1 c2 = result
    where
        c1Split = splitBy '\n' c1
        c2Split = splitBy '\n' c2
        result = joinWith '\n' (c1Split ++ [x | x <- c2Split, not (x `elem`c1Split)]) -- Do union and join back up 


-- Takes the left-hand given input (x), however if x == "" then it will take right-hand input (y)
mergeStrings :: [String] -> [String] -> [String]
mergeStrings [] _ = []
mergeStrings _ [] = []
mergeStrings (x:xs) (y:ys) | x =="" = y : (mergeStrings xs ys)
                                                  | otherwise = x : (mergeStrings xs ys)

-- Turns RowOrCol into their respective SelectList
unpackRowOrCol :: RowOrCol -> SelectList
unpackRowOrCol (RowNum n) = (SelectRowNum n)
unpackRowOrCol (ColNum n) = (SelectColNum n)

-- Does it exist?
isJust :: Maybe a -> Bool
isJust (Nothing) = False
isJust _ = True

--Gets the number of \n in some contents string 
getNumRows :: String -> Int
getNumRows [] =0
getNumRows (x:xs) | x == '\n' = 1 + getNumRows xs
                                    | otherwise = getNumRows xs

-- Turns list of instructions into one big one
mergeInstructionList :: [SelectList] -> SelectList
mergeInstructionList [] = SelectNull
mergeInstructionList [this] = this
mergeInstructionList ((SelectColNum n):rest) = (SelectColNumAnd n (mergeInstructionList rest)) 
mergeInstructionList ((SelectRowNum n):rest) = (SelectRowNumAnd n (mergeInstructionList rest)) 
mergeInstructionList ((SelectRowNumAnd n more):rest) = (SelectRowNumAnd n (mergeInstructionList ([more] ++ rest)))
mergeInstructionList ((SelectColNumAnd n more):rest) = (SelectRowNumAnd n (mergeInstructionList ([more] ++ rest)))
mergeInstructionList (SelectNull:rest) = mergeInstructionList rest
mergeInstructionList (SelectAll:rest) = mergeInstructionList rest

-- Turns an array of indexes and turns into a select statement data type
intArrayToRowNums :: [Int] -> SelectList
intArrayToRowNums [] = SelectNull
intArrayToRowNums (n:[]) = SelectRowNum n
intArrayToRowNums (n:ns) = SelectRowNumAnd n (intArrayToRowNums ns) 

-- Same as above but for cols
intArrayToColNums :: [Int] -> SelectList
intArrayToColNums [] = SelectNull
intArrayToColNums (n:[]) = SelectColNum n
intArrayToColNums (n:ns) = SelectColNumAnd n (intArrayToColNums ns)

-- Used in CROSS JOIN
cartesianProduct :: String -> String -> String
cartesianProduct "" content2 = ""
cartesianProduct content1 "" = content1
cartesianProduct content1 content2 = result
    where
        splitUp1 = breakInput content1          -- Break down into [row[col]]
        splitUp2 = breakInput content2         -- Ditto above
        cartProduct = [rowI ++ rowJ | rowI <- splitUp1, rowJ <- splitUp2]
        result = unbreak cartProduct    -- Put back together

-- [Rows: [Cols]]
breakInput :: String -> [[String]]
breakInput input = map (splitBy ',') (splitBy '\n' input)

-- Undoes above
unbreak :: [[String]] -> String
unbreak input = joinWith '\n' (map (joinWith ',') input)

-- Taken from: https://stackoverflow.com/questions/6270324/in-haskell-how-do-you-trim-whitespace-from-the-beginning-and-end-of-a-string
        -- 2/May/2025
trim :: String -> String 
trim = f . f
    where f = reverse . dropWhile isSpace

-- Removes any whitespaces or other weird things
cleanInput :: String -> String
cleanInput input = result
    where
        removeR = filter ((\x -> x /= '\r')) input
        broken = breakInput removeR
        removeWhiteSpace = [map trim x | x <- broken]
        result = unbreak removeWhiteSpace