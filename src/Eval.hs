module Eval where

import Tokens
import Grammar
import System.IO
import Data.List

-- Reads the file from tableRef into "contents"
eval :: SelectStatement -> IO ()
eval (SELECT whatToSelect (SimpleTableRef tableRef) Nothing) = do
    fileHandle <- openFile (tableRef ++ ".csv") ReadMode
    contents <- hGetContents fileHandle
    let selectOutput = select contents whatToSelect
    putStrLn selectOutput
    hClose fileHandle

-- File contents, what to select, outputs what is needed
select :: String -> SelectList -> String
select contents (SelectAll) = contents
select contents (SelectRowNum rowNum) = (splitBy '\n' contents)!!rowNum
select contents (SelectRowNumAnd rowNum next) =(splitBy '\n' contents)!!rowNum ++ "\n" ++ select contents next
select contents (SelectColNum colNum) =swapRowAndCol((splitBy '\n' contents))!!colNum
select contents (SelectColNumAnd colNum next) = swapRowAndCol((splitBy '\n' contents))!!colNum ++ "\n" ++ select contents next

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