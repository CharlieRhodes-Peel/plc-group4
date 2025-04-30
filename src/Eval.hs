module Eval where

import Tokens
import Grammar
import System.IO

-- Reads the file from tableRef into "contents"
eval :: SelectStatement -> IO ()
eval (SELECT whatToSelect (SimpleTableRef tableRef)) = do
    fileHandle <- openFile (tableRef ++ ".csv") ReadMode
    contents <- hGetContents fileHandle
    let selectOutput = select contents whatToSelect
    putStrLn selectOutput
    hClose fileHandle

-- File contents, what to select, outputs what is needed
select :: String -> SelectList -> String
select contents (SelectAll) = contents
select contents (SelectRowNum rowNum) = (splitBy '\n' contents)!!rowNum







-- What to split with, what is getting split, the split
splitBy :: Char -> String -> [String]
splitBy splitChar [] = []
splitBy splitChar input = 
    prefix : case suffix of
        [] -> []
        (_:rest) -> splitBy splitChar rest
    where
        (prefix, suffix) = break (== splitChar) input

