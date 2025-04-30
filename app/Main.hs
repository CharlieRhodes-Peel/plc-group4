import Tokens (alexScanTokens)
import Grammar (parseCalc)
import Eval (eval)
import System.Environment
import Control.Exception
import System.IO
import Prelude

-- Taken from the labs
main :: IO()
main = catch main' noLex

main'  = do (fileName : _) <- getArgs
            --Lexing and Parsing
            sourceText <- readFile fileName
            putStrLn ("Lexing: " ++ sourceText)
            let lexedProg = alexScanTokens sourceText
            putStrLn ("lexed as " ++ show lexedProg)
            let parsedSyntax = parseCalc lexedProg
            putStrLn ("parsed as: " ++ show parsedSyntax)

            --Eval
            let evaled = eval parsedSyntax []
            putStrLn ("Evaled to: " ++ show evaled)

noLex :: ErrorCall -> IO ()
noLex e = do  let err =  show e
              hPutStr stderr err
              return()
