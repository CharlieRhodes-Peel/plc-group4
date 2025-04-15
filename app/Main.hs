import Tokens (alexScanTokens)
import System.Environment
import Control.Exception
import System.IO
import Prelude

-- Taken from the labs
main :: IO()
main = catch main' noLex

main'  = do (fileName : _) <- getArgs
            sourceText <- readFile fileName
            putStrLn ("Lexing: " ++ sourceText)
            let lexedProg = alexScanTokens sourceText
            putStrLn ("lexed as " ++ show lexedProg)

noLex :: ErrorCall -> IO ()
noLex e = do  let err =  show e
              hPutStr stderr err
              return()
