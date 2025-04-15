{
    module Tokens where
}

%wrapper "posn"

$digit = 0-9
$alphabet = [a-zA-Z]

tokens :-
$white+         ;   -- White spaces
    "--".*      ;   -- Comments
    SELECT      {\p s -> TokenSELECT p}
    FROM        {\p s -> TokenFROM p}
    JOIN        {\p s -> TokenJOIN p}
    WHERE       {\p s -> TokenWHERE p}
    INTO        {\p s -> TokenINTO p}
    ON          {\p s -> TokenON p}
    CROSS       {\p s -> TokenCROSS p}
    LEFT        {\p s -> TokenLEFT p}
    RIGHT       {\p s -> TokenRIGHT p}
    INNER       {\p s -> TokenINNER p}
    OUTER       {\p s -> TokenOUTER p}
    UNION       {\p s -> TokenUNION p}
    INTERSECTION{\p s -> TokenINTERSECTION p}
    IS          {\p s -> TokenIS p}
    IN          {\p s -> TokenIN p}
    AS          {\p s -> TokenAS p}
    NULL        {\p s -> TokenNULL p}
    ORDER       {\p s -> TokenORDER p}
    BY          {\p s -> TokenBY p}
    DROP        {\p s -> TokenDROP p}
    UPDATE      {\p s -> TokenUPDATE p}
    IF          {\p s -> TokenIF p}
    ELSE        {\p s -> TokenELSE p}
    COUNT       {\p s -> TokenCOUNT p}
    SUM         {\p s -> TokenSUM p}
    AVG         {\p s -> TokenAVG p}
    MIN         {\p s -> TokenMIN p}
    MAX         {\p s -> TokenMAX p}
    ROW         {\p s -> TokenROW p}
    COL         {\p s -> TokenCOL p}
    \*          {\p s -> TokenWildcard p}
    \=          {\p s -> TokenEquals p}
    "=="        {\p s -> TokenEqualsTo p}
    \+          {\p s -> TokenPlus p}
    \-          {\p s -> TokenMinus p}
    \<          {\p s -> TokenLessThan p}
    \>          {\p s -> TokenMoreThan p}
    \:          {\p s -> TokenColon p}
    \(          {\p s -> TokenLParen p}
    \)          {\p s -> TokenRParen p}
    \{          {\p s -> TokenLCurlyParen p}
    \}          {\p s -> TokenRCurlyParen p}
    $digit+     {\p s -> TokenInt p (read s)}
    $alphabet+  {\p s -> TokenVar p s}

{
    -- :: AlexPosn -> String -> Token

data Token = 
    TokenSELECT AlexPosn |
    TokenFROM AlexPosn |
    TokenJOIN AlexPosn |
    TokenWHERE AlexPosn |
    TokenINTO AlexPosn |
    TokenON AlexPosn |
    TokenCROSS AlexPosn |
    TokenLEFT AlexPosn |
    TokenRIGHT AlexPosn |
    TokenINNER AlexPosn |
    TokenOUTER AlexPosn |
    TokenUNION AlexPosn |
    TokenINTERSECTION AlexPosn |
    TokenIS AlexPosn |
    TokenIN AlexPosn |
    TokenAS AlexPosn |
    TokenNULL AlexPosn |
    TokenORDER AlexPosn |
    TokenBY AlexPosn |
    TokenDROP AlexPosn |
    TokenUPDATE AlexPosn |
    TokenIF AlexPosn |
    TokenELSE AlexPosn |
    TokenCOUNT AlexPosn |
    TokenSUM AlexPosn |
    TokenAVG AlexPosn |
    TokenMIN AlexPosn |
    TokenMAX AlexPosn |
    TokenROW AlexPosn |
    TokenCOL AlexPosn |
    TokenWildcard AlexPosn |
    TokenEquals AlexPosn |
    TokenEqualsTo AlexPosn |
    TokenPlus AlexPosn |
    TokenMinus AlexPosn |
    TokenLessThan AlexPosn |
    TokenMoreThan AlexPosn |
    TokenColon AlexPosn |
    TokenInt AlexPosn Int |
    TokenVar AlexPosn String |
    TokenLCurlyParen AlexPosn |
    TokenRCurlyParen AlexPosn |
    TokenLParen AlexPosn |
    TokenRParen AlexPosn
    deriving (Eq, Show)

tokenPosn :: Token -> String
tokenPosn (TokenInt (AlexPn a l c) n) = show l ++ ":" ++ show c
tokenPosn (TokenVar (AlexPn a l c) x) = show l ++ ":" ++ show c
tokenPosn (TokenSELECT (AlexPn a l c)) = show l ++ ":" ++ show c
tokenPosn (TokenFROM (AlexPn a l c)) = show l ++ ":" ++ show c
tokenPosn (TokenJOIN (AlexPn a l c)) = show l ++ ":" ++ show c
tokenPosn (TokenWHERE (AlexPn a l c)) = show l ++ ":" ++ show c
tokenPosn (TokenINTO (AlexPn a l c)) = show l ++ ":" ++ show c
tokenPosn (TokenON (AlexPn a l c)) = show l ++ ":" ++ show c
tokenPosn (TokenCROSS (AlexPn a l c)) = show l ++ ":" ++ show c
tokenPosn (TokenLEFT (AlexPn a l c)) = show l ++ ":" ++ show c
tokenPosn (TokenRIGHT (AlexPn a l c)) = show l ++ ":" ++ show c
tokenPosn (TokenINNER (AlexPn a l c)) = show l ++ ":" ++ show c
tokenPosn (TokenOUTER (AlexPn a l c)) = show l ++ ":" ++ show c
tokenPosn (TokenUNION (AlexPn a l c)) = show l ++ ":" ++ show c
tokenPosn (TokenINTERSECTION (AlexPn a l c)) = show l ++ ":" ++ show c
tokenPosn (TokenIS (AlexPn a l c)) = show l ++ ":" ++ show c
tokenPosn (TokenIN (AlexPn a l c)) = show l ++ ":" ++ show c
tokenPosn (TokenAS (AlexPn a l c)) = show l ++ ":" ++ show c
tokenPosn (TokenNULL (AlexPn a l c)) = show l ++ ":" ++ show c
tokenPosn (TokenORDER (AlexPn a l c)) = show l ++ ":" ++ show c
tokenPosn (TokenBY (AlexPn a l c)) = show l ++ ":" ++ show c
tokenPosn (TokenDROP (AlexPn a l c)) = show l ++ ":" ++ show c
tokenPosn (TokenUPDATE (AlexPn a l c)) = show l ++ ":" ++ show c
tokenPosn (TokenIF (AlexPn a l c)) = show l ++ ":" ++ show c
tokenPosn (TokenELSE (AlexPn a l c)) = show l ++ ":" ++ show c
tokenPosn (TokenCOUNT (AlexPn a l c)) = show l ++ ":" ++ show c
tokenPosn (TokenSUM (AlexPn a l c)) = show l ++ ":" ++ show c
tokenPosn (TokenAVG (AlexPn a l c)) = show l ++ ":" ++ show c
tokenPosn (TokenMIN (AlexPn a l c)) = show l ++ ":" ++ show c
tokenPosn (TokenMAX (AlexPn a l c)) = show l ++ ":" ++ show c
tokenPosn (TokenROW (AlexPn a l c)) = show l ++ ":" ++ show c
tokenPosn (TokenCOL (AlexPn a l c)) = show l ++ ":" ++ show c
tokenPosn (TokenWildcard (AlexPn a l c)) = show l ++ ":" ++ show c
tokenPosn (TokenEquals (AlexPn a l c)) = show l ++ ":" ++ show c
tokenPosn (TokenEqualsTo (AlexPn a l c)) = show l ++ ":" ++ show c
tokenPosn (TokenPlus (AlexPn a l c)) = show l ++ ":" ++ show c
tokenPosn (TokenMinus (AlexPn a l c)) = show l ++ ":" ++ show c
tokenPosn (TokenLessThan (AlexPn a l c)) = show l ++ ":" ++ show c
tokenPosn (TokenMoreThan (AlexPn a l c)) = show l ++ ":" ++ show c
tokenPosn (TokenColon (AlexPn a l c)) = show l ++ ":" ++ show c
tokenPosn (TokenLParen (AlexPn a l c)) = show l ++ ":" ++ show c
tokenPosn (TokenRParen (AlexPn a l c)) = show l ++ ":" ++ show c
tokenPosn (TokenLCurlyParen (AlexPn a l c)) = show l ++ ":" ++ show c
tokenPosn (TokenRCurlyParen (AlexPn a l c)) = show l ++ ":" ++ show c
}