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
    $digit+     {\p s -> TokenInt p (read s)}
    $alphabet+  {\p s -> TokenVar p s}

