{
module Grammar where
import Tokens
}

--Naming convensions:
    --CamelCase like this for non-terminals
    --Use the syntax defined in %tokens for terminals
    -- Prefix 'Opt' means that this non-terminal is optional to the statement

%name parseCalc
%tokentype { Token }
%error { parseError }
%token
    -- SQL ish commands
    SELECT      { TokenSELECT _}
    FROM        { TokenFROM _}
    JOIN        { TokenJOIN _}
    WHERE       { TokenWHERE _}
    INTO        { TokenINTO _}
    ON          { TokenON _}
    CROSS       { TokenCROSS _}
    LEFT        { TokenLEFT _}
    RIGHT       { TokenRIGHT _}
    INNER       { TokenINNER _}
    OUTER       { TokenOUTER _}
    UNION       { TokenUNION _}
    INTERSECTION{ TokenINTERSECTION _}
    IS          { TokenIS _}
    IN          { TokenIN _}
    AS          { TokenAS _}
    NULL        { TokenNULL _}
    ORDER       { TokenORDER _}
    BY          { TokenBY _}
    ASC         { TokenASC _}
    DSC         { TokenDSC _}
    DROP        { TokenDROP _}
    UPDATE      { TokenUPDATE _}
    IF          { TokenIF _}
    ELSE        { TokenELSE _}
    COUNT       { TokenCOUNT _}
    SUM         { TokenSUM _}
    AVG         { TokenAVG _}
    MIN         { TokenMIN _}
    MAX         { TokenMAX _}
    ROW         { TokenROW _}
    COL         { TokenCOL _}

    -- Symbols
    '*'          { TokenWildcard _}
    "=="          { TokenEqualsTo _}
    "!="        { TokenNotEqualsTo _}
    ','          { TokenComma _}
    ';'          { TokenSemiColon _}
    '('         {  TokenLParen _}
    ')'         {  TokenRParen _}

    -- Literals
    int       { TokenInt _ $$}
    var       { TokenVar _ $$}

%nonassoc var int
%left SELECT FROM
%%

Statement : SelectStatement ';'             {$1}

-- SELECT STUFF
SelectStatement : SELECT SelectList FROM FromList OptWhere OptOrderBy {SELECT $2 $4 $5 $6}

SelectList : '*'                                    { SelectAll }
                      | var ',' SelectList       { SelectRowAnd $1 $3}
                      | var                                 { SelectRow $1}
                      | ROW '(' int ')' ',' SelectList           { SelectRowNumAnd $3 $6}
                      | ROW '(' int ')'                         { SelectRowNum $3 }
                      | COL '(' int ')' ',' SelectList { SelectColNumAnd $3 $6}
                      | COL '(' int ')'                           { SelectColNum $3 }

RowOrCol : ROW '(' int ')'                            { RowNum $3 }
                   | COL '(' int ')'                           { ColNum $3 }

FromList : TableRef OptJoin             {OptJoin $1 $2}
                  | TableRef                             {SingleFrom $1}

TableRef : var                                     { SimpleTableRef $1 }

OptJoin :: {Maybe JoinStatement}
OptJoin : {- empty -}                   {Nothing}
                | JoinStatement         {Just $1}

JoinStatement : CROSS JOIN var      {CrossJoin $3}
                              | INNER JOIN var       {InnerJoin $3}
                              | OUTER JOIN var       {OuterJoin $3}

OptWhere :: { Maybe Condition }
OptWhere : {- empty -}                {Nothing} 
                   | WHERE Condition      {Just $2}

OptOrderBy :: { Maybe Order}
OptOrderBy : {- empty -}            {Nothing}
                        | ORDER BY Order  {Just $3}

-- Done in where statements
Condition : RowOrCol "==" RowOrCol                    {Equals $1 $3}
                   | RowOrCol "!=" RowOrCol                      {NotEquals $1 $3}
                   | RowOrCol "!=" var                                 {NotEqualTo $1 $3}

Order : ASC                                 {ASC}
             | DSC                                 {DSC}

{
-- Taken from the labs 
parseError :: [Token] -> a
parseError [] = error "Unknown error"
parseError (e:rest) = error ("Parse error at (line:column) " ++ tokenPosn (e))
parseError _ = error "Parse error"

--SELECT STUFF
data SelectStatement  = SELECT SelectList FromList (Maybe Condition) (Maybe Order)
    deriving (Show)

data SelectList = SelectAll 
                | SelectRow String | SelectRowAnd String SelectList 
                | SelectRowNum Int | SelectRowNumAnd Int SelectList 
                | SelectColNum Int | SelectColNumAnd Int SelectList
    deriving (Show)

data FromList = SingleFrom TableRef | OptJoin TableRef (Maybe JoinStatement)
    deriving (Show)

data JoinStatement = CrossJoin String | InnerJoin String | OuterJoin String
    deriving (Show)

data RowOrCol = RowNum Int | ColNum Int
    deriving (Show)

data TableRef = SimpleTableRef String | TableRefAnd String TableRef
    deriving (Show)

data WhereStatement = WHERE Condition
    deriving (Show)

data Order = ASC | DSC
    deriving (Show)

data Condition = Equals RowOrCol RowOrCol
                                | NotEquals RowOrCol RowOrCol
                                | NotEqualTo RowOrCol String
    deriving (Show)

}