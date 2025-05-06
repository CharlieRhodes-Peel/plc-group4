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
    MERGE       { TokenMERGE _}
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
    WITH         { TokenWITH _}

    -- Symbols
    '*'          { TokenWildcard _}
    "=="          { TokenEqualsTo _}
    "!="        { TokenNotEqualsTo _}
    '>'             { TokenMoreThan _}
    ">="          { TokenMoreThanOrEqualTo _}
    '<'             { TokenLessThan _}
    "<="        { TokenLessThanOrEqualTo _}
    ','          { TokenComma _}
    ';'          { TokenSemiColon _}
    '('         {  TokenLParen _}
    ')'         {  TokenRParen _}
    '"'         { TokenSpeechMark _}
    '.'           { TokenFullStop _}

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

                      | ROW '(' int ')' ',' SelectList               { SelectRowNumAnd $3 $6}
                      | ROW '(' int ')'                                         { SelectRowNum $3 }
                      | COL '(' int ')' ',' SelectList               { SelectColNumAnd $3 $6}
                      | COL '(' int ')'                                         { SelectColNum $3 }

                      | var '.' ROW '(' int ')' ',' SelectList  { SelectRefRowNumAnd $1 $5 $8}
                      | var '.' ROW '(' int ')'                             { SelectRefRowNum $1 $5}
                      | var '.' COL '(' int ')' ',' SelectList  { SelectRefColNumAnd $1 $5 $8}
                      | var '.' COL '(' int ')'                             { SelectRefColNum $1 $5}

                      | WITH '(' '"' var '"' ')'                           {SelectWith $4}
                      | WITH '(' '"' var '"' ')' ',' SelectList {SelectWithAnd $4 $8}

                      | MERGE '(' RowOrCol ',' RowOrCol ')'                            {SelectMerge $3 $5}
                      | MERGE '(' RowOrCol ',' RowOrCol ')' ',' SelectList {SelectMergeAnd $3 $5 $8} 

RowOrCol : ROW '(' int ')'                      {RowNum $3}
                   | COL '(' int ')'                      {ColNum $3}
                   | var '.' ROW '(' int ')'          { RefRowNum $1 $5 }
                   | var '.'  COL '(' int ')'         { RefColNum $1 $5 }


FromList : TableRef OptJoin             {OptJoin $1 $2}
                  | TableRef                             {SingleFrom $1}

TableRef : var                                     { SimpleTableRef $1 }

OptJoin :: {Maybe JoinStatement}
OptJoin : {- empty -}                   {Nothing}
                | JoinStatement         {Just $1}

JoinStatement : CROSS JOIN var      {CrossJoin $3}
                              | INNER JOIN var ON Condition       {InnerJoinOn $3 $5}
                              | OUTER JOIN var ON Condition       {OuterJoinOn $3 $5}

OptWhere :: { Maybe Condition }
OptWhere : {- empty -}                {Nothing} 
                   | WHERE Condition      {Just $2}

OptOrderBy :: { Maybe Order}
OptOrderBy : {- empty -}            {Nothing}
                        | ORDER BY Order  {Just $3}

-- Done in where statements
Condition : RowOrCol "==" RowOrCol                    {Equals $1 $3}
                   | RowOrCol "==" var                                 {EqualTo $1 $3}
                   | RowOrCol "==" NULL                                 {EqualToNull $1}
                   | RowOrCol "!=" RowOrCol                      {NotEquals $1 $3}
                   | RowOrCol "!=" var                                 {NotEqualTo $1 $3}
                   | RowOrCol "!=" NULL                                 {NotEqualToNull $1}
                   | RowOrCol '>' RowOrCol                          {MoreThans $1 $3}
                   | RowOrCol '>' var                                       {MoreThan $1 $3}
                   | RowOrCol '<' RowOrCol                              {LessThans $1 $3}
                   | RowOrCol '<' var                                         {LessThan $1 $3}
                   | RowOrCol ">=" RowOrCol                         {MoreOrEqualThans $1 $3}
                   | RowOrCol ">=" var                                      {MoreOrEqualThan $1 $3}
                   | RowOrCol "<=" RowOrCol                           {LessOrEqualThans $1 $3}
                   | RowOrCol "<=" var                                      {LessOrEqualThan $1 $3}

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

data SelectList = SelectAll | SelectNull
                | SelectRow String | SelectRowAnd String SelectList 
                | SelectRowNum Int | SelectRowNumAnd Int SelectList 
                | SelectColNum Int | SelectColNumAnd Int SelectList

                | SelectRefRowNum String Int | SelectRefRowNumAnd String Int SelectList
                | SelectRefColNum String Int | SelectRefColNumAnd String Int SelectList

                | SelectWith String | SelectWithAnd String SelectList
                | SelectMerge RowOrCol RowOrCol | SelectMergeAnd RowOrCol RowOrCol SelectList
    deriving (Show)

data FromList = SingleFrom TableRef | OptJoin TableRef (Maybe JoinStatement)
    deriving (Show)

data JoinStatement = CrossJoin String | InnerJoinOn String Condition | OuterJoinOn String Condition
    deriving (Show)

data RowOrCol = RowNum Int | ColNum Int
                              |   RefRowNum String Int | RefColNum String Int
    deriving (Show)

data TableRef = SimpleTableRef String | TableRefAnd String TableRef
    deriving (Show)

data WhereStatement = WHERE Condition
    deriving (Show)

data Order = ASC | DSC
    deriving (Show)

data Condition = Equals RowOrCol RowOrCol
                                | EqualTo RowOrCol String
                                | EqualToNull RowOrCol
                                | NotEquals RowOrCol RowOrCol
                                | NotEqualTo RowOrCol String
                                | NotEqualToNull RowOrCol
                                | MoreThans RowOrCol RowOrCol
                                | MoreThan RowOrCol String
                                | LessThans RowOrCol RowOrCol
                                | LessThan RowOrCol String
                                | MoreOrEqualThans RowOrCol RowOrCol
                                | MoreOrEqualThan RowOrCol String
                                | LessOrEqualThans RowOrCol RowOrCol
                                | LessOrEqualThan RowOrCol String

    deriving (Show)

}