{
    module Grammar where
    import Tokens
}

--Naming convensions:
    --CamelCase like this for non-terminals
    --Use the syntax defined in %tokens for terminals
    -- Prefix 'Opt' means that this non-terminal is optional to the statement



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

    -- Operators
    '*'          { TokenWildcard _}
    '='          { TokenEquals _}
    "=="       { TokenEqualsTo _}
    "!="       { TokenNotEqualsTo _} 
    '+'          { TokenPlus _}
    '-'          { TokenMinus _}
    "<="      { TokenLessThanOrEqualTo _ }
    '<'          { TokenLessThan _}
    ">="       { TokenMoreThanOrEqualTo _ }
   '>'          { TokenMoreThan _}
    ':'          { TokenColon _}
    "||"          { TokenOr _ }
    "&&"       { TokenAnd _ }
    '!'             { TokenNot _ }
    '('          { TokenLparen _}
    ')'          { TokenRparen _}
    '{'          { TokenLCurlyparen _}
    '}'          { TokenRCurlyparen _}
    
    -- Literals
    int         { TokenInt _ $$}
    var       { TokenVar _ $$}

%nonassoc var int
%%

Plc : Statement         {$1}
       |  Plc Statement {$1 $2}

Statement : SelectStatement ';'             {$1}
                      | InsertStatement ';'              {$1}
                      | UpdateStatement ';'            {$1}
                      | DeleteStatement ';'             {$1}  
                      | CreateTableStatement ';' {$1}
                      | DropTableStatement ';'      {$1}

SelectStatement : SELECT SelectList FROM TableRef OptWhere OptGroupBy OptOrderBy {SELECT $2 $4 $5 $6 $7}

InsertStatement : INSERT INTO TableRef (COL NAMES) VALUES (VALUES) {INSERT $3 $4 $6}

UpdateStatement : UPDATE TableRef SET [COL NAMES = VALUE] ReqWhere {UPDATE $2 $4 $5}

DeleteStatement : DELETE FROM TableRef OptWhere {DELETE $3 $4}

CreateTableStatement : CREATE TABLE TableRef '(' column dataType, ...')' {CREATE $3 $5}
                     |  CREATE TABLE TableRef SelectStatement {CREATE $3 $4}


DropTableStatement : DROP TABLE TableRef {DROP $3}


SelectList : '*'                                 { SelectAll }
--                      | ExprList                    { SelectExprs $1}

TableRef : TableName OptAlias    { SimpleTableRef $1 $2}
                   | TableRef JoinType JOIN TableRef ON Condition {JoinTableRef $1 $3 $4 $6}

OptAlias : {- empty -}          { Nothing }
                  | AS var                 { Just $2 }
                  | var                       { Just $1 }

-- Assumes inner join if left empty
JoinType : INNER                    { InnerJoin }
                 | LEFT OptOuter   { LeftJoin }
                 | RIGHT OptOuter { RightJoin }
                 | FULL OptOuter   { FullJoin }
                 | {- empty -}             { InnerJoin}

OptOuter : OUTER                { () }
                    | {- empty -}       { () }

OptWhere : {- empty -}         { Nothing }
                   | WHERE Condition {Just $2}

ReqWhere :  WHERE Condition {Just $2}

OptGroupBy : {- emtpy -}                    { Nothing }
                        | GROUP BY ColumnList { Just $3 }

OptOrderBy : {- empty -}            { Nothing }
                        | ORDER BY SortList {Just $3 }



{
-- Taken from the labs 
parseError :: [Token] -> a
parseError [] = error "Uknown error"
parseError (e:rest) = error ("Parse error at (line:column) " ++ tokenPosn (e))
parseError _ = error "Parse error"


}