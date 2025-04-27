{
module Grammar where
import Tokens
}

--Naming convensions:
    --CamelCase like this for non-terminals
    --Use the syntax defined in %tokens for terminals
    -- Prefix 'Opt' means that this non-terminal is optional to the statement

-- Only testing that binary operations work properly
%name parseCalc


%tokentype { Token }
%error { parseError }
%token
    -- Operators
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
    '('          { TokenLParen _}
    ')'          { TokenRParen _}
    '{'          { TokenLCurlyParen _}
    '}'          { TokenRCurlyParen _}
    
    -- Literals
    int         { TokenInt _ $$}
    var       { TokenVar _ $$}

    --True & false
    "true"  { TokenTrue _}
    "false" { TokenFalse _}

%nonassoc var int "true" "false"
%left "||"
%left "&&"
%nonassoc "==" "!=" "<=" ">=" '<' '>'
%left '+' '-'
%nonassoc '!' '(' ')'
%left LEFT
%%

Start : Expr { ExprResult $1 }
             | Condition { ConditionResult $1 }

Expr : Expr '+' Expr {Plus $1 $3}
          | Expr '-'   Expr  {Minus $1 $3}
          | '-' Expr               {Negate $2}
          | '(' Expr ')'           {$2}
          | var                     {Var $1}
          | int                      {Int $1}

-- Conditions
Condition : Expr CompareOps Expr   {Compare $1 $2 $3}
                  | Condition "||" Condition       {Or $1 $3 }
                  | Condition "&&" Condition    {And $1 $3}
                  | '!' Condition                            {Not $2}
                  | '(' Condition ')'                       { $2 }
                  | "true"                                      {TrueCond}
                  | "false"                                     {FalseCond}

CompareOps : "=="            { EqualsTo }
                         | "!="              { NotEqualsTo }
                         | "<="              { LessThanOrEqualTo }
                         | '<'                  { LessThan }
                         | ">="              { MoreThanOrEqualTo }
                         | '>'                  { MoreThan }

--OptNot : '!'                            { Prelude.True }
--               | {- empty -}            { Prelude.False }

--Variable : var  {Var $1}
--Integer : int {Int $1}

{
-- Taken from the labs 
parseError :: [Token] -> a
parseError [] = error "Uknown error"
parseError (e:rest) = error ("Parse error at (line:column) " ++ tokenPosn (e))
parseError _ = error "Parse error"

data CalcResult = ExprResult Expr | ConditionResult Condition
                deriving Show

data Expr = Plus Expr Expr
                      |  Minus Expr Expr
                      |  Negate Expr
                      | Var String
                      | Int Int
    deriving Show 

data Condition = Compare Expr CompareOps Expr
                              | And Condition Condition
                              | Or Condition Condition
                              | Not Condition
                              | TrueCond
                              | FalseCond
    deriving Show 

data CompareOps = EqualsTo | NotEqualsTo | LessThanOrEqualTo | LessThan | MoreThanOrEqualTo | MoreThan
    deriving Show 

}