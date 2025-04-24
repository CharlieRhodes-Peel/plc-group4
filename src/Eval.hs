module Eval where

import Data.List
import Grammar --TODO: Match lexed and parsed names to that here in the eval (Errors: PlcBool, PlcNum, Eq, NotEqualsTo)
-- Alot is adapted from lab05

-- Values expressions can eval to
data Value = NumVal Int | BoolVal Bool
    deriving (Show, Eq)

-- Variable binding environment
type Environment = [ (String, Value) ]

data Frame = HolePlus Expr Environment --Left hole needs to be further evaled
                         |  PlusHole  Value                        -- Right hole has to be a value
                         | HoleMinus Expr Environment
                         | MinusHole Value
                         | HoleAnd Expr Environment
                         | AndHole Value
                         | HoleOr Expr Environment
                         | OrHole Value
                         | NotHole 
                         | HoleLessThan Expr Environment
                         | LessThanHole Value
                         | HoleMoreThan Expr Environment
                         | MoreThanHole Value
                         | HoleLessThanOrEqualTo Expr Environment
                         | LessThanOrEqualToHole Value
                         | HoleMoreThanOrEqualTo Expr Environment
                         | MoreThanOrEqualToHole Value
                         | HoleNotEqualsTo Expr Environment
                         | NotEqualsToHole Value
    deriving (Show, Eq)

type Kontinuation = [ Frame ]
type Configuration = (Expr, Environment, Kontinuation)

-- Ensures a value is a num, else throws an error
verifyNumVal :: Value -> Int
verifyNumVal (NumVal n) = n
--verifyNumVal val  = error "Expected a numeric value but got: " ++ show (val)

-- Ensures a value is a bool, else throws an error
verifyBoolVal :: Value -> Bool
verifyBoolVal (BoolVal b) = b
--verifyBoolVal val = error "Expected a boolean but got: " ++ show (val)

isValue :: Expr -> Bool
isValue (Var _) = True
isValue (Int _)  = True
isValue (TrueCond) = True
isValue (FalseCond) = True
isValue _ = False

exprToValue :: Expr -> Value
exprToValue (Var n)  =NumVal n
exprToValue (Int n) = NumVal n
exprToValue (TrueCond) = (BoolVal True)
exprToValue (FalseCond) = (BoolVal False)
--exprToValue exp = error "Expression: " ++ show (exp) ++ "is not a value"

valueToExpr :: Value -> Environment -> Kontinuation -> Configuration
valueToExpr (NumVal n) env k = (Int n, env, k)
valueToExpr (BoolVal True) env k = (TrueCond, env, k)
valueToExpr (BoolVal False) env k = (FalseCond, env, k)

-- Single step evaluation
eval1 :: Configuration -> Configuration
eval1 ((Var x), env, k) = 
    case lookup x env of
        Just val ->
            case val of
                NumVal n -> (PlcNum n, env, k)
                (BoolVal True) -> (TrueCond, env, k)
                (BoolVal False) -> (FalseCond, env, k)
--        Nothing -> error "Unbound variable: " ++ show x

eval1 (e, env, k) | isValue e = 
    case k of
        [] -> (e, env, k) -- If empty kontinuation, then finished!!
        (frame:frames) -> applyFrame (exprToValue e) env frame frames

--                                                                                             || Binary Ops ||

eval1 ((Plus e1 e2), env, k) = (e1, env, HolePlus e2 env : k)           -- Plus +
eval1 ((Minus e1 e2), env, k) = (e1, env, HoleMinus e2 env : k)        -- Minus -
eval1 ((And e1 e2), env, k) = (e1, env, HoleAnd e2 env : k)              -- And &&
eval1 ((Or e1 e2), env, k) =   (e1, env, HoleOr e2 env : k)                 -- Or ||
eval1 ((LessThan e1 e2), env, k) = (e1, env, HoleLessThan e2 env : k) -- LessThan <
eval1 ((MoreThan e1 e2), env, k) = (e1, env, HoleMoreThan e2 env : k) -- MoreThan >
eval1 ((LessThanOrEqualTo e1 e2), env, k) = (e1,env, HoleLessThanOrEqualTo e2 env : k) -- LessThanOrEqualTo <=
eval1 ((MoreThanOrEqualTo e1 e2), env, k) = (e1, env, HoleMoreThanOrEqualTo e2 env : k) -- MoreThanOrEqualTo >=
eval1 ((NotEqualsTo e1 e2), env, k) = (e1, env, HoleNotEqualsTo e2 env : k) -- NotEqualsTo !=


--Apply frame handles the ACTUAL computation
applyFrame :: Value -> Environment -> Frame -> Kontinuation -> Configuration

--                                           || Arithmetic ||
--Plus +
applyFrame v1 env (HolePlus e2 env2) k = (e2, env2, PlusHole v1 : k)
applyFrame v2 env (PlusHole v1) k = valueToExpr result env k
    where result = NumVal (verifyNumVal v1 + verifyNumVal v2)

--Minus -
applyFrame v1 env (HoleMinus e2 env2) k = (e2, env2, MinusHole v1 : k)
applyFrame v2 env (MinusHole v1) k = valueToExpr result env k
    where result = NumVal (verifyNumVal v2 - verifyNumVal v2)

--                                       || Boolean ||
--And &&
applyFrame v1 env (HoleAnd e2 env2) k =
    if not (verifyBoolVal v1) then valueToExpr (BoolVal False) env k
    else (e2, env2, AndHole v1 : k)
applyFrame v2 env (AndHole v1) k = valueToExpr result env k
    where result = BoolVal (verifyBoolVal v1 && verifyBoolVal v2)

--Or ||
applyFrame v1 env (HoleOr e2 env2) k =
    if not (verifyBoolVal v1) then valueToExpr (BoolVal False) env k
    else (e2, env2, OrHole v1 : k)
applyFrame v2 env (OrHole v1) k  =valueToExpr result env k
    where result = BoolVal (verifyBoolVal v1 || verifyBoolVal v2)

--LessThan <
applyFrame v1 env (HoleLessThan e2 env2) k = (e2, env2, LessThanHole v1 : k)
applyFrame v2 env (LessThanHole v1) k =valueToExpr result env k
    where result = BoolVal (verifyNumVal v1 < verifyNumVal v2)

--MoreThan >
applyFrame v1 env (HoleMoreThan e2 env2) k = (e2, env2, MoreThanHole v1 : k)
applyFrame v2 env (MoreThanHole v1) k =valueToExpr result env k
    where result = BoolVal (verifyNumVal v1 > verifyNumVal v2)

-- LessThen or equal to <=
applyFrame v1 env (HoleLessThanOrEqualTo e2 env2) k = (e2, env2, LessThanOrEqualToHole v1 : k)
applyFrame v2 env (LessThanOrEqualToHole v1) k =valueToExpr result env k
    where result = BoolVal (verifyNumVal v1 <= verifyNumVal v2)

-- MoreThan or equal to >=
applyFrame v1 env (HoleMoreThanOrEqualTo e2 env2) k = (e2, env2, MoreThanOrEqualToHole v1 : k)
applyFrame v2 env (MoreThanOrEqualToHole v1) k =valueToExpr result env k
    where result = BoolVal (verifyNumVal v1 >= verifyNumVal v2)

-- NotEqualsTo !=
applyFrame v1 env (HoleNotEqualsTo e2 env2) k = (e2, env2, NotEqualsToHole v1 : k)
applyFrame v2 env (NotEqualsToHole v1) k =valueToExpr result env k
    where result = BoolVal (v1 /= v2)


-- Full eval function
eval :: Expr -> Environment -> Value
eval expr env =
    case evalLoop (expr, env, []) of
        (Var n, _, _) -> NumVal n
        (Int n, _, _) -> NumVal n
        (TrueCond, _, _) -> (BoolVal True)
        (FalseCond, _, _) -> (BoolVal False)
        _ -> error "Evaluation did not produce a value"
    where
        evalLoop :: Configuration -> Configuration
        evalLoop config@(e, env, []) | isValue e = config
        evalLoop config = evalLoop (eval1 config)




