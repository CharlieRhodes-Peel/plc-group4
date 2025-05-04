{-# OPTIONS_GHC -w #-}
module Grammar where
import Tokens
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.1.1

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t11 t14 t15
	= HappyTerminal (Token)
	| HappyErrorToken Prelude.Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 (Maybe JoinStatement)
	| HappyAbsSyn11 t11
	| HappyAbsSyn12 (Maybe Condition)
	| HappyAbsSyn13 (Maybe Order)
	| HappyAbsSyn14 t14
	| HappyAbsSyn15 t15

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,110) ([32768,0,0,0,512,0,0,0,0,0,128,0,0,14336,64,0,0,0,0,4,0,0,0,0,0,1,0,0,1024,0,0,0,0,0,0,4096,0,0,0,0,0,0,14336,64,0,0,32768,0,0,0,512,0,0,0,16,256,0,0,0,800,0,0,0,0,0,0,0,0,512,0,0,0,8,0,0,0,0,0,0,16,0,0,16384,0,0,0,0,0,0,0,0,2048,0,0,0,32,0,0,32768,0,0,0,0,256,0,0,0,32768,1,0,0,12288,0,0,0,0,0,0,0,16,0,0,16384,0,0,0,0,0,32768,0,0,0,0,0,1,0,0,1024,0,0,0,16,0,0,16440,0,0,57344,256,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,49152,0,0,0,0,8192,0,0,0,128,0,512,384,4,0,8,4102,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32768,0,0,0,512,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parseCalc","Statement","SelectStatement","SelectList","RowOrCol","FromList","TableRef","OptJoin","JoinStatement","OptWhere","OptOrderBy","Condition","Order","SELECT","FROM","JOIN","WHERE","INTO","ON","CROSS","LEFT","RIGHT","INNER","OUTER","UNION","INTERSECTION","IS","IN","AS","NULL","ORDER","BY","ASC","DSC","DROP","UPDATE","IF","ELSE","COUNT","SUM","AVG","MIN","MAX","ROW","COL","'*'","\"==\"","\"!=\"","','","';'","'('","')'","'\"'","int","var","%eof"]
        bit_start = st Prelude.* 58
        bit_end = (st Prelude.+ 1) Prelude.* 58
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..57]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (16) = happyShift action_3
action_0 (4) = happyGoto action_4
action_0 (5) = happyGoto action_2
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (16) = happyShift action_3
action_1 (5) = happyGoto action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (52) = happyShift action_10
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (46) = happyShift action_6
action_3 (47) = happyShift action_7
action_3 (48) = happyShift action_8
action_3 (57) = happyShift action_9
action_3 (6) = happyGoto action_5
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (58) = happyAccept
action_4 _ = happyFail (happyExpListPerState 4)

action_5 (17) = happyShift action_14
action_5 _ = happyFail (happyExpListPerState 5)

action_6 (53) = happyShift action_13
action_6 _ = happyFail (happyExpListPerState 6)

action_7 (53) = happyShift action_12
action_7 _ = happyFail (happyExpListPerState 7)

action_8 _ = happyReduce_3

action_9 (51) = happyShift action_11
action_9 _ = happyReduce_5

action_10 _ = happyReduce_1

action_11 (46) = happyShift action_6
action_11 (47) = happyShift action_7
action_11 (48) = happyShift action_8
action_11 (57) = happyShift action_9
action_11 (6) = happyGoto action_20
action_11 _ = happyFail (happyExpListPerState 11)

action_12 (56) = happyShift action_19
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (56) = happyShift action_18
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (57) = happyShift action_17
action_14 (8) = happyGoto action_15
action_14 (9) = happyGoto action_16
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (19) = happyShift action_29
action_15 (12) = happyGoto action_28
action_15 _ = happyReduce_20

action_16 (19) = happyReduce_15
action_16 (22) = happyShift action_25
action_16 (25) = happyShift action_26
action_16 (26) = happyShift action_27
action_16 (33) = happyReduce_15
action_16 (52) = happyReduce_15
action_16 (10) = happyGoto action_23
action_16 (11) = happyGoto action_24
action_16 _ = happyReduce_15

action_17 _ = happyReduce_14

action_18 (54) = happyShift action_22
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (54) = happyShift action_21
action_19 _ = happyFail (happyExpListPerState 19)

action_20 _ = happyReduce_4

action_21 (51) = happyShift action_40
action_21 _ = happyReduce_9

action_22 (51) = happyShift action_39
action_22 _ = happyReduce_7

action_23 _ = happyReduce_12

action_24 _ = happyReduce_16

action_25 (18) = happyShift action_38
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (18) = happyShift action_37
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (18) = happyShift action_36
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (33) = happyShift action_35
action_28 (13) = happyGoto action_34
action_28 _ = happyReduce_22

action_29 (46) = happyShift action_32
action_29 (47) = happyShift action_33
action_29 (7) = happyGoto action_30
action_29 (14) = happyGoto action_31
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (49) = happyShift action_49
action_30 (50) = happyShift action_50
action_30 _ = happyFail (happyExpListPerState 30)

action_31 _ = happyReduce_21

action_32 (53) = happyShift action_48
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (53) = happyShift action_47
action_33 _ = happyFail (happyExpListPerState 33)

action_34 _ = happyReduce_2

action_35 (34) = happyShift action_46
action_35 _ = happyFail (happyExpListPerState 35)

action_36 (57) = happyShift action_45
action_36 _ = happyFail (happyExpListPerState 36)

action_37 (57) = happyShift action_44
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (57) = happyShift action_43
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (46) = happyShift action_6
action_39 (47) = happyShift action_7
action_39 (48) = happyShift action_8
action_39 (57) = happyShift action_9
action_39 (6) = happyGoto action_42
action_39 _ = happyFail (happyExpListPerState 39)

action_40 (46) = happyShift action_6
action_40 (47) = happyShift action_7
action_40 (48) = happyShift action_8
action_40 (57) = happyShift action_9
action_40 (6) = happyGoto action_41
action_40 _ = happyFail (happyExpListPerState 40)

action_41 _ = happyReduce_8

action_42 _ = happyReduce_6

action_43 _ = happyReduce_17

action_44 _ = happyReduce_18

action_45 _ = happyReduce_19

action_46 (35) = happyShift action_60
action_46 (36) = happyShift action_61
action_46 (15) = happyGoto action_59
action_46 _ = happyFail (happyExpListPerState 46)

action_47 (56) = happyShift action_58
action_47 _ = happyFail (happyExpListPerState 47)

action_48 (56) = happyShift action_57
action_48 _ = happyFail (happyExpListPerState 48)

action_49 (32) = happyShift action_55
action_49 (46) = happyShift action_32
action_49 (47) = happyShift action_33
action_49 (57) = happyShift action_56
action_49 (7) = happyGoto action_54
action_49 _ = happyFail (happyExpListPerState 49)

action_50 (32) = happyShift action_52
action_50 (46) = happyShift action_32
action_50 (47) = happyShift action_33
action_50 (57) = happyShift action_53
action_50 (7) = happyGoto action_51
action_50 _ = happyFail (happyExpListPerState 50)

action_51 _ = happyReduce_27

action_52 _ = happyReduce_29

action_53 _ = happyReduce_28

action_54 _ = happyReduce_24

action_55 _ = happyReduce_26

action_56 _ = happyReduce_25

action_57 (54) = happyShift action_63
action_57 _ = happyFail (happyExpListPerState 57)

action_58 (54) = happyShift action_62
action_58 _ = happyFail (happyExpListPerState 58)

action_59 _ = happyReduce_23

action_60 _ = happyReduce_30

action_61 _ = happyReduce_31

action_62 _ = happyReduce_11

action_63 _ = happyReduce_10

happyReduce_1 = happySpecReduce_2  4 happyReduction_1
happyReduction_1 _
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_1 _ _  = notHappyAtAll 

happyReduce_2 = happyReduce 6 5 happyReduction_2
happyReduction_2 ((HappyAbsSyn13  happy_var_6) `HappyStk`
	(HappyAbsSyn12  happy_var_5) `HappyStk`
	(HappyAbsSyn8  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (SELECT happy_var_2 happy_var_4 happy_var_5 happy_var_6
	) `HappyStk` happyRest

happyReduce_3 = happySpecReduce_1  6 happyReduction_3
happyReduction_3 _
	 =  HappyAbsSyn6
		 (SelectAll
	)

happyReduce_4 = happySpecReduce_3  6 happyReduction_4
happyReduction_4 (HappyAbsSyn6  happy_var_3)
	_
	(HappyTerminal (TokenVar _ happy_var_1))
	 =  HappyAbsSyn6
		 (SelectRowAnd happy_var_1 happy_var_3
	)
happyReduction_4 _ _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_1  6 happyReduction_5
happyReduction_5 (HappyTerminal (TokenVar _ happy_var_1))
	 =  HappyAbsSyn6
		 (SelectRow happy_var_1
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happyReduce 6 6 happyReduction_6
happyReduction_6 ((HappyAbsSyn6  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenInt _ happy_var_3)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (SelectRowNumAnd happy_var_3 happy_var_6
	) `HappyStk` happyRest

happyReduce_7 = happyReduce 4 6 happyReduction_7
happyReduction_7 (_ `HappyStk`
	(HappyTerminal (TokenInt _ happy_var_3)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (SelectRowNum happy_var_3
	) `HappyStk` happyRest

happyReduce_8 = happyReduce 6 6 happyReduction_8
happyReduction_8 ((HappyAbsSyn6  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenInt _ happy_var_3)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (SelectColNumAnd happy_var_3 happy_var_6
	) `HappyStk` happyRest

happyReduce_9 = happyReduce 4 6 happyReduction_9
happyReduction_9 (_ `HappyStk`
	(HappyTerminal (TokenInt _ happy_var_3)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (SelectColNum happy_var_3
	) `HappyStk` happyRest

happyReduce_10 = happyReduce 4 7 happyReduction_10
happyReduction_10 (_ `HappyStk`
	(HappyTerminal (TokenInt _ happy_var_3)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (RowNum happy_var_3
	) `HappyStk` happyRest

happyReduce_11 = happyReduce 4 7 happyReduction_11
happyReduction_11 (_ `HappyStk`
	(HappyTerminal (TokenInt _ happy_var_3)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (ColNum happy_var_3
	) `HappyStk` happyRest

happyReduce_12 = happySpecReduce_2  8 happyReduction_12
happyReduction_12 (HappyAbsSyn10  happy_var_2)
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn8
		 (OptJoin happy_var_1 happy_var_2
	)
happyReduction_12 _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  8 happyReduction_13
happyReduction_13 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn8
		 (SingleFrom happy_var_1
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_1  9 happyReduction_14
happyReduction_14 (HappyTerminal (TokenVar _ happy_var_1))
	 =  HappyAbsSyn9
		 (SimpleTableRef happy_var_1
	)
happyReduction_14 _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_0  10 happyReduction_15
happyReduction_15  =  HappyAbsSyn10
		 (Nothing
	)

happyReduce_16 = happySpecReduce_1  10 happyReduction_16
happyReduction_16 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn10
		 (Just happy_var_1
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_3  11 happyReduction_17
happyReduction_17 (HappyTerminal (TokenVar _ happy_var_3))
	_
	_
	 =  HappyAbsSyn11
		 (CrossJoin happy_var_3
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_3  11 happyReduction_18
happyReduction_18 (HappyTerminal (TokenVar _ happy_var_3))
	_
	_
	 =  HappyAbsSyn11
		 (InnerJoin happy_var_3
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  11 happyReduction_19
happyReduction_19 (HappyTerminal (TokenVar _ happy_var_3))
	_
	_
	 =  HappyAbsSyn11
		 (OuterJoin happy_var_3
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_0  12 happyReduction_20
happyReduction_20  =  HappyAbsSyn12
		 (Nothing
	)

happyReduce_21 = happySpecReduce_2  12 happyReduction_21
happyReduction_21 (HappyAbsSyn14  happy_var_2)
	_
	 =  HappyAbsSyn12
		 (Just happy_var_2
	)
happyReduction_21 _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_0  13 happyReduction_22
happyReduction_22  =  HappyAbsSyn13
		 (Nothing
	)

happyReduce_23 = happySpecReduce_3  13 happyReduction_23
happyReduction_23 (HappyAbsSyn15  happy_var_3)
	_
	_
	 =  HappyAbsSyn13
		 (Just happy_var_3
	)
happyReduction_23 _ _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_3  14 happyReduction_24
happyReduction_24 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn14
		 (Equals happy_var_1 happy_var_3
	)
happyReduction_24 _ _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_3  14 happyReduction_25
happyReduction_25 (HappyTerminal (TokenVar _ happy_var_3))
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn14
		 (EqualTo happy_var_1 happy_var_3
	)
happyReduction_25 _ _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_3  14 happyReduction_26
happyReduction_26 _
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn14
		 (EqualToNull happy_var_1
	)
happyReduction_26 _ _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_3  14 happyReduction_27
happyReduction_27 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn14
		 (NotEquals happy_var_1 happy_var_3
	)
happyReduction_27 _ _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_3  14 happyReduction_28
happyReduction_28 (HappyTerminal (TokenVar _ happy_var_3))
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn14
		 (NotEqualTo happy_var_1 happy_var_3
	)
happyReduction_28 _ _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_3  14 happyReduction_29
happyReduction_29 _
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn14
		 (NotEqualToNull happy_var_1
	)
happyReduction_29 _ _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_1  15 happyReduction_30
happyReduction_30 _
	 =  HappyAbsSyn15
		 (ASC
	)

happyReduce_31 = happySpecReduce_1  15 happyReduction_31
happyReduction_31 _
	 =  HappyAbsSyn15
		 (DSC
	)

happyNewToken action sts stk [] =
	action 58 58 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenSELECT _ -> cont 16;
	TokenFROM _ -> cont 17;
	TokenJOIN _ -> cont 18;
	TokenWHERE _ -> cont 19;
	TokenINTO _ -> cont 20;
	TokenON _ -> cont 21;
	TokenCROSS _ -> cont 22;
	TokenLEFT _ -> cont 23;
	TokenRIGHT _ -> cont 24;
	TokenINNER _ -> cont 25;
	TokenOUTER _ -> cont 26;
	TokenUNION _ -> cont 27;
	TokenINTERSECTION _ -> cont 28;
	TokenIS _ -> cont 29;
	TokenIN _ -> cont 30;
	TokenAS _ -> cont 31;
	TokenNULL _ -> cont 32;
	TokenORDER _ -> cont 33;
	TokenBY _ -> cont 34;
	TokenASC _ -> cont 35;
	TokenDSC _ -> cont 36;
	TokenDROP _ -> cont 37;
	TokenUPDATE _ -> cont 38;
	TokenIF _ -> cont 39;
	TokenELSE _ -> cont 40;
	TokenCOUNT _ -> cont 41;
	TokenSUM _ -> cont 42;
	TokenAVG _ -> cont 43;
	TokenMIN _ -> cont 44;
	TokenMAX _ -> cont 45;
	TokenROW _ -> cont 46;
	TokenCOL _ -> cont 47;
	TokenWildcard _ -> cont 48;
	TokenEqualsTo _ -> cont 49;
	TokenNotEqualsTo _ -> cont 50;
	TokenComma _ -> cont 51;
	TokenSemiColon _ -> cont 52;
	TokenLParen _ -> cont 53;
	TokenRParen _ -> cont 54;
	TokenSpeechMark _ -> cont 55;
	TokenInt _ happy_dollar_dollar -> cont 56;
	TokenVar _ happy_dollar_dollar -> cont 57;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 58 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Prelude.Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = HappyIdentity
    (<*>) = ap
instance Prelude.Monad HappyIdentity where
    return = pure
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (Prelude.>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (Prelude.return)
happyThen1 m k tks = (Prelude.>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (Prelude.return) a
happyError' :: () => ([(Token)], [Prelude.String]) -> HappyIdentity a
happyError' = HappyIdentity Prelude.. (\(tokens, _) -> parseError tokens)
parseCalc tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


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
                | SelectNull
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
                                | EqualTo RowOrCol String
                                | EqualToNull RowOrCol
                                | NotEquals RowOrCol RowOrCol
                                | NotEqualTo RowOrCol String
                                | NotEqualToNull RowOrCol

    deriving (Show)
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $










































data Happy_IntList = HappyCons Prelude.Int Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action









































indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x Prelude.< y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `Prelude.div` 16)) (bit `Prelude.mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Prelude.Int ->                    -- token number
         Prelude.Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Prelude.- ((1) :: Prelude.Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Prelude.Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n Prelude.- ((1) :: Prelude.Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Prelude.- ((1)::Prelude.Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ((HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Prelude.error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `Prelude.seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.









{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
