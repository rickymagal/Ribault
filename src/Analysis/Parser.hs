{-# OPTIONS_GHC -w #-}
{-# OPTIONS -XMagicHash -XBangPatterns -XTypeSynonymInstances -XFlexibleInstances -cpp #-}
#if __GLASGOW_HASKELL__ >= 710
{-# OPTIONS_GHC -XPartialTypeSignatures #-}
#endif
module Analysis.Parser where

import Syntax
import Analysis.Lexer (Token(..))
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import qualified GHC.Exts as Happy_GHC_Exts
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.1.1

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Prelude.Int
	| HappyAbsSyn4 (Program)
	| HappyAbsSyn5 ([Decl])
	| HappyAbsSyn7 (Decl)
	| HappyAbsSyn8 ([Ident])
	| HappyAbsSyn9 (Expr)
	| HappyAbsSyn15 ([(Pattern,Expr)])
	| HappyAbsSyn17 (())
	| HappyAbsSyn18 ((Pattern,Expr))
	| HappyAbsSyn22 ([Expr])
	| HappyAbsSyn26 ([(BinOperator,Expr)])
	| HappyAbsSyn27 (BinOperator)
	| HappyAbsSyn41 (SuperKind)
	| HappyAbsSyn42 (Literal)
	| HappyAbsSyn49 (Pattern)
	| HappyAbsSyn51 ([Pattern])

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m = 
	   Happy_GHC_Exts.Int# 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> m HappyAbsSyn
-}

action_0,
 action_1,
 action_2,
 action_3,
 action_4,
 action_5,
 action_6,
 action_7,
 action_8,
 action_9,
 action_10,
 action_11,
 action_12,
 action_13,
 action_14,
 action_15,
 action_16,
 action_17,
 action_18,
 action_19,
 action_20,
 action_21,
 action_22,
 action_23,
 action_24,
 action_25,
 action_26,
 action_27,
 action_28,
 action_29,
 action_30,
 action_31,
 action_32,
 action_33,
 action_34,
 action_35,
 action_36,
 action_37,
 action_38,
 action_39,
 action_40,
 action_41,
 action_42,
 action_43,
 action_44,
 action_45,
 action_46,
 action_47,
 action_48,
 action_49,
 action_50,
 action_51,
 action_52,
 action_53,
 action_54,
 action_55,
 action_56,
 action_57,
 action_58,
 action_59,
 action_60,
 action_61,
 action_62,
 action_63,
 action_64,
 action_65,
 action_66,
 action_67,
 action_68,
 action_69,
 action_70,
 action_71,
 action_72,
 action_73,
 action_74,
 action_75,
 action_76,
 action_77,
 action_78,
 action_79,
 action_80,
 action_81,
 action_82,
 action_83,
 action_84,
 action_85,
 action_86,
 action_87,
 action_88,
 action_89,
 action_90,
 action_91,
 action_92,
 action_93,
 action_94,
 action_95,
 action_96,
 action_97,
 action_98,
 action_99,
 action_100,
 action_101,
 action_102,
 action_103,
 action_104,
 action_105,
 action_106,
 action_107,
 action_108,
 action_109,
 action_110,
 action_111,
 action_112,
 action_113,
 action_114,
 action_115,
 action_116,
 action_117,
 action_118,
 action_119,
 action_120,
 action_121,
 action_122,
 action_123,
 action_124,
 action_125,
 action_126,
 action_127,
 action_128,
 action_129,
 action_130,
 action_131,
 action_132,
 action_133,
 action_134,
 action_135,
 action_136,
 action_137,
 action_138,
 action_139,
 action_140,
 action_141,
 action_142,
 action_143,
 action_144,
 action_145,
 action_146,
 action_147,
 action_148,
 action_149,
 action_150,
 action_151,
 action_152,
 action_153,
 action_154,
 action_155,
 action_156,
 action_157,
 action_158,
 action_159,
 action_160,
 action_161,
 action_162,
 action_163,
 action_164,
 action_165,
 action_166,
 action_167,
 action_168,
 action_169,
 action_170,
 action_171,
 action_172,
 action_173,
 action_174,
 action_175 :: () => Happy_GHC_Exts.Int# -> ({-HappyReduction (HappyIdentity) = -}
	   Happy_GHC_Exts.Int# 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (HappyIdentity) HappyAbsSyn)

happyReduce_1,
 happyReduce_2,
 happyReduce_3,
 happyReduce_4,
 happyReduce_5,
 happyReduce_6,
 happyReduce_7,
 happyReduce_8,
 happyReduce_9,
 happyReduce_10,
 happyReduce_11,
 happyReduce_12,
 happyReduce_13,
 happyReduce_14,
 happyReduce_15,
 happyReduce_16,
 happyReduce_17,
 happyReduce_18,
 happyReduce_19,
 happyReduce_20,
 happyReduce_21,
 happyReduce_22,
 happyReduce_23,
 happyReduce_24,
 happyReduce_25,
 happyReduce_26,
 happyReduce_27,
 happyReduce_28,
 happyReduce_29,
 happyReduce_30,
 happyReduce_31,
 happyReduce_32,
 happyReduce_33,
 happyReduce_34,
 happyReduce_35,
 happyReduce_36,
 happyReduce_37,
 happyReduce_38,
 happyReduce_39,
 happyReduce_40,
 happyReduce_41,
 happyReduce_42,
 happyReduce_43,
 happyReduce_44,
 happyReduce_45,
 happyReduce_46,
 happyReduce_47,
 happyReduce_48,
 happyReduce_49,
 happyReduce_50,
 happyReduce_51,
 happyReduce_52,
 happyReduce_53,
 happyReduce_54,
 happyReduce_55,
 happyReduce_56,
 happyReduce_57,
 happyReduce_58,
 happyReduce_59,
 happyReduce_60,
 happyReduce_61,
 happyReduce_62,
 happyReduce_63,
 happyReduce_64,
 happyReduce_65,
 happyReduce_66,
 happyReduce_67,
 happyReduce_68,
 happyReduce_69,
 happyReduce_70,
 happyReduce_71,
 happyReduce_72,
 happyReduce_73,
 happyReduce_74,
 happyReduce_75,
 happyReduce_76,
 happyReduce_77,
 happyReduce_78,
 happyReduce_79,
 happyReduce_80,
 happyReduce_81,
 happyReduce_82,
 happyReduce_83,
 happyReduce_84,
 happyReduce_85,
 happyReduce_86,
 happyReduce_87,
 happyReduce_88,
 happyReduce_89,
 happyReduce_90,
 happyReduce_91,
 happyReduce_92,
 happyReduce_93,
 happyReduce_94,
 happyReduce_95,
 happyReduce_96,
 happyReduce_97,
 happyReduce_98,
 happyReduce_99,
 happyReduce_100,
 happyReduce_101,
 happyReduce_102,
 happyReduce_103 :: () => ({-HappyReduction (HappyIdentity) = -}
	   Happy_GHC_Exts.Int# 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (HappyIdentity) HappyAbsSyn)

happyExpList :: HappyAddr
happyExpList = HappyA# "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x28\x95\x22\x00\xfe\x01\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc0\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc0\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x28\x00\xe0\x1f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\xa0\x54\x8a\x00\xf8\x07\x00\x00\x00\x00\x00\x00\x00\x28\x95\x22\x00\xfe\x01\x00\x00\x00\x00\x00\x00\x00\x00\xa1\x08\x80\x7f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\xa0\x54\x8a\x00\xf8\x07\x00\x00\x00\x00\x00\x00\x00\x28\x95\x22\x00\xfe\x01\x00\x00\x00\x00\x00\x00\x00\x00\xa1\x08\x80\x7f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x90\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x44\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0a\x00\xf8\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa1\x08\x80\x7f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x28\x02\xe0\x1f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x28\x02\xe0\x1f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x84\x22\x00\xfe\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x84\x22\x00\xfe\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x28\x02\xe0\x1f\x00\x00\x00\x00\x00\x00\x00\xa0\x54\x8a\x00\xf8\x07\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x18\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x78\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x1c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x52\x29\x02\xe0\x1f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0b\x00\xf8\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x52\x29\x02\xe0\x1f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x28\x95\x22\x00\xfe\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa0\x54\x8a\x00\xf8\x07\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x40\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0b\x00\xf8\x03\x00\x00\x00\x00\x00\x00\x00\x00\xc0\x02\x00\xfe\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa0\x54\x8a\x00\xf8\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa0\x54\x8a\x00\xf8\x07\x00\x00\x00\x00\x00\x00\x00\x04\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x24\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc0\x02\x00\xfe\x00\x00\x00\x00\x00\x00\x00\x00\x4a\xa5\x08\x80\x7f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2c\x00\xe0\x0f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa0\x54\x8a\x00\xf8\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x10\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb0\x00\x80\x3f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc0\x02\x00\xfe\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc0\x02\x00\xfe\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parse","Program","DeclList","DeclRest","Decl","Params","Expr","Lambda","LamParams","IdentList","IfExpr","CaseExpr","Alts","AltList","LayoutEnd","Alt","LetExpr","DeclBlockEnd","ExprOr","OrRest","ExprAnd","AndRest","ExprEq","EqRest","EqOp","ExprRel","RelRest","RelOp","ExprAdd","AddRest","AddOp","ExprMul","MulRest","MulOp","ExprUnary","ExprApp","AppTail","Atom","SuperKind","Literal","List","ExprListOpt","ExprList","ExprListTail","Tuple","TupleTail","Pattern","ListPattern","PatternListOpt","PatternList","PatternListTail","TuplePattern","PatternTupleTail","superbody","\";\"","\"layout_end\"","\":\"","\"let\"","\"in\"","\"if\"","\"then\"","\"else\"","\"case\"","\"of\"","\"not\"","\"->\"","\"\\\\\"","\"=\"","\"_\"","\"(\"","\")\"","\"[\"","\"]\"","\",\"","\"+\"","\"-\"","\"*\"","\"/\"","\"%\"","\"==\"","\"/=\"","\"<\"","\"<=\"","\">\"","\">=\"","\"&&\"","\"||\"","int","float","char","string","\"True\"","\"False\"","ident","\"super\"","\"single\"","\"parallel\"","\"input\"","\"output\"","%eof"]
        bit_start = st Prelude.* 102
        bit_end = (st Prelude.+ 1) Prelude.* 102
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..101]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (96#) = happyShift action_4
action_0 (4#) = happyGoto action_5
action_0 (5#) = happyGoto action_2
action_0 (7#) = happyGoto action_3
action_0 x = happyTcHack x happyFail (happyExpListPerState 0)

action_1 (96#) = happyShift action_4
action_1 (5#) = happyGoto action_2
action_1 (7#) = happyGoto action_3
action_1 x = happyTcHack x happyFail (happyExpListPerState 1)

action_2 x = happyTcHack x happyReduce_1

action_3 (57#) = happyShift action_9
action_3 (6#) = happyGoto action_8
action_3 x = happyTcHack x happyReduce_3

action_4 (96#) = happyShift action_7
action_4 (8#) = happyGoto action_6
action_4 x = happyTcHack x happyReduce_7

action_5 (102#) = happyAccept
action_5 x = happyTcHack x happyFail (happyExpListPerState 5)

action_6 (70#) = happyShift action_12
action_6 x = happyTcHack x happyFail (happyExpListPerState 6)

action_7 (96#) = happyShift action_7
action_7 (8#) = happyGoto action_11
action_7 x = happyTcHack x happyReduce_7

action_8 x = happyTcHack x happyReduce_2

action_9 (96#) = happyShift action_4
action_9 (7#) = happyGoto action_10
action_9 x = happyTcHack x happyReduce_4

action_10 (57#) = happyShift action_9
action_10 (6#) = happyGoto action_46
action_10 x = happyTcHack x happyReduce_3

action_11 x = happyTcHack x happyReduce_8

action_12 (60#) = happyShift action_30
action_12 (62#) = happyShift action_31
action_12 (65#) = happyShift action_32
action_12 (67#) = happyShift action_33
action_12 (69#) = happyShift action_34
action_12 (72#) = happyShift action_35
action_12 (74#) = happyShift action_36
action_12 (78#) = happyShift action_37
action_12 (90#) = happyShift action_38
action_12 (91#) = happyShift action_39
action_12 (92#) = happyShift action_40
action_12 (93#) = happyShift action_41
action_12 (94#) = happyShift action_42
action_12 (95#) = happyShift action_43
action_12 (96#) = happyShift action_44
action_12 (97#) = happyShift action_45
action_12 (9#) = happyGoto action_13
action_12 (10#) = happyGoto action_14
action_12 (13#) = happyGoto action_15
action_12 (14#) = happyGoto action_16
action_12 (19#) = happyGoto action_17
action_12 (21#) = happyGoto action_18
action_12 (23#) = happyGoto action_19
action_12 (25#) = happyGoto action_20
action_12 (28#) = happyGoto action_21
action_12 (31#) = happyGoto action_22
action_12 (34#) = happyGoto action_23
action_12 (37#) = happyGoto action_24
action_12 (38#) = happyGoto action_25
action_12 (40#) = happyGoto action_26
action_12 (42#) = happyGoto action_27
action_12 (43#) = happyGoto action_28
action_12 (47#) = happyGoto action_29
action_12 x = happyTcHack x happyFail (happyExpListPerState 12)

action_13 (59#) = happyShift action_87
action_13 x = happyTcHack x happyReduce_6

action_14 x = happyTcHack x happyReduce_9

action_15 x = happyTcHack x happyReduce_10

action_16 x = happyTcHack x happyReduce_11

action_17 x = happyTcHack x happyReduce_12

action_18 x = happyTcHack x happyReduce_13

action_19 (89#) = happyShift action_86
action_19 (22#) = happyGoto action_85
action_19 x = happyTcHack x happyReduce_31

action_20 (88#) = happyShift action_84
action_20 (24#) = happyGoto action_83
action_20 x = happyTcHack x happyReduce_34

action_21 (82#) = happyShift action_81
action_21 (83#) = happyShift action_82
action_21 (26#) = happyGoto action_79
action_21 (27#) = happyGoto action_80
action_21 x = happyTcHack x happyReduce_37

action_22 (84#) = happyShift action_75
action_22 (85#) = happyShift action_76
action_22 (86#) = happyShift action_77
action_22 (87#) = happyShift action_78
action_22 (29#) = happyGoto action_73
action_22 (30#) = happyGoto action_74
action_22 x = happyTcHack x happyReduce_42

action_23 (77#) = happyShift action_71
action_23 (78#) = happyShift action_72
action_23 (32#) = happyGoto action_69
action_23 (33#) = happyGoto action_70
action_23 x = happyTcHack x happyReduce_49

action_24 (79#) = happyShift action_66
action_24 (80#) = happyShift action_67
action_24 (81#) = happyShift action_68
action_24 (35#) = happyGoto action_64
action_24 (36#) = happyGoto action_65
action_24 x = happyTcHack x happyReduce_54

action_25 x = happyTcHack x happyReduce_61

action_26 (72#) = happyShift action_35
action_26 (74#) = happyShift action_36
action_26 (90#) = happyShift action_38
action_26 (91#) = happyShift action_39
action_26 (92#) = happyShift action_40
action_26 (93#) = happyShift action_41
action_26 (94#) = happyShift action_42
action_26 (95#) = happyShift action_43
action_26 (96#) = happyShift action_44
action_26 (97#) = happyShift action_45
action_26 (39#) = happyGoto action_62
action_26 (40#) = happyGoto action_63
action_26 (42#) = happyGoto action_27
action_26 (43#) = happyGoto action_28
action_26 (47#) = happyGoto action_29
action_26 x = happyTcHack x happyReduce_63

action_27 x = happyTcHack x happyReduce_65

action_28 x = happyTcHack x happyReduce_68

action_29 x = happyTcHack x happyReduce_69

action_30 (96#) = happyShift action_4
action_30 (5#) = happyGoto action_61
action_30 (7#) = happyGoto action_3
action_30 x = happyTcHack x happyFail (happyExpListPerState 30)

action_31 (60#) = happyShift action_30
action_31 (62#) = happyShift action_31
action_31 (65#) = happyShift action_32
action_31 (67#) = happyShift action_33
action_31 (69#) = happyShift action_34
action_31 (72#) = happyShift action_35
action_31 (74#) = happyShift action_36
action_31 (78#) = happyShift action_37
action_31 (90#) = happyShift action_38
action_31 (91#) = happyShift action_39
action_31 (92#) = happyShift action_40
action_31 (93#) = happyShift action_41
action_31 (94#) = happyShift action_42
action_31 (95#) = happyShift action_43
action_31 (96#) = happyShift action_44
action_31 (97#) = happyShift action_45
action_31 (9#) = happyGoto action_60
action_31 (10#) = happyGoto action_14
action_31 (13#) = happyGoto action_15
action_31 (14#) = happyGoto action_16
action_31 (19#) = happyGoto action_17
action_31 (21#) = happyGoto action_18
action_31 (23#) = happyGoto action_19
action_31 (25#) = happyGoto action_20
action_31 (28#) = happyGoto action_21
action_31 (31#) = happyGoto action_22
action_31 (34#) = happyGoto action_23
action_31 (37#) = happyGoto action_24
action_31 (38#) = happyGoto action_25
action_31 (40#) = happyGoto action_26
action_31 (42#) = happyGoto action_27
action_31 (43#) = happyGoto action_28
action_31 (47#) = happyGoto action_29
action_31 x = happyTcHack x happyFail (happyExpListPerState 31)

action_32 (60#) = happyShift action_30
action_32 (62#) = happyShift action_31
action_32 (65#) = happyShift action_32
action_32 (67#) = happyShift action_33
action_32 (69#) = happyShift action_34
action_32 (72#) = happyShift action_35
action_32 (74#) = happyShift action_36
action_32 (78#) = happyShift action_37
action_32 (90#) = happyShift action_38
action_32 (91#) = happyShift action_39
action_32 (92#) = happyShift action_40
action_32 (93#) = happyShift action_41
action_32 (94#) = happyShift action_42
action_32 (95#) = happyShift action_43
action_32 (96#) = happyShift action_44
action_32 (97#) = happyShift action_45
action_32 (9#) = happyGoto action_59
action_32 (10#) = happyGoto action_14
action_32 (13#) = happyGoto action_15
action_32 (14#) = happyGoto action_16
action_32 (19#) = happyGoto action_17
action_32 (21#) = happyGoto action_18
action_32 (23#) = happyGoto action_19
action_32 (25#) = happyGoto action_20
action_32 (28#) = happyGoto action_21
action_32 (31#) = happyGoto action_22
action_32 (34#) = happyGoto action_23
action_32 (37#) = happyGoto action_24
action_32 (38#) = happyGoto action_25
action_32 (40#) = happyGoto action_26
action_32 (42#) = happyGoto action_27
action_32 (43#) = happyGoto action_28
action_32 (47#) = happyGoto action_29
action_32 x = happyTcHack x happyFail (happyExpListPerState 32)

action_33 (67#) = happyShift action_33
action_33 (72#) = happyShift action_35
action_33 (74#) = happyShift action_36
action_33 (78#) = happyShift action_37
action_33 (90#) = happyShift action_38
action_33 (91#) = happyShift action_39
action_33 (92#) = happyShift action_40
action_33 (93#) = happyShift action_41
action_33 (94#) = happyShift action_42
action_33 (95#) = happyShift action_43
action_33 (96#) = happyShift action_44
action_33 (97#) = happyShift action_45
action_33 (37#) = happyGoto action_58
action_33 (38#) = happyGoto action_25
action_33 (40#) = happyGoto action_26
action_33 (42#) = happyGoto action_27
action_33 (43#) = happyGoto action_28
action_33 (47#) = happyGoto action_29
action_33 x = happyTcHack x happyFail (happyExpListPerState 33)

action_34 (72#) = happyShift action_57
action_34 (96#) = happyShift action_7
action_34 (8#) = happyGoto action_55
action_34 (11#) = happyGoto action_56
action_34 x = happyTcHack x happyReduce_7

action_35 (60#) = happyShift action_30
action_35 (62#) = happyShift action_31
action_35 (65#) = happyShift action_32
action_35 (67#) = happyShift action_33
action_35 (69#) = happyShift action_34
action_35 (72#) = happyShift action_35
action_35 (74#) = happyShift action_36
action_35 (78#) = happyShift action_37
action_35 (90#) = happyShift action_38
action_35 (91#) = happyShift action_39
action_35 (92#) = happyShift action_40
action_35 (93#) = happyShift action_41
action_35 (94#) = happyShift action_42
action_35 (95#) = happyShift action_43
action_35 (96#) = happyShift action_44
action_35 (97#) = happyShift action_45
action_35 (9#) = happyGoto action_54
action_35 (10#) = happyGoto action_14
action_35 (13#) = happyGoto action_15
action_35 (14#) = happyGoto action_16
action_35 (19#) = happyGoto action_17
action_35 (21#) = happyGoto action_18
action_35 (23#) = happyGoto action_19
action_35 (25#) = happyGoto action_20
action_35 (28#) = happyGoto action_21
action_35 (31#) = happyGoto action_22
action_35 (34#) = happyGoto action_23
action_35 (37#) = happyGoto action_24
action_35 (38#) = happyGoto action_25
action_35 (40#) = happyGoto action_26
action_35 (42#) = happyGoto action_27
action_35 (43#) = happyGoto action_28
action_35 (47#) = happyGoto action_29
action_35 x = happyTcHack x happyFail (happyExpListPerState 35)

action_36 (60#) = happyShift action_30
action_36 (62#) = happyShift action_31
action_36 (65#) = happyShift action_32
action_36 (67#) = happyShift action_33
action_36 (69#) = happyShift action_34
action_36 (72#) = happyShift action_35
action_36 (74#) = happyShift action_36
action_36 (78#) = happyShift action_37
action_36 (90#) = happyShift action_38
action_36 (91#) = happyShift action_39
action_36 (92#) = happyShift action_40
action_36 (93#) = happyShift action_41
action_36 (94#) = happyShift action_42
action_36 (95#) = happyShift action_43
action_36 (96#) = happyShift action_44
action_36 (97#) = happyShift action_45
action_36 (9#) = happyGoto action_51
action_36 (10#) = happyGoto action_14
action_36 (13#) = happyGoto action_15
action_36 (14#) = happyGoto action_16
action_36 (19#) = happyGoto action_17
action_36 (21#) = happyGoto action_18
action_36 (23#) = happyGoto action_19
action_36 (25#) = happyGoto action_20
action_36 (28#) = happyGoto action_21
action_36 (31#) = happyGoto action_22
action_36 (34#) = happyGoto action_23
action_36 (37#) = happyGoto action_24
action_36 (38#) = happyGoto action_25
action_36 (40#) = happyGoto action_26
action_36 (42#) = happyGoto action_27
action_36 (43#) = happyGoto action_28
action_36 (44#) = happyGoto action_52
action_36 (45#) = happyGoto action_53
action_36 (47#) = happyGoto action_29
action_36 x = happyTcHack x happyReduce_80

action_37 (67#) = happyShift action_33
action_37 (72#) = happyShift action_35
action_37 (74#) = happyShift action_36
action_37 (78#) = happyShift action_37
action_37 (90#) = happyShift action_38
action_37 (91#) = happyShift action_39
action_37 (92#) = happyShift action_40
action_37 (93#) = happyShift action_41
action_37 (94#) = happyShift action_42
action_37 (95#) = happyShift action_43
action_37 (96#) = happyShift action_44
action_37 (97#) = happyShift action_45
action_37 (37#) = happyGoto action_50
action_37 (38#) = happyGoto action_25
action_37 (40#) = happyGoto action_26
action_37 (42#) = happyGoto action_27
action_37 (43#) = happyGoto action_28
action_37 (47#) = happyGoto action_29
action_37 x = happyTcHack x happyFail (happyExpListPerState 37)

action_38 x = happyTcHack x happyReduce_73

action_39 x = happyTcHack x happyReduce_74

action_40 x = happyTcHack x happyReduce_75

action_41 x = happyTcHack x happyReduce_76

action_42 x = happyTcHack x happyReduce_77

action_43 x = happyTcHack x happyReduce_78

action_44 x = happyTcHack x happyReduce_66

action_45 (98#) = happyShift action_48
action_45 (99#) = happyShift action_49
action_45 (41#) = happyGoto action_47
action_45 x = happyTcHack x happyFail (happyExpListPerState 45)

action_46 x = happyTcHack x happyReduce_5

action_47 (100#) = happyShift action_108
action_47 x = happyTcHack x happyFail (happyExpListPerState 47)

action_48 x = happyTcHack x happyReduce_71

action_49 x = happyTcHack x happyReduce_72

action_50 x = happyTcHack x happyReduce_60

action_51 (59#) = happyShift action_87
action_51 (76#) = happyShift action_107
action_51 (46#) = happyGoto action_106
action_51 x = happyTcHack x happyReduce_83

action_52 (75#) = happyShift action_105
action_52 x = happyTcHack x happyFail (happyExpListPerState 52)

action_53 x = happyTcHack x happyReduce_81

action_54 (59#) = happyShift action_87
action_54 (73#) = happyShift action_103
action_54 (76#) = happyShift action_104
action_54 x = happyTcHack x happyFail (happyExpListPerState 54)

action_55 x = happyTcHack x happyReduce_16

action_56 (68#) = happyShift action_102
action_56 x = happyTcHack x happyFail (happyExpListPerState 56)

action_57 (96#) = happyShift action_101
action_57 (12#) = happyGoto action_100
action_57 x = happyTcHack x happyFail (happyExpListPerState 57)

action_58 x = happyTcHack x happyReduce_59

action_59 (59#) = happyShift action_87
action_59 (66#) = happyShift action_99
action_59 x = happyTcHack x happyFail (happyExpListPerState 59)

action_60 (59#) = happyShift action_87
action_60 (63#) = happyShift action_98
action_60 x = happyTcHack x happyFail (happyExpListPerState 60)

action_61 (58#) = happyShift action_97
action_61 (20#) = happyGoto action_96
action_61 x = happyTcHack x happyReduce_28

action_62 x = happyTcHack x happyReduce_62

action_63 (72#) = happyShift action_35
action_63 (74#) = happyShift action_36
action_63 (90#) = happyShift action_38
action_63 (91#) = happyShift action_39
action_63 (92#) = happyShift action_40
action_63 (93#) = happyShift action_41
action_63 (94#) = happyShift action_42
action_63 (95#) = happyShift action_43
action_63 (96#) = happyShift action_44
action_63 (97#) = happyShift action_45
action_63 (39#) = happyGoto action_95
action_63 (40#) = happyGoto action_63
action_63 (42#) = happyGoto action_27
action_63 (43#) = happyGoto action_28
action_63 (47#) = happyGoto action_29
action_63 x = happyTcHack x happyReduce_63

action_64 x = happyTcHack x happyReduce_53

action_65 (67#) = happyShift action_33
action_65 (72#) = happyShift action_35
action_65 (74#) = happyShift action_36
action_65 (78#) = happyShift action_37
action_65 (90#) = happyShift action_38
action_65 (91#) = happyShift action_39
action_65 (92#) = happyShift action_40
action_65 (93#) = happyShift action_41
action_65 (94#) = happyShift action_42
action_65 (95#) = happyShift action_43
action_65 (96#) = happyShift action_44
action_65 (97#) = happyShift action_45
action_65 (37#) = happyGoto action_94
action_65 (38#) = happyGoto action_25
action_65 (40#) = happyGoto action_26
action_65 (42#) = happyGoto action_27
action_65 (43#) = happyGoto action_28
action_65 (47#) = happyGoto action_29
action_65 x = happyTcHack x happyFail (happyExpListPerState 65)

action_66 x = happyTcHack x happyReduce_56

action_67 x = happyTcHack x happyReduce_57

action_68 x = happyTcHack x happyReduce_58

action_69 x = happyTcHack x happyReduce_48

action_70 (67#) = happyShift action_33
action_70 (72#) = happyShift action_35
action_70 (74#) = happyShift action_36
action_70 (78#) = happyShift action_37
action_70 (90#) = happyShift action_38
action_70 (91#) = happyShift action_39
action_70 (92#) = happyShift action_40
action_70 (93#) = happyShift action_41
action_70 (94#) = happyShift action_42
action_70 (95#) = happyShift action_43
action_70 (96#) = happyShift action_44
action_70 (97#) = happyShift action_45
action_70 (34#) = happyGoto action_93
action_70 (37#) = happyGoto action_24
action_70 (38#) = happyGoto action_25
action_70 (40#) = happyGoto action_26
action_70 (42#) = happyGoto action_27
action_70 (43#) = happyGoto action_28
action_70 (47#) = happyGoto action_29
action_70 x = happyTcHack x happyFail (happyExpListPerState 70)

action_71 x = happyTcHack x happyReduce_51

action_72 x = happyTcHack x happyReduce_52

action_73 x = happyTcHack x happyReduce_41

action_74 (67#) = happyShift action_33
action_74 (72#) = happyShift action_35
action_74 (74#) = happyShift action_36
action_74 (78#) = happyShift action_37
action_74 (90#) = happyShift action_38
action_74 (91#) = happyShift action_39
action_74 (92#) = happyShift action_40
action_74 (93#) = happyShift action_41
action_74 (94#) = happyShift action_42
action_74 (95#) = happyShift action_43
action_74 (96#) = happyShift action_44
action_74 (97#) = happyShift action_45
action_74 (31#) = happyGoto action_92
action_74 (34#) = happyGoto action_23
action_74 (37#) = happyGoto action_24
action_74 (38#) = happyGoto action_25
action_74 (40#) = happyGoto action_26
action_74 (42#) = happyGoto action_27
action_74 (43#) = happyGoto action_28
action_74 (47#) = happyGoto action_29
action_74 x = happyTcHack x happyFail (happyExpListPerState 74)

action_75 x = happyTcHack x happyReduce_44

action_76 x = happyTcHack x happyReduce_45

action_77 x = happyTcHack x happyReduce_46

action_78 x = happyTcHack x happyReduce_47

action_79 x = happyTcHack x happyReduce_36

action_80 (67#) = happyShift action_33
action_80 (72#) = happyShift action_35
action_80 (74#) = happyShift action_36
action_80 (78#) = happyShift action_37
action_80 (90#) = happyShift action_38
action_80 (91#) = happyShift action_39
action_80 (92#) = happyShift action_40
action_80 (93#) = happyShift action_41
action_80 (94#) = happyShift action_42
action_80 (95#) = happyShift action_43
action_80 (96#) = happyShift action_44
action_80 (97#) = happyShift action_45
action_80 (28#) = happyGoto action_91
action_80 (31#) = happyGoto action_22
action_80 (34#) = happyGoto action_23
action_80 (37#) = happyGoto action_24
action_80 (38#) = happyGoto action_25
action_80 (40#) = happyGoto action_26
action_80 (42#) = happyGoto action_27
action_80 (43#) = happyGoto action_28
action_80 (47#) = happyGoto action_29
action_80 x = happyTcHack x happyFail (happyExpListPerState 80)

action_81 x = happyTcHack x happyReduce_39

action_82 x = happyTcHack x happyReduce_40

action_83 x = happyTcHack x happyReduce_33

action_84 (67#) = happyShift action_33
action_84 (72#) = happyShift action_35
action_84 (74#) = happyShift action_36
action_84 (78#) = happyShift action_37
action_84 (90#) = happyShift action_38
action_84 (91#) = happyShift action_39
action_84 (92#) = happyShift action_40
action_84 (93#) = happyShift action_41
action_84 (94#) = happyShift action_42
action_84 (95#) = happyShift action_43
action_84 (96#) = happyShift action_44
action_84 (97#) = happyShift action_45
action_84 (25#) = happyGoto action_90
action_84 (28#) = happyGoto action_21
action_84 (31#) = happyGoto action_22
action_84 (34#) = happyGoto action_23
action_84 (37#) = happyGoto action_24
action_84 (38#) = happyGoto action_25
action_84 (40#) = happyGoto action_26
action_84 (42#) = happyGoto action_27
action_84 (43#) = happyGoto action_28
action_84 (47#) = happyGoto action_29
action_84 x = happyTcHack x happyFail (happyExpListPerState 84)

action_85 x = happyTcHack x happyReduce_30

action_86 (67#) = happyShift action_33
action_86 (72#) = happyShift action_35
action_86 (74#) = happyShift action_36
action_86 (78#) = happyShift action_37
action_86 (90#) = happyShift action_38
action_86 (91#) = happyShift action_39
action_86 (92#) = happyShift action_40
action_86 (93#) = happyShift action_41
action_86 (94#) = happyShift action_42
action_86 (95#) = happyShift action_43
action_86 (96#) = happyShift action_44
action_86 (97#) = happyShift action_45
action_86 (23#) = happyGoto action_89
action_86 (25#) = happyGoto action_20
action_86 (28#) = happyGoto action_21
action_86 (31#) = happyGoto action_22
action_86 (34#) = happyGoto action_23
action_86 (37#) = happyGoto action_24
action_86 (38#) = happyGoto action_25
action_86 (40#) = happyGoto action_26
action_86 (42#) = happyGoto action_27
action_86 (43#) = happyGoto action_28
action_86 (47#) = happyGoto action_29
action_86 x = happyTcHack x happyFail (happyExpListPerState 86)

action_87 (60#) = happyShift action_30
action_87 (62#) = happyShift action_31
action_87 (65#) = happyShift action_32
action_87 (67#) = happyShift action_33
action_87 (69#) = happyShift action_34
action_87 (72#) = happyShift action_35
action_87 (74#) = happyShift action_36
action_87 (78#) = happyShift action_37
action_87 (90#) = happyShift action_38
action_87 (91#) = happyShift action_39
action_87 (92#) = happyShift action_40
action_87 (93#) = happyShift action_41
action_87 (94#) = happyShift action_42
action_87 (95#) = happyShift action_43
action_87 (96#) = happyShift action_44
action_87 (97#) = happyShift action_45
action_87 (9#) = happyGoto action_88
action_87 (10#) = happyGoto action_14
action_87 (13#) = happyGoto action_15
action_87 (14#) = happyGoto action_16
action_87 (19#) = happyGoto action_17
action_87 (21#) = happyGoto action_18
action_87 (23#) = happyGoto action_19
action_87 (25#) = happyGoto action_20
action_87 (28#) = happyGoto action_21
action_87 (31#) = happyGoto action_22
action_87 (34#) = happyGoto action_23
action_87 (37#) = happyGoto action_24
action_87 (38#) = happyGoto action_25
action_87 (40#) = happyGoto action_26
action_87 (42#) = happyGoto action_27
action_87 (43#) = happyGoto action_28
action_87 (47#) = happyGoto action_29
action_87 x = happyTcHack x happyFail (happyExpListPerState 87)

action_88 (59#) = happyShift action_87
action_88 x = happyTcHack x happyReduce_14

action_89 (89#) = happyShift action_86
action_89 (22#) = happyGoto action_133
action_89 x = happyTcHack x happyReduce_31

action_90 (88#) = happyShift action_84
action_90 (24#) = happyGoto action_132
action_90 x = happyTcHack x happyReduce_34

action_91 (82#) = happyShift action_81
action_91 (83#) = happyShift action_82
action_91 (26#) = happyGoto action_131
action_91 (27#) = happyGoto action_80
action_91 x = happyTcHack x happyReduce_37

action_92 (84#) = happyShift action_75
action_92 (85#) = happyShift action_76
action_92 (86#) = happyShift action_77
action_92 (87#) = happyShift action_78
action_92 (29#) = happyGoto action_130
action_92 (30#) = happyGoto action_74
action_92 x = happyTcHack x happyReduce_42

action_93 (77#) = happyShift action_71
action_93 (78#) = happyShift action_72
action_93 (32#) = happyGoto action_129
action_93 (33#) = happyGoto action_70
action_93 x = happyTcHack x happyReduce_49

action_94 (79#) = happyShift action_66
action_94 (80#) = happyShift action_67
action_94 (81#) = happyShift action_68
action_94 (35#) = happyGoto action_128
action_94 (36#) = happyGoto action_65
action_94 x = happyTcHack x happyReduce_54

action_95 x = happyTcHack x happyReduce_64

action_96 (61#) = happyShift action_127
action_96 x = happyTcHack x happyFail (happyExpListPerState 96)

action_97 x = happyTcHack x happyReduce_29

action_98 (60#) = happyShift action_30
action_98 (62#) = happyShift action_31
action_98 (65#) = happyShift action_32
action_98 (67#) = happyShift action_33
action_98 (69#) = happyShift action_34
action_98 (72#) = happyShift action_35
action_98 (74#) = happyShift action_36
action_98 (78#) = happyShift action_37
action_98 (90#) = happyShift action_38
action_98 (91#) = happyShift action_39
action_98 (92#) = happyShift action_40
action_98 (93#) = happyShift action_41
action_98 (94#) = happyShift action_42
action_98 (95#) = happyShift action_43
action_98 (96#) = happyShift action_44
action_98 (97#) = happyShift action_45
action_98 (9#) = happyGoto action_126
action_98 (10#) = happyGoto action_14
action_98 (13#) = happyGoto action_15
action_98 (14#) = happyGoto action_16
action_98 (19#) = happyGoto action_17
action_98 (21#) = happyGoto action_18
action_98 (23#) = happyGoto action_19
action_98 (25#) = happyGoto action_20
action_98 (28#) = happyGoto action_21
action_98 (31#) = happyGoto action_22
action_98 (34#) = happyGoto action_23
action_98 (37#) = happyGoto action_24
action_98 (38#) = happyGoto action_25
action_98 (40#) = happyGoto action_26
action_98 (42#) = happyGoto action_27
action_98 (43#) = happyGoto action_28
action_98 (47#) = happyGoto action_29
action_98 x = happyTcHack x happyFail (happyExpListPerState 98)

action_99 (71#) = happyShift action_122
action_99 (72#) = happyShift action_123
action_99 (74#) = happyShift action_124
action_99 (90#) = happyShift action_38
action_99 (91#) = happyShift action_39
action_99 (92#) = happyShift action_40
action_99 (93#) = happyShift action_41
action_99 (94#) = happyShift action_42
action_99 (95#) = happyShift action_43
action_99 (96#) = happyShift action_125
action_99 (15#) = happyGoto action_115
action_99 (16#) = happyGoto action_116
action_99 (18#) = happyGoto action_117
action_99 (42#) = happyGoto action_118
action_99 (49#) = happyGoto action_119
action_99 (50#) = happyGoto action_120
action_99 (54#) = happyGoto action_121
action_99 x = happyTcHack x happyFail (happyExpListPerState 99)

action_100 (73#) = happyShift action_114
action_100 x = happyTcHack x happyFail (happyExpListPerState 100)

action_101 (76#) = happyShift action_113
action_101 x = happyTcHack x happyReduce_18

action_102 (60#) = happyShift action_30
action_102 (62#) = happyShift action_31
action_102 (65#) = happyShift action_32
action_102 (67#) = happyShift action_33
action_102 (69#) = happyShift action_34
action_102 (72#) = happyShift action_35
action_102 (74#) = happyShift action_36
action_102 (78#) = happyShift action_37
action_102 (90#) = happyShift action_38
action_102 (91#) = happyShift action_39
action_102 (92#) = happyShift action_40
action_102 (93#) = happyShift action_41
action_102 (94#) = happyShift action_42
action_102 (95#) = happyShift action_43
action_102 (96#) = happyShift action_44
action_102 (97#) = happyShift action_45
action_102 (9#) = happyGoto action_112
action_102 (10#) = happyGoto action_14
action_102 (13#) = happyGoto action_15
action_102 (14#) = happyGoto action_16
action_102 (19#) = happyGoto action_17
action_102 (21#) = happyGoto action_18
action_102 (23#) = happyGoto action_19
action_102 (25#) = happyGoto action_20
action_102 (28#) = happyGoto action_21
action_102 (31#) = happyGoto action_22
action_102 (34#) = happyGoto action_23
action_102 (37#) = happyGoto action_24
action_102 (38#) = happyGoto action_25
action_102 (40#) = happyGoto action_26
action_102 (42#) = happyGoto action_27
action_102 (43#) = happyGoto action_28
action_102 (47#) = happyGoto action_29
action_102 x = happyTcHack x happyFail (happyExpListPerState 102)

action_103 x = happyTcHack x happyReduce_67

action_104 (60#) = happyShift action_30
action_104 (62#) = happyShift action_31
action_104 (65#) = happyShift action_32
action_104 (67#) = happyShift action_33
action_104 (69#) = happyShift action_34
action_104 (72#) = happyShift action_35
action_104 (74#) = happyShift action_36
action_104 (78#) = happyShift action_37
action_104 (90#) = happyShift action_38
action_104 (91#) = happyShift action_39
action_104 (92#) = happyShift action_40
action_104 (93#) = happyShift action_41
action_104 (94#) = happyShift action_42
action_104 (95#) = happyShift action_43
action_104 (96#) = happyShift action_44
action_104 (97#) = happyShift action_45
action_104 (9#) = happyGoto action_111
action_104 (10#) = happyGoto action_14
action_104 (13#) = happyGoto action_15
action_104 (14#) = happyGoto action_16
action_104 (19#) = happyGoto action_17
action_104 (21#) = happyGoto action_18
action_104 (23#) = happyGoto action_19
action_104 (25#) = happyGoto action_20
action_104 (28#) = happyGoto action_21
action_104 (31#) = happyGoto action_22
action_104 (34#) = happyGoto action_23
action_104 (37#) = happyGoto action_24
action_104 (38#) = happyGoto action_25
action_104 (40#) = happyGoto action_26
action_104 (42#) = happyGoto action_27
action_104 (43#) = happyGoto action_28
action_104 (47#) = happyGoto action_29
action_104 x = happyTcHack x happyFail (happyExpListPerState 104)

action_105 x = happyTcHack x happyReduce_79

action_106 x = happyTcHack x happyReduce_82

action_107 (60#) = happyShift action_30
action_107 (62#) = happyShift action_31
action_107 (65#) = happyShift action_32
action_107 (67#) = happyShift action_33
action_107 (69#) = happyShift action_34
action_107 (72#) = happyShift action_35
action_107 (74#) = happyShift action_36
action_107 (78#) = happyShift action_37
action_107 (90#) = happyShift action_38
action_107 (91#) = happyShift action_39
action_107 (92#) = happyShift action_40
action_107 (93#) = happyShift action_41
action_107 (94#) = happyShift action_42
action_107 (95#) = happyShift action_43
action_107 (96#) = happyShift action_44
action_107 (97#) = happyShift action_45
action_107 (9#) = happyGoto action_110
action_107 (10#) = happyGoto action_14
action_107 (13#) = happyGoto action_15
action_107 (14#) = happyGoto action_16
action_107 (19#) = happyGoto action_17
action_107 (21#) = happyGoto action_18
action_107 (23#) = happyGoto action_19
action_107 (25#) = happyGoto action_20
action_107 (28#) = happyGoto action_21
action_107 (31#) = happyGoto action_22
action_107 (34#) = happyGoto action_23
action_107 (37#) = happyGoto action_24
action_107 (38#) = happyGoto action_25
action_107 (40#) = happyGoto action_26
action_107 (42#) = happyGoto action_27
action_107 (43#) = happyGoto action_28
action_107 (47#) = happyGoto action_29
action_107 x = happyTcHack x happyFail (happyExpListPerState 107)

action_108 (72#) = happyShift action_109
action_108 x = happyTcHack x happyFail (happyExpListPerState 108)

action_109 (96#) = happyShift action_149
action_109 x = happyTcHack x happyFail (happyExpListPerState 109)

action_110 (59#) = happyShift action_87
action_110 (76#) = happyShift action_107
action_110 (46#) = happyGoto action_148
action_110 x = happyTcHack x happyReduce_83

action_111 (59#) = happyShift action_87
action_111 (76#) = happyShift action_147
action_111 (48#) = happyGoto action_146
action_111 x = happyTcHack x happyReduce_86

action_112 (59#) = happyShift action_87
action_112 x = happyTcHack x happyReduce_15

action_113 (96#) = happyShift action_101
action_113 (12#) = happyGoto action_145
action_113 x = happyTcHack x happyFail (happyExpListPerState 113)

action_114 x = happyTcHack x happyReduce_17

action_115 x = happyTcHack x happyReduce_21

action_116 (58#) = happyShift action_144
action_116 (17#) = happyGoto action_143
action_116 x = happyTcHack x happyFail (happyExpListPerState 116)

action_117 (57#) = happyShift action_142
action_117 x = happyTcHack x happyReduce_23

action_118 x = happyTcHack x happyReduce_90

action_119 (59#) = happyShift action_140
action_119 (68#) = happyShift action_141
action_119 x = happyTcHack x happyFail (happyExpListPerState 119)

action_120 x = happyTcHack x happyReduce_93

action_121 x = happyTcHack x happyReduce_94

action_122 x = happyTcHack x happyReduce_88

action_123 (71#) = happyShift action_122
action_123 (72#) = happyShift action_123
action_123 (74#) = happyShift action_124
action_123 (90#) = happyShift action_38
action_123 (91#) = happyShift action_39
action_123 (92#) = happyShift action_40
action_123 (93#) = happyShift action_41
action_123 (94#) = happyShift action_42
action_123 (95#) = happyShift action_43
action_123 (96#) = happyShift action_125
action_123 (42#) = happyGoto action_118
action_123 (49#) = happyGoto action_139
action_123 (50#) = happyGoto action_120
action_123 (54#) = happyGoto action_121
action_123 x = happyTcHack x happyFail (happyExpListPerState 123)

action_124 (71#) = happyShift action_122
action_124 (72#) = happyShift action_123
action_124 (74#) = happyShift action_124
action_124 (90#) = happyShift action_38
action_124 (91#) = happyShift action_39
action_124 (92#) = happyShift action_40
action_124 (93#) = happyShift action_41
action_124 (94#) = happyShift action_42
action_124 (95#) = happyShift action_43
action_124 (96#) = happyShift action_125
action_124 (42#) = happyGoto action_118
action_124 (49#) = happyGoto action_136
action_124 (50#) = happyGoto action_120
action_124 (51#) = happyGoto action_137
action_124 (52#) = happyGoto action_138
action_124 (54#) = happyGoto action_121
action_124 x = happyTcHack x happyReduce_96

action_125 x = happyTcHack x happyReduce_89

action_126 (59#) = happyShift action_87
action_126 (64#) = happyShift action_135
action_126 x = happyTcHack x happyFail (happyExpListPerState 126)

action_127 (60#) = happyShift action_30
action_127 (62#) = happyShift action_31
action_127 (65#) = happyShift action_32
action_127 (67#) = happyShift action_33
action_127 (69#) = happyShift action_34
action_127 (72#) = happyShift action_35
action_127 (74#) = happyShift action_36
action_127 (78#) = happyShift action_37
action_127 (90#) = happyShift action_38
action_127 (91#) = happyShift action_39
action_127 (92#) = happyShift action_40
action_127 (93#) = happyShift action_41
action_127 (94#) = happyShift action_42
action_127 (95#) = happyShift action_43
action_127 (96#) = happyShift action_44
action_127 (97#) = happyShift action_45
action_127 (9#) = happyGoto action_134
action_127 (10#) = happyGoto action_14
action_127 (13#) = happyGoto action_15
action_127 (14#) = happyGoto action_16
action_127 (19#) = happyGoto action_17
action_127 (21#) = happyGoto action_18
action_127 (23#) = happyGoto action_19
action_127 (25#) = happyGoto action_20
action_127 (28#) = happyGoto action_21
action_127 (31#) = happyGoto action_22
action_127 (34#) = happyGoto action_23
action_127 (37#) = happyGoto action_24
action_127 (38#) = happyGoto action_25
action_127 (40#) = happyGoto action_26
action_127 (42#) = happyGoto action_27
action_127 (43#) = happyGoto action_28
action_127 (47#) = happyGoto action_29
action_127 x = happyTcHack x happyFail (happyExpListPerState 127)

action_128 x = happyTcHack x happyReduce_55

action_129 x = happyTcHack x happyReduce_50

action_130 x = happyTcHack x happyReduce_43

action_131 x = happyTcHack x happyReduce_38

action_132 x = happyTcHack x happyReduce_35

action_133 x = happyTcHack x happyReduce_32

action_134 (59#) = happyShift action_87
action_134 x = happyTcHack x happyReduce_27

action_135 (60#) = happyShift action_30
action_135 (62#) = happyShift action_31
action_135 (65#) = happyShift action_32
action_135 (67#) = happyShift action_33
action_135 (69#) = happyShift action_34
action_135 (72#) = happyShift action_35
action_135 (74#) = happyShift action_36
action_135 (78#) = happyShift action_37
action_135 (90#) = happyShift action_38
action_135 (91#) = happyShift action_39
action_135 (92#) = happyShift action_40
action_135 (93#) = happyShift action_41
action_135 (94#) = happyShift action_42
action_135 (95#) = happyShift action_43
action_135 (96#) = happyShift action_44
action_135 (97#) = happyShift action_45
action_135 (9#) = happyGoto action_161
action_135 (10#) = happyGoto action_14
action_135 (13#) = happyGoto action_15
action_135 (14#) = happyGoto action_16
action_135 (19#) = happyGoto action_17
action_135 (21#) = happyGoto action_18
action_135 (23#) = happyGoto action_19
action_135 (25#) = happyGoto action_20
action_135 (28#) = happyGoto action_21
action_135 (31#) = happyGoto action_22
action_135 (34#) = happyGoto action_23
action_135 (37#) = happyGoto action_24
action_135 (38#) = happyGoto action_25
action_135 (40#) = happyGoto action_26
action_135 (42#) = happyGoto action_27
action_135 (43#) = happyGoto action_28
action_135 (47#) = happyGoto action_29
action_135 x = happyTcHack x happyFail (happyExpListPerState 135)

action_136 (59#) = happyShift action_140
action_136 (76#) = happyShift action_160
action_136 (53#) = happyGoto action_159
action_136 x = happyTcHack x happyReduce_99

action_137 (75#) = happyShift action_158
action_137 x = happyTcHack x happyFail (happyExpListPerState 137)

action_138 x = happyTcHack x happyReduce_97

action_139 (59#) = happyShift action_140
action_139 (73#) = happyShift action_156
action_139 (76#) = happyShift action_157
action_139 x = happyTcHack x happyFail (happyExpListPerState 139)

action_140 (71#) = happyShift action_122
action_140 (72#) = happyShift action_123
action_140 (74#) = happyShift action_124
action_140 (90#) = happyShift action_38
action_140 (91#) = happyShift action_39
action_140 (92#) = happyShift action_40
action_140 (93#) = happyShift action_41
action_140 (94#) = happyShift action_42
action_140 (95#) = happyShift action_43
action_140 (96#) = happyShift action_125
action_140 (42#) = happyGoto action_118
action_140 (49#) = happyGoto action_155
action_140 (50#) = happyGoto action_120
action_140 (54#) = happyGoto action_121
action_140 x = happyTcHack x happyFail (happyExpListPerState 140)

action_141 (60#) = happyShift action_30
action_141 (62#) = happyShift action_31
action_141 (65#) = happyShift action_32
action_141 (67#) = happyShift action_33
action_141 (69#) = happyShift action_34
action_141 (72#) = happyShift action_35
action_141 (74#) = happyShift action_36
action_141 (78#) = happyShift action_37
action_141 (90#) = happyShift action_38
action_141 (91#) = happyShift action_39
action_141 (92#) = happyShift action_40
action_141 (93#) = happyShift action_41
action_141 (94#) = happyShift action_42
action_141 (95#) = happyShift action_43
action_141 (96#) = happyShift action_44
action_141 (97#) = happyShift action_45
action_141 (9#) = happyGoto action_154
action_141 (10#) = happyGoto action_14
action_141 (13#) = happyGoto action_15
action_141 (14#) = happyGoto action_16
action_141 (19#) = happyGoto action_17
action_141 (21#) = happyGoto action_18
action_141 (23#) = happyGoto action_19
action_141 (25#) = happyGoto action_20
action_141 (28#) = happyGoto action_21
action_141 (31#) = happyGoto action_22
action_141 (34#) = happyGoto action_23
action_141 (37#) = happyGoto action_24
action_141 (38#) = happyGoto action_25
action_141 (40#) = happyGoto action_26
action_141 (42#) = happyGoto action_27
action_141 (43#) = happyGoto action_28
action_141 (47#) = happyGoto action_29
action_141 x = happyTcHack x happyFail (happyExpListPerState 141)

action_142 (71#) = happyShift action_122
action_142 (72#) = happyShift action_123
action_142 (74#) = happyShift action_124
action_142 (90#) = happyShift action_38
action_142 (91#) = happyShift action_39
action_142 (92#) = happyShift action_40
action_142 (93#) = happyShift action_41
action_142 (94#) = happyShift action_42
action_142 (95#) = happyShift action_43
action_142 (96#) = happyShift action_125
action_142 (16#) = happyGoto action_153
action_142 (18#) = happyGoto action_117
action_142 (42#) = happyGoto action_118
action_142 (49#) = happyGoto action_119
action_142 (50#) = happyGoto action_120
action_142 (54#) = happyGoto action_121
action_142 x = happyTcHack x happyFail (happyExpListPerState 142)

action_143 x = happyTcHack x happyReduce_22

action_144 x = happyTcHack x happyReduce_25

action_145 x = happyTcHack x happyReduce_19

action_146 (73#) = happyShift action_152
action_146 x = happyTcHack x happyFail (happyExpListPerState 146)

action_147 (60#) = happyShift action_30
action_147 (62#) = happyShift action_31
action_147 (65#) = happyShift action_32
action_147 (67#) = happyShift action_33
action_147 (69#) = happyShift action_34
action_147 (72#) = happyShift action_35
action_147 (74#) = happyShift action_36
action_147 (78#) = happyShift action_37
action_147 (90#) = happyShift action_38
action_147 (91#) = happyShift action_39
action_147 (92#) = happyShift action_40
action_147 (93#) = happyShift action_41
action_147 (94#) = happyShift action_42
action_147 (95#) = happyShift action_43
action_147 (96#) = happyShift action_44
action_147 (97#) = happyShift action_45
action_147 (9#) = happyGoto action_151
action_147 (10#) = happyGoto action_14
action_147 (13#) = happyGoto action_15
action_147 (14#) = happyGoto action_16
action_147 (19#) = happyGoto action_17
action_147 (21#) = happyGoto action_18
action_147 (23#) = happyGoto action_19
action_147 (25#) = happyGoto action_20
action_147 (28#) = happyGoto action_21
action_147 (31#) = happyGoto action_22
action_147 (34#) = happyGoto action_23
action_147 (37#) = happyGoto action_24
action_147 (38#) = happyGoto action_25
action_147 (40#) = happyGoto action_26
action_147 (42#) = happyGoto action_27
action_147 (43#) = happyGoto action_28
action_147 (47#) = happyGoto action_29
action_147 x = happyTcHack x happyFail (happyExpListPerState 147)

action_148 x = happyTcHack x happyReduce_84

action_149 (73#) = happyShift action_150
action_149 x = happyTcHack x happyFail (happyExpListPerState 149)

action_150 (101#) = happyShift action_165
action_150 x = happyTcHack x happyFail (happyExpListPerState 150)

action_151 (59#) = happyShift action_87
action_151 (76#) = happyShift action_147
action_151 (48#) = happyGoto action_164
action_151 x = happyTcHack x happyReduce_86

action_152 x = happyTcHack x happyReduce_85

action_153 x = happyTcHack x happyReduce_24

action_154 (59#) = happyShift action_87
action_154 x = happyTcHack x happyReduce_26

action_155 (59#) = happyShift action_140
action_155 x = happyTcHack x happyReduce_92

action_156 x = happyTcHack x happyReduce_91

action_157 (71#) = happyShift action_122
action_157 (72#) = happyShift action_123
action_157 (74#) = happyShift action_124
action_157 (90#) = happyShift action_38
action_157 (91#) = happyShift action_39
action_157 (92#) = happyShift action_40
action_157 (93#) = happyShift action_41
action_157 (94#) = happyShift action_42
action_157 (95#) = happyShift action_43
action_157 (96#) = happyShift action_125
action_157 (42#) = happyGoto action_118
action_157 (49#) = happyGoto action_163
action_157 (50#) = happyGoto action_120
action_157 (54#) = happyGoto action_121
action_157 x = happyTcHack x happyFail (happyExpListPerState 157)

action_158 x = happyTcHack x happyReduce_95

action_159 x = happyTcHack x happyReduce_98

action_160 (71#) = happyShift action_122
action_160 (72#) = happyShift action_123
action_160 (74#) = happyShift action_124
action_160 (90#) = happyShift action_38
action_160 (91#) = happyShift action_39
action_160 (92#) = happyShift action_40
action_160 (93#) = happyShift action_41
action_160 (94#) = happyShift action_42
action_160 (95#) = happyShift action_43
action_160 (96#) = happyShift action_125
action_160 (42#) = happyGoto action_118
action_160 (49#) = happyGoto action_162
action_160 (50#) = happyGoto action_120
action_160 (54#) = happyGoto action_121
action_160 x = happyTcHack x happyFail (happyExpListPerState 160)

action_161 (59#) = happyShift action_87
action_161 x = happyTcHack x happyReduce_20

action_162 (59#) = happyShift action_140
action_162 (76#) = happyShift action_160
action_162 (53#) = happyGoto action_169
action_162 x = happyTcHack x happyReduce_99

action_163 (59#) = happyShift action_140
action_163 (76#) = happyShift action_168
action_163 (55#) = happyGoto action_167
action_163 x = happyTcHack x happyReduce_102

action_164 x = happyTcHack x happyReduce_87

action_165 (72#) = happyShift action_166
action_165 x = happyTcHack x happyFail (happyExpListPerState 165)

action_166 (96#) = happyShift action_172
action_166 x = happyTcHack x happyFail (happyExpListPerState 166)

action_167 (73#) = happyShift action_171
action_167 x = happyTcHack x happyFail (happyExpListPerState 167)

action_168 (71#) = happyShift action_122
action_168 (72#) = happyShift action_123
action_168 (74#) = happyShift action_124
action_168 (90#) = happyShift action_38
action_168 (91#) = happyShift action_39
action_168 (92#) = happyShift action_40
action_168 (93#) = happyShift action_41
action_168 (94#) = happyShift action_42
action_168 (95#) = happyShift action_43
action_168 (96#) = happyShift action_125
action_168 (42#) = happyGoto action_118
action_168 (49#) = happyGoto action_170
action_168 (50#) = happyGoto action_120
action_168 (54#) = happyGoto action_121
action_168 x = happyTcHack x happyFail (happyExpListPerState 168)

action_169 x = happyTcHack x happyReduce_100

action_170 (59#) = happyShift action_140
action_170 (76#) = happyShift action_168
action_170 (55#) = happyGoto action_174
action_170 x = happyTcHack x happyReduce_102

action_171 x = happyTcHack x happyReduce_101

action_172 (73#) = happyShift action_173
action_172 x = happyTcHack x happyFail (happyExpListPerState 172)

action_173 (56#) = happyShift action_175
action_173 x = happyTcHack x happyFail (happyExpListPerState 173)

action_174 x = happyTcHack x happyReduce_103

action_175 x = happyTcHack x happyReduce_70

happyReduce_1 = happySpecReduce_1  4# happyReduction_1
happyReduction_1 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (Program (reverse happy_var_1)
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_2  5# happyReduction_2
happyReduction_2 (HappyAbsSyn5  happy_var_2)
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1 : happy_var_2
	)
happyReduction_2 _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_0  6# happyReduction_3
happyReduction_3  =  HappyAbsSyn5
		 ([]
	)

happyReduce_4 = happySpecReduce_1  6# happyReduction_4
happyReduction_4 _
	 =  HappyAbsSyn5
		 ([]
	)

happyReduce_5 = happySpecReduce_3  6# happyReduction_5
happyReduction_5 (HappyAbsSyn5  happy_var_3)
	(HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (happy_var_2 : happy_var_3
	)
happyReduction_5 _ _ _  = notHappyAtAll 

happyReduce_6 = happyReduce 4# 7# happyReduction_6
happyReduction_6 ((HappyAbsSyn9  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_2) `HappyStk`
	(HappyTerminal (TokenIdent happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (FunDecl happy_var_1 happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_7 = happySpecReduce_0  8# happyReduction_7
happyReduction_7  =  HappyAbsSyn8
		 ([]
	)

happyReduce_8 = happySpecReduce_2  8# happyReduction_8
happyReduction_8 (HappyAbsSyn8  happy_var_2)
	(HappyTerminal (TokenIdent happy_var_1))
	 =  HappyAbsSyn8
		 (happy_var_1 : happy_var_2
	)
happyReduction_8 _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_1  9# happyReduction_9
happyReduction_9 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1
	)
happyReduction_9 _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_1  9# happyReduction_10
happyReduction_10 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1
	)
happyReduction_10 _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_1  9# happyReduction_11
happyReduction_11 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1
	)
happyReduction_11 _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_1  9# happyReduction_12
happyReduction_12 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  9# happyReduction_13
happyReduction_13 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_3  9# happyReduction_14
happyReduction_14 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (Cons happy_var_1 happy_var_3
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happyReduce 4# 10# happyReduction_15
happyReduction_15 ((HappyAbsSyn9  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (Lambda happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_16 = happySpecReduce_1  11# happyReduction_16
happyReduction_16 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_3  11# happyReduction_17
happyReduction_17 _
	(HappyAbsSyn8  happy_var_2)
	_
	 =  HappyAbsSyn8
		 (happy_var_2
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_1  12# happyReduction_18
happyReduction_18 (HappyTerminal (TokenIdent happy_var_1))
	 =  HappyAbsSyn8
		 ([happy_var_1]
	)
happyReduction_18 _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  12# happyReduction_19
happyReduction_19 (HappyAbsSyn8  happy_var_3)
	_
	(HappyTerminal (TokenIdent happy_var_1))
	 =  HappyAbsSyn8
		 (happy_var_1 : happy_var_3
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happyReduce 6# 13# happyReduction_20
happyReduction_20 ((HappyAbsSyn9  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (If happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_21 = happyReduce 4# 14# happyReduction_21
happyReduction_21 ((HappyAbsSyn15  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (Case happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_22 = happySpecReduce_2  15# happyReduction_22
happyReduction_22 _
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (happy_var_1
	)
happyReduction_22 _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_1  16# happyReduction_23
happyReduction_23 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn15
		 ([happy_var_1]
	)
happyReduction_23 _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_3  16# happyReduction_24
happyReduction_24 (HappyAbsSyn15  happy_var_3)
	_
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn15
		 (happy_var_1 : happy_var_3
	)
happyReduction_24 _ _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_1  17# happyReduction_25
happyReduction_25 _
	 =  HappyAbsSyn17
		 (()
	)

happyReduce_26 = happySpecReduce_3  18# happyReduction_26
happyReduction_26 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn49  happy_var_1)
	 =  HappyAbsSyn18
		 ((happy_var_1,happy_var_3)
	)
happyReduction_26 _ _ _  = notHappyAtAll 

happyReduce_27 = happyReduce 5# 19# happyReduction_27
happyReduction_27 ((HappyAbsSyn9  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (Let (reverse happy_var_2) happy_var_5
	) `HappyStk` happyRest

happyReduce_28 = happySpecReduce_0  20# happyReduction_28
happyReduction_28  =  HappyAbsSyn17
		 (()
	)

happyReduce_29 = happySpecReduce_1  20# happyReduction_29
happyReduction_29 _
	 =  HappyAbsSyn17
		 (()
	)

happyReduce_30 = happySpecReduce_2  21# happyReduction_30
happyReduction_30 (HappyAbsSyn22  happy_var_2)
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (foldl (BinOp Or) happy_var_1 happy_var_2
	)
happyReduction_30 _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_0  22# happyReduction_31
happyReduction_31  =  HappyAbsSyn22
		 ([]
	)

happyReduce_32 = happySpecReduce_3  22# happyReduction_32
happyReduction_32 (HappyAbsSyn22  happy_var_3)
	(HappyAbsSyn9  happy_var_2)
	_
	 =  HappyAbsSyn22
		 (happy_var_2 : happy_var_3
	)
happyReduction_32 _ _ _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_2  23# happyReduction_33
happyReduction_33 (HappyAbsSyn22  happy_var_2)
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (foldl (BinOp And) happy_var_1 happy_var_2
	)
happyReduction_33 _ _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_0  24# happyReduction_34
happyReduction_34  =  HappyAbsSyn22
		 ([]
	)

happyReduce_35 = happySpecReduce_3  24# happyReduction_35
happyReduction_35 (HappyAbsSyn22  happy_var_3)
	(HappyAbsSyn9  happy_var_2)
	_
	 =  HappyAbsSyn22
		 (happy_var_2 : happy_var_3
	)
happyReduction_35 _ _ _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_2  25# happyReduction_36
happyReduction_36 (HappyAbsSyn26  happy_var_2)
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (foldl (\e1 (op,e2) -> BinOp op e1 e2) happy_var_1 happy_var_2
	)
happyReduction_36 _ _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_0  26# happyReduction_37
happyReduction_37  =  HappyAbsSyn26
		 ([]
	)

happyReduce_38 = happySpecReduce_3  26# happyReduction_38
happyReduction_38 (HappyAbsSyn26  happy_var_3)
	(HappyAbsSyn9  happy_var_2)
	(HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn26
		 ((happy_var_1,happy_var_2) : happy_var_3
	)
happyReduction_38 _ _ _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_1  27# happyReduction_39
happyReduction_39 _
	 =  HappyAbsSyn27
		 (Eq
	)

happyReduce_40 = happySpecReduce_1  27# happyReduction_40
happyReduction_40 _
	 =  HappyAbsSyn27
		 (Neq
	)

happyReduce_41 = happySpecReduce_2  28# happyReduction_41
happyReduction_41 (HappyAbsSyn26  happy_var_2)
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (foldl (\e1 (op,e2) -> BinOp op e1 e2) happy_var_1 happy_var_2
	)
happyReduction_41 _ _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_0  29# happyReduction_42
happyReduction_42  =  HappyAbsSyn26
		 ([]
	)

happyReduce_43 = happySpecReduce_3  29# happyReduction_43
happyReduction_43 (HappyAbsSyn26  happy_var_3)
	(HappyAbsSyn9  happy_var_2)
	(HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn26
		 ((happy_var_1,happy_var_2) : happy_var_3
	)
happyReduction_43 _ _ _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_1  30# happyReduction_44
happyReduction_44 _
	 =  HappyAbsSyn27
		 (Lt
	)

happyReduce_45 = happySpecReduce_1  30# happyReduction_45
happyReduction_45 _
	 =  HappyAbsSyn27
		 (Le
	)

happyReduce_46 = happySpecReduce_1  30# happyReduction_46
happyReduction_46 _
	 =  HappyAbsSyn27
		 (Gt
	)

happyReduce_47 = happySpecReduce_1  30# happyReduction_47
happyReduction_47 _
	 =  HappyAbsSyn27
		 (Ge
	)

happyReduce_48 = happySpecReduce_2  31# happyReduction_48
happyReduction_48 (HappyAbsSyn26  happy_var_2)
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (foldl (\e1 (op,e2) -> BinOp op e1 e2) happy_var_1 happy_var_2
	)
happyReduction_48 _ _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_0  32# happyReduction_49
happyReduction_49  =  HappyAbsSyn26
		 ([]
	)

happyReduce_50 = happySpecReduce_3  32# happyReduction_50
happyReduction_50 (HappyAbsSyn26  happy_var_3)
	(HappyAbsSyn9  happy_var_2)
	(HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn26
		 ((happy_var_1,happy_var_2) : happy_var_3
	)
happyReduction_50 _ _ _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_1  33# happyReduction_51
happyReduction_51 _
	 =  HappyAbsSyn27
		 (Add
	)

happyReduce_52 = happySpecReduce_1  33# happyReduction_52
happyReduction_52 _
	 =  HappyAbsSyn27
		 (Sub
	)

happyReduce_53 = happySpecReduce_2  34# happyReduction_53
happyReduction_53 (HappyAbsSyn26  happy_var_2)
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (foldl (\e1 (op,e2) -> BinOp op e1 e2) happy_var_1 happy_var_2
	)
happyReduction_53 _ _  = notHappyAtAll 

happyReduce_54 = happySpecReduce_0  35# happyReduction_54
happyReduction_54  =  HappyAbsSyn26
		 ([]
	)

happyReduce_55 = happySpecReduce_3  35# happyReduction_55
happyReduction_55 (HappyAbsSyn26  happy_var_3)
	(HappyAbsSyn9  happy_var_2)
	(HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn26
		 ((happy_var_1,happy_var_2) : happy_var_3
	)
happyReduction_55 _ _ _  = notHappyAtAll 

happyReduce_56 = happySpecReduce_1  36# happyReduction_56
happyReduction_56 _
	 =  HappyAbsSyn27
		 (Mul
	)

happyReduce_57 = happySpecReduce_1  36# happyReduction_57
happyReduction_57 _
	 =  HappyAbsSyn27
		 (Div
	)

happyReduce_58 = happySpecReduce_1  36# happyReduction_58
happyReduction_58 _
	 =  HappyAbsSyn27
		 (Mod
	)

happyReduce_59 = happySpecReduce_2  37# happyReduction_59
happyReduction_59 (HappyAbsSyn9  happy_var_2)
	_
	 =  HappyAbsSyn9
		 (UnOp Not happy_var_2
	)
happyReduction_59 _ _  = notHappyAtAll 

happyReduce_60 = happySpecReduce_2  37# happyReduction_60
happyReduction_60 (HappyAbsSyn9  happy_var_2)
	_
	 =  HappyAbsSyn9
		 (UnOp Neg happy_var_2
	)
happyReduction_60 _ _  = notHappyAtAll 

happyReduce_61 = happySpecReduce_1  37# happyReduction_61
happyReduction_61 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1
	)
happyReduction_61 _  = notHappyAtAll 

happyReduce_62 = happySpecReduce_2  38# happyReduction_62
happyReduction_62 (HappyAbsSyn22  happy_var_2)
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (foldl App happy_var_1 happy_var_2
	)
happyReduction_62 _ _  = notHappyAtAll 

happyReduce_63 = happySpecReduce_0  39# happyReduction_63
happyReduction_63  =  HappyAbsSyn22
		 ([]
	)

happyReduce_64 = happySpecReduce_2  39# happyReduction_64
happyReduction_64 (HappyAbsSyn22  happy_var_2)
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_1 : happy_var_2
	)
happyReduction_64 _ _  = notHappyAtAll 

happyReduce_65 = happySpecReduce_1  40# happyReduction_65
happyReduction_65 (HappyAbsSyn42  happy_var_1)
	 =  HappyAbsSyn9
		 (Lit happy_var_1
	)
happyReduction_65 _  = notHappyAtAll 

happyReduce_66 = happySpecReduce_1  40# happyReduction_66
happyReduction_66 (HappyTerminal (TokenIdent happy_var_1))
	 =  HappyAbsSyn9
		 (Var happy_var_1
	)
happyReduction_66 _  = notHappyAtAll 

happyReduce_67 = happySpecReduce_3  40# happyReduction_67
happyReduction_67 _
	(HappyAbsSyn9  happy_var_2)
	_
	 =  HappyAbsSyn9
		 (happy_var_2
	)
happyReduction_67 _ _ _  = notHappyAtAll 

happyReduce_68 = happySpecReduce_1  40# happyReduction_68
happyReduction_68 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1
	)
happyReduction_68 _  = notHappyAtAll 

happyReduce_69 = happySpecReduce_1  40# happyReduction_69
happyReduction_69 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1
	)
happyReduction_69 _  = notHappyAtAll 

happyReduce_70 = happyReduce 11# 40# happyReduction_70
happyReduction_70 ((HappyTerminal (TokenSuperBody happy_var_11)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenIdent happy_var_9)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenIdent happy_var_5)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn41  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (Super "" happy_var_2 happy_var_5 happy_var_9 happy_var_11
	) `HappyStk` happyRest

happyReduce_71 = happySpecReduce_1  41# happyReduction_71
happyReduction_71 _
	 =  HappyAbsSyn41
		 (SuperSingle
	)

happyReduce_72 = happySpecReduce_1  41# happyReduction_72
happyReduction_72 _
	 =  HappyAbsSyn41
		 (SuperParallel
	)

happyReduce_73 = happySpecReduce_1  42# happyReduction_73
happyReduction_73 (HappyTerminal (TokenInt happy_var_1))
	 =  HappyAbsSyn42
		 (LInt happy_var_1
	)
happyReduction_73 _  = notHappyAtAll 

happyReduce_74 = happySpecReduce_1  42# happyReduction_74
happyReduction_74 (HappyTerminal (TokenFloat happy_var_1))
	 =  HappyAbsSyn42
		 (LFloat happy_var_1
	)
happyReduction_74 _  = notHappyAtAll 

happyReduce_75 = happySpecReduce_1  42# happyReduction_75
happyReduction_75 (HappyTerminal (TokenChar happy_var_1))
	 =  HappyAbsSyn42
		 (LChar happy_var_1
	)
happyReduction_75 _  = notHappyAtAll 

happyReduce_76 = happySpecReduce_1  42# happyReduction_76
happyReduction_76 (HappyTerminal (TokenString happy_var_1))
	 =  HappyAbsSyn42
		 (LString happy_var_1
	)
happyReduction_76 _  = notHappyAtAll 

happyReduce_77 = happySpecReduce_1  42# happyReduction_77
happyReduction_77 _
	 =  HappyAbsSyn42
		 (LBool True
	)

happyReduce_78 = happySpecReduce_1  42# happyReduction_78
happyReduction_78 _
	 =  HappyAbsSyn42
		 (LBool False
	)

happyReduce_79 = happySpecReduce_3  43# happyReduction_79
happyReduction_79 _
	(HappyAbsSyn22  happy_var_2)
	_
	 =  HappyAbsSyn9
		 (List happy_var_2
	)
happyReduction_79 _ _ _  = notHappyAtAll 

happyReduce_80 = happySpecReduce_0  44# happyReduction_80
happyReduction_80  =  HappyAbsSyn22
		 ([]
	)

happyReduce_81 = happySpecReduce_1  44# happyReduction_81
happyReduction_81 (HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_1
	)
happyReduction_81 _  = notHappyAtAll 

happyReduce_82 = happySpecReduce_2  45# happyReduction_82
happyReduction_82 (HappyAbsSyn22  happy_var_2)
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_1 : happy_var_2
	)
happyReduction_82 _ _  = notHappyAtAll 

happyReduce_83 = happySpecReduce_0  46# happyReduction_83
happyReduction_83  =  HappyAbsSyn22
		 ([]
	)

happyReduce_84 = happySpecReduce_3  46# happyReduction_84
happyReduction_84 (HappyAbsSyn22  happy_var_3)
	(HappyAbsSyn9  happy_var_2)
	_
	 =  HappyAbsSyn22
		 (happy_var_2 : happy_var_3
	)
happyReduction_84 _ _ _  = notHappyAtAll 

happyReduce_85 = happyReduce 6# 47# happyReduction_85
happyReduction_85 (_ `HappyStk`
	(HappyAbsSyn22  happy_var_5) `HappyStk`
	(HappyAbsSyn9  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (Tuple (happy_var_2 : happy_var_4 : happy_var_5)
	) `HappyStk` happyRest

happyReduce_86 = happySpecReduce_0  48# happyReduction_86
happyReduction_86  =  HappyAbsSyn22
		 ([]
	)

happyReduce_87 = happySpecReduce_3  48# happyReduction_87
happyReduction_87 (HappyAbsSyn22  happy_var_3)
	(HappyAbsSyn9  happy_var_2)
	_
	 =  HappyAbsSyn22
		 (happy_var_2 : happy_var_3
	)
happyReduction_87 _ _ _  = notHappyAtAll 

happyReduce_88 = happySpecReduce_1  49# happyReduction_88
happyReduction_88 _
	 =  HappyAbsSyn49
		 (PWildcard
	)

happyReduce_89 = happySpecReduce_1  49# happyReduction_89
happyReduction_89 (HappyTerminal (TokenIdent happy_var_1))
	 =  HappyAbsSyn49
		 (PVar happy_var_1
	)
happyReduction_89 _  = notHappyAtAll 

happyReduce_90 = happySpecReduce_1  49# happyReduction_90
happyReduction_90 (HappyAbsSyn42  happy_var_1)
	 =  HappyAbsSyn49
		 (PLit happy_var_1
	)
happyReduction_90 _  = notHappyAtAll 

happyReduce_91 = happySpecReduce_3  49# happyReduction_91
happyReduction_91 _
	(HappyAbsSyn49  happy_var_2)
	_
	 =  HappyAbsSyn49
		 (happy_var_2
	)
happyReduction_91 _ _ _  = notHappyAtAll 

happyReduce_92 = happySpecReduce_3  49# happyReduction_92
happyReduction_92 (HappyAbsSyn49  happy_var_3)
	_
	(HappyAbsSyn49  happy_var_1)
	 =  HappyAbsSyn49
		 (PCons happy_var_1 happy_var_3
	)
happyReduction_92 _ _ _  = notHappyAtAll 

happyReduce_93 = happySpecReduce_1  49# happyReduction_93
happyReduction_93 (HappyAbsSyn49  happy_var_1)
	 =  HappyAbsSyn49
		 (happy_var_1
	)
happyReduction_93 _  = notHappyAtAll 

happyReduce_94 = happySpecReduce_1  49# happyReduction_94
happyReduction_94 (HappyAbsSyn49  happy_var_1)
	 =  HappyAbsSyn49
		 (happy_var_1
	)
happyReduction_94 _  = notHappyAtAll 

happyReduce_95 = happySpecReduce_3  50# happyReduction_95
happyReduction_95 _
	(HappyAbsSyn51  happy_var_2)
	_
	 =  HappyAbsSyn49
		 (PList happy_var_2
	)
happyReduction_95 _ _ _  = notHappyAtAll 

happyReduce_96 = happySpecReduce_0  51# happyReduction_96
happyReduction_96  =  HappyAbsSyn51
		 ([]
	)

happyReduce_97 = happySpecReduce_1  51# happyReduction_97
happyReduction_97 (HappyAbsSyn51  happy_var_1)
	 =  HappyAbsSyn51
		 (happy_var_1
	)
happyReduction_97 _  = notHappyAtAll 

happyReduce_98 = happySpecReduce_2  52# happyReduction_98
happyReduction_98 (HappyAbsSyn51  happy_var_2)
	(HappyAbsSyn49  happy_var_1)
	 =  HappyAbsSyn51
		 (happy_var_1 : happy_var_2
	)
happyReduction_98 _ _  = notHappyAtAll 

happyReduce_99 = happySpecReduce_0  53# happyReduction_99
happyReduction_99  =  HappyAbsSyn51
		 ([]
	)

happyReduce_100 = happySpecReduce_3  53# happyReduction_100
happyReduction_100 (HappyAbsSyn51  happy_var_3)
	(HappyAbsSyn49  happy_var_2)
	_
	 =  HappyAbsSyn51
		 (happy_var_2 : happy_var_3
	)
happyReduction_100 _ _ _  = notHappyAtAll 

happyReduce_101 = happyReduce 6# 54# happyReduction_101
happyReduction_101 (_ `HappyStk`
	(HappyAbsSyn51  happy_var_5) `HappyStk`
	(HappyAbsSyn49  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn49  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn49
		 (PTuple (happy_var_2 : happy_var_4 : happy_var_5)
	) `HappyStk` happyRest

happyReduce_102 = happySpecReduce_0  55# happyReduction_102
happyReduction_102  =  HappyAbsSyn51
		 ([]
	)

happyReduce_103 = happySpecReduce_3  55# happyReduction_103
happyReduction_103 (HappyAbsSyn51  happy_var_3)
	(HappyAbsSyn49  happy_var_2)
	_
	 =  HappyAbsSyn51
		 (happy_var_2 : happy_var_3
	)
happyReduction_103 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 102# 102# notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenSuperBody happy_dollar_dollar -> cont 56#;
	TokenSemi -> cont 57#;
	TokenLayoutEnd -> cont 58#;
	TokenColon -> cont 59#;
	TokenLet -> cont 60#;
	TokenIn -> cont 61#;
	TokenIf -> cont 62#;
	TokenThen -> cont 63#;
	TokenElse -> cont 64#;
	TokenCase -> cont 65#;
	TokenOf -> cont 66#;
	TokenNot -> cont 67#;
	TokenArrow -> cont 68#;
	TokenBackslash -> cont 69#;
	TokenEquals -> cont 70#;
	TokenUnderscore -> cont 71#;
	TokenLParen -> cont 72#;
	TokenRParen -> cont 73#;
	TokenLBracket -> cont 74#;
	TokenRBracket -> cont 75#;
	TokenComma -> cont 76#;
	TokenPlus -> cont 77#;
	TokenMinus -> cont 78#;
	TokenTimes -> cont 79#;
	TokenDiv -> cont 80#;
	TokenMod -> cont 81#;
	TokenEq -> cont 82#;
	TokenNeq -> cont 83#;
	TokenLt -> cont 84#;
	TokenLe -> cont 85#;
	TokenGt -> cont 86#;
	TokenGe -> cont 87#;
	TokenAnd -> cont 88#;
	TokenOr -> cont 89#;
	TokenInt happy_dollar_dollar -> cont 90#;
	TokenFloat happy_dollar_dollar -> cont 91#;
	TokenChar happy_dollar_dollar -> cont 92#;
	TokenString happy_dollar_dollar -> cont 93#;
	TokenBool True -> cont 94#;
	TokenBool False -> cont 95#;
	TokenIdent happy_dollar_dollar -> cont 96#;
	TokenSuper -> cont 97#;
	TokenSingle -> cont 98#;
	TokenParallel -> cont 99#;
	TokenInput -> cont 100#;
	TokenOutput -> cont 101#;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 102# tk tks = happyError' (tks, explist)
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
parse tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> a
parseError (t:_) = error ("parse error: unexpected token " ++ show t)
parseError []    = error "parse error: empty input"
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $













-- Do not remove this comment. Required to fix CPP parsing when using GCC and a clang-compiled alex.
#if __GLASGOW_HASKELL__ > 706
#define LT(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.<# m)) :: Prelude.Bool)
#define GTE(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.>=# m)) :: Prelude.Bool)
#define EQ(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.==# m)) :: Prelude.Bool)
#else
#define LT(n,m) (n Happy_GHC_Exts.<# m)
#define GTE(n,m) (n Happy_GHC_Exts.>=# m)
#define EQ(n,m) (n Happy_GHC_Exts.==# m)
#endif



















data Happy_IntList = HappyCons Happy_GHC_Exts.Int# Happy_IntList








































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
happyAccept 1# tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
        (happyTcHack j ) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

































indexShortOffAddr (HappyA# arr) off =
        Happy_GHC_Exts.narrow16Int# i
  where
        i = Happy_GHC_Exts.word2Int# (Happy_GHC_Exts.or# (Happy_GHC_Exts.uncheckedShiftL# high 8#) low)
        high = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr (off' Happy_GHC_Exts.+# 1#)))
        low  = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr off'))
        off' = off Happy_GHC_Exts.*# 2#




{-# INLINE happyLt #-}
happyLt x y = LT(x,y)


readArrayBit arr bit =
    Bits.testBit (Happy_GHC_Exts.I# (indexShortOffAddr arr ((unbox_int bit) `Happy_GHC_Exts.iShiftRA#` 4#))) (bit `Prelude.mod` 16)
  where unbox_int (Happy_GHC_Exts.I# x) = x






data HappyAddr = HappyA# Happy_GHC_Exts.Addr#


-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Happy_GHC_Exts.Int# ->                    -- token number
         Happy_GHC_Exts.Int# ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state 1# tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (Happy_GHC_Exts.I# (i)) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn 1# tk st sts stk
     = happyFail [] 1# tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn 1# tk st sts stk
     = happyFail [] 1# tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn 1# tk st sts stk
     = happyFail [] 1# tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn 1# tk st sts stk
     = happyFail [] 1# tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn 1# tk st sts stk
     = happyFail [] 1# tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn 1# tk st sts stk
     = happyFail [] 1# tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn 1# tk st sts stk
     = happyFail [] 1# tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Happy_GHC_Exts.Int#
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop 0# l = l
happyDrop n ((_):(t)) = happyDrop (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) t

happyDropStk 0# l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Happy_GHC_Exts.-# (1#::Happy_GHC_Exts.Int#)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist 1# tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (Happy_GHC_Exts.I# (i)) -> i }) in
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
        action 1# 1# tk (HappyState (action)) sts ((HappyErrorToken (Happy_GHC_Exts.I# (i))) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Prelude.error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions


happyTcHack :: Happy_GHC_Exts.Int# -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}


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
