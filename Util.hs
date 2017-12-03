{-# LANGUAGE MultiWayIf, MultiParamTypeClasses, FlexibleInstances #-}

module Util where

import Control.Monad
import Data.Monoid
import Data.Char
import Text.ParserCombinators.Parsec
import Text.Parsec.Prim (ParsecT)

data Name = Name 
	{ qualifiers :: [String]
	, name :: String
	, infixn :: Bool
	}
	deriving Eq

instance Show Name where
	show (Name qs n i)
		| i         = "`" ++ foldr (\a b -> a ++ "." ++ b) [] qs ++ n ++ "`"
		| otherwise = show $ foldr (\a b -> a ++ "." ++ b) [] qs ++ n

qualify (Name qs n i) q = Name (q:qs) n i

data Position = Position String (Int, Int) (Int, Int)
	deriving (Show, Eq)

fromSP sp = Position name pos pos
	where
		name = sourceName sp
		pos  = (sourceLine sp, sourceColumn sp)

instance Monoid Position where
	mempty = Position "" (0,0) (-1,0)
	mappend (Position n1 min1 max1) (Position n2 min2 max2) = Position n mi ma
		where
			n  = if length n1 > length n2 then n1 else n2
			mi = if 
				| fst max1 < 0 -> min2
				| fst max2 < 0 -> min1
				| otherwise    -> min min1 min2
			ma = if fst max1 < 0 then max2 else max max1 max2

class Uncurry a r b where
	apply :: a -> b -> r

instance Uncurry (a -> r) r a where
	apply f a = f a

instance Uncurry (a -> b -> r) r (a, b) where
	apply f (a, b) = f a b

instance Uncurry (a -> b -> c -> r) r (a, b, c) where
	apply f (a, b, c) = f a b c

instance Uncurry (a -> b -> c -> d -> r) r (a, b, c, d) where
	apply f (a, b, c, d) = f a b c d
