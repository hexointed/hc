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

class Uncurry a r b where
	apply :: a -> b -> r

instance Uncurry a a () where
	apply f a = f

instance Uncurry (a -> r) r a where
	apply f a = f a

instance Uncurry (a -> b -> r) r (a, b) where
	apply f (a, b) = f a b

instance Uncurry (a -> b -> c -> r) r (a, b, c) where
	apply f (a, b, c) = f a b c

instance Uncurry (a -> b -> c -> d -> r) r (a, b, c, d) where
	apply f (a, b, c, d) = f a b c d

fromFile file t = do
	f <- readFile file
	t f

data Fixity = Fixity
	{ fix :: Int
	, assoc :: T
	}
	deriving (Show, Eq)

data T = L | R
	deriving (Show, Eq)

inc (Fixity i a) = Fixity (i + 1) a

instance Ord Fixity where
	Fixity i a <= Fixity j b = i <= j

