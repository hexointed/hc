{-# LANGUAGE DeriveFunctor #-}

module AST where

import Lexer

data Decl a
	= VarD a String (Expr a)
	deriving (Eq, Functor)

varD a n ps e = VarD a n (LamE a ps e)

data Expr a
	= AppE  a (Expr a) [Expr a]
	| CaseE a (Expr a) [(Pattern a, Expr a)]
	| VarE  a Name
	| LitE  a String
	| LamE  a [Name] (Expr a)
	| LetE  a [Decl a] (Expr a)
	deriving (Eq, Functor)

data Pattern a
	= VarP    a Name
	| ConstrP a Name [Pattern a]
	deriving (Eq, Functor)

show' :: Show a => a -> String
show' = drop 3 . init . unlines . map ("    "++) . lines . show

showAnn name ann = name ++ " { " ++ show ann ++ "}\n"

showSub a = "  ->" ++ show' a ++ "\n"

instance Show a => Show (Decl a) where
	show (VarD a n e) = showAnn "VarD" a ++ showSub n ++ showSub e

instance Show a => Show (Expr a) where
	show (AppE a f es)  = showAnn "AppE"  a ++ showSub f ++ showSub es
	show (CaseE a e ps) = showAnn "CaseE" a ++ showSub e ++ showSub ps
	show (VarE a n)     = showAnn "VarE"  a ++ showSub n
	show (LitE a s)     = showAnn "LitE"  a ++ showSub s
	show (LamE a as e)  = showAnn "LamE"  a ++ showSub as ++ showSub e

instance Show a => Show (Pattern a) where
	show (VarP a n)       = showAnn "VarP" a    ++ showSub n
	show (ConstrP a n ps) = showAnn "ConstrP" a ++ showSub n ++ showSub ps

class Ast a where
	simplify :: a f -> a f

instance Ast Decl where
	simplify (VarD a n e) = VarD a n (simplify e)

instance Ast Expr where
	simplify (AppE a e [])  = simplify e
	simplify (AppE a e es)  = AppE a (simplify e) (map simplify es)
	simplify (CaseE a e ps) = CaseE a (simplify e) (map (\(p,e) -> (simplify p, simplify e)) ps)
	simplify (LamE a [] e)  = simplify e
	simplify (LamE a as e)  = LamE a as (simplify e)
	simplify (LetE a [] e)  = simplify e
	simplify (LetE a ds e)  = LetE a (map simplify ds) (simplify e)
	simplify e              = e

instance Ast Pattern where
	simplify = id
