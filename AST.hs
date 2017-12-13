{-# LANGUAGE DeriveFunctor #-}

module AST where

import Util

data Decl a
	= VarD a Name (Expr a)
	deriving (Eq, Functor)

varD a n ps e = VarD a n (LamE a ps e)

data Expr a
	= AppE  a (Expr a) [Expr a]
	| CaseE a (Expr a) [(Pattern a, Expr a)]
	| VarE  a Name
	| LitE  a Literal
	| LamE  a [Name] (Expr a)
	| LetE  a [Decl a] (Expr a)
	deriving (Eq, Functor)

data Pattern a
	= VarP    a Name
	| ConstrP a Name [Pattern a]
	deriving (Eq, Functor)

data Token a
	= Keyword    a String
	| Identifier a Name
	| LitTok     a Literal
	| Lambda     a
	| LParens    a ParT
	| RParens    a ParT
	| Semic      a
	| FixDecl    a Name
	deriving (Eq, Functor)

data Literal
	= StrLit String
	| ChrLit Char
	| IntLit Integer
	deriving (Eq, Show)

data ParT
	= Parenthesis
	| SquareBracket
	| CurlyBracket
	deriving (Show, Eq)

show' :: Show a => a -> String
show' = drop 3 . init . unlines . map ("    "++) . lines . show

showAnn name ann = name ++ " { " ++ show ann ++ " }\n"

showSub a = "  ->" ++ show' a ++ "\n"

instance Show a => Show (Decl a) where
	show (VarD a n e) = showAnn "VarD" a ++ showSub n ++ showSub e

instance Show a => Show (Expr a) where
	show (AppE a f es)  = showAnn "AppE"  a ++ showSub f ++ showSub es
	show (CaseE a e ps) = showAnn "CaseE" a ++ showSub e ++ showSub ps
	show (VarE a n)     = showAnn "VarE"  a ++ showSub n
	show (LitE a s)     = showAnn "LitE"  a ++ showSub s
	show (LamE a as e)  = showAnn "LamE"  a ++ showSub as ++ showSub e
	show (LetE a ds e)  = showAnn "LetE"  a ++ showSub ds ++ showSub e

instance Show a => Show (Pattern a) where
	show (VarP a n)       = showAnn "VarP" a    ++ showSub n
	show (ConstrP a n ps) = showAnn "ConstrP" a ++ showSub n ++ showSub ps

instance Show a => Show (Token a) where
	show (Keyword    a n) = showAnn "Keyword    " a ++ showSub n
	show (Identifier a n) = showAnn "Identifier " a ++ showSub n
	show (LitTok     a n) = showAnn "LitTok     " a ++ showSub n
	show (Lambda     a  ) = showAnn "Lambda     " a
	show (LParens    a n) = showAnn "LParens    " a ++ showSub n
	show (RParens    a n) = showAnn "RParens    " a ++ showSub n
	show (Semic      a  ) = showAnn "Semic      " a
	show (FixDecl    a n) = showAnn "FixDecl    " a ++ showSub n

class Ast a where
	simplify :: a f -> a f
	getAnn :: a f -> f

instance Ast Decl where
	simplify (VarD a n e) = VarD a n (simplify e)

	getAnn (VarD a n e) = a

instance Ast Expr where
	simplify (AppE a e [])  = simplify e
	simplify (AppE a e es)  = AppE a (simplify e) (map simplify es)
	simplify (CaseE a e ps) = CaseE a (simplify e) (map (\(p,e) -> (simplify p, simplify e)) ps)
	simplify (LamE a [] e)  = simplify e
	simplify (LamE a as e)  = LamE a as (simplify e)
	simplify (LetE a [] e)  = simplify e
	simplify (LetE a ds e)  = LetE a (map simplify ds) (simplify e)
	simplify e              = e

	getAnn (AppE a e es)  = a
	getAnn (CaseE a e es) = a
	getAnn (LamE a e es)  = a
	getAnn (LetE a e es)  = a
	getAnn (LitE a s)     = a
	getAnn (VarE a s)     = a

instance Ast Pattern where
	simplify = id

	getAnn (VarP a s)       = a
	getAnn (ConstrP a n ps) = a

instance Ast Token where
	simplify = id

	getAnn (Keyword    a b) = a
	getAnn (Identifier a b) = a
	getAnn (LitTok     a b) = a
	getAnn (Lambda     a)   = a
	getAnn (LParens    a b) = a
	getAnn (RParens    a b) = a
