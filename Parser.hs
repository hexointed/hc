{-# LANGUAGE NoMonomorphismRestriction, MultiWayIf #-}

module Parser where

import Control.Monad
import Data.Char
import Text.ParserCombinators.Parsec hiding (Parser)
import Text.Parsec.Prim (ParsecT)
import Data.Functor.Identity

import Lexer
import AST

type Parser a = ParsecT [Token ()] () Identity (a ())

parser str = do
	t <- lexer str
	case runParser declList () "" t of
		Right t -> return t
		Left e  -> fail (show e)

declList = do
	ds <- many $ do
		d <- decl
		tok (Semic ())
		return d
	eof
	return (map simplify ds)

decl :: Parser Decl
decl = unit varD $ 
	do
		n <- identifier
		ps <- many identifier
		keyword "="
		e <- expr
		return (n, ps, e)

primExpr = 
	    varE
	<|> litE
	<|> case'
	<|> let'
	<|> lambda
	<|> between lpar rpar expr

expr = expr' (-1)

appExpr :: Parser Expr
appExpr = 
	unit AppE $ do
		e <- primExpr <|> indent expr
		es <- many primExpr
		ess <- optionMaybe $ indents expr
		return (e, es ++ concat ess)

os = [("$",0),("||",2),("&&",3),("-",6),("+",6),("*",7),(".",9)]

expr' :: Int -> Parser Expr
expr' 10 = appExpr
expr' i = do
	f <- pEither (operator i) (expr' (i+1))
	case f of
		Left o  -> do
			e <- optionMaybe $ expr' (fixity o)
			case e of
				Just e  -> return $ operRE () o e
				Nothing -> return $ VarE () o
		Right e -> do
			o <- optionMaybe (operator i)
			case o of
				Just o  -> do
					e' <- optionMaybe $ expr' (fixity o)
					case e' of
						Just e' -> return $ operE () o [e, e']
						Nothing -> return $ operE () o [e]
				Nothing -> return e

operRE a o e = LamE a [n] (AppE a (VarE a o) [VarE a n, e])
	where
		n = Name ["unique"] "x" False 
operE a o es = AppE a (VarE a o) es


pEither p q = do
	p <- optionMaybe p

	case p of
		Just p' -> return $ Left p'
		Nothing -> fmap Right q
	
lambda :: Parser Expr
lambda = 
	unit LamE $ do
		tok $ Lambda ()
		ps <- many identifier
		keyword "->"
		e <- expr
		return (ps, e)

case' :: Parser Expr
case' = unit CaseE $ do 
	keyword "case"
	e <- expr
	keyword "of"
	cs <- indents $ do
		p <- pattern
		keyword "->"
		e <- expr
		return (p, e)
	return (e, cs)

let' :: Parser Expr
let' = unit LetE $ do
	keyword "let"
	ds <- (<|>)
		(indents decl << endl << keyword "in") 
		(fmap (:[]) decl << keyword "in")
	e <- expr
	return (ds, e)

litE :: Parser Expr
litE = do
	LitTok _ l <- mfilter' isLiteral
	return (LitE () l)

varE :: Parser Expr
varE = unit VarE (constructor <|> identifier)

pattern = constrP <|> varP

constrP :: Parser Pattern
constrP = 
	unit ConstrP $ do
		c <- constructor
		cs <- many pattern
		return (c, cs)

varP :: Parser Pattern
varP = unit VarP (identifier <|> constructor)

constructor = try $ do
	Identifier () n <- mfilter' isIdentifier
	if isUpper . head . name $ n
		then return n
		else fail ""

fixity :: Name -> Int
fixity o = case lookup (name o) os of
	Just i  -> i
	Nothing -> 9

operator i = try $ do
	Identifier () n <- mfilter' isIdentifier
	if infixn n && fixity n == i
		then return n
		else fail ""


identifier = try $ do
	Identifier () n <- mfilter' isIdentifier
	if isLower . head . name $ n
		then return n
		else fail ""

isIdentifier t = case t of
	Identifier () i -> True
	_               -> False

isLiteral t = case t of
	LitTok () i -> True
	_           -> False

unit f p = fmap (apply (f ())) p

indents f = do
	tok (LParens () CurlyBracket)
	t <- f
	ts <- many $ do
		tok (Semic ())
		f
	tok (RParens () CurlyBracket)
	return (t:ts)

indent f = do
	tok (LParens () CurlyBracket)
	t <- f
	tok (RParens () CurlyBracket)
	return t

endl = do
	tok (Semic ())

lpar = tok (LParens () Parenthesis)
rpar = tok (RParens () Parenthesis)

mfilter' f = try $ mfilter f anyToken

keyword k = tok $ Keyword () k

tok t = mfilter' (== t) <|> fail ("Expected " ++ show t)

f << g = do
	f' <- f
	g
	return f'
