{-# LANGUAGE NoMonomorphismRestriction, MultiWayIf #-}

module Parser where

import Control.Monad
import Control.Monad.State
import Data.Char
import Text.ParserCombinators.Parsec hiding (Parser)
import Text.Parsec.Prim (ParsecT)
import Data.Functor.Identity

import Lexer
import AST

type Parser a = ParsecT [Token ()] [(Name, Fixity)] Identity (a ())

parser str = do
	(os, t) <- lexer str
	case runParser declList os "" t of
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
		f <- optionMaybe (mfilter' isFixty << tok (Semic ()))
		n <- identifier <|> operator
		case f of
			Just (FixDecl _ a) -> if n == a then return () else fail ""
			Nothing            -> return ()
		ps <- many identifier
		keyword "="
		e <- expr
		return (n, ps, e)

expr = evalStateT expp []

primExpr = 
	    varE
	<|> litE
	<|> case'
	<|> let'
	<|> lambda
	<|> between lpar rpar expr

appExpr :: Parser Expr
appExpr = 
	unit AppE $ do
		e <- primExpr <|> indent expr
		es <- many primExpr
		ess <- optionMaybe $ indents expr
		return (e, es ++ concat ess)

data ST = O Name | E (Expr ())
	deriving (Eq, Show)

expp :: StateT [ST] (ParsecT [Token ()] [(Name, Fixity)] Identity) (Expr ())
expp = do
	f <- lift $ optionMaybe (pE operator appExpr)
	os <- lift $ getState
	case f of
		Just f  -> modify (insert os f) >> expp
		Nothing -> do
			modify (let y f = f (y f) in y collapse)
			[E e] <- get
			return e

insert :: [(Name, Fixity)] -> ST -> [ST] -> [ST]
insert os o xs = case xs of
	[]   -> [o]
	x:xss -> case lowerFix os o x of
		True -> o : collapse id xs
		False -> case xss of
			[]      -> [o, x]
			xx:_ -> case lowerFix os o xx of
				True  -> o : collapse id xs
				False -> o : xs

lowerFix os a b = case (a, b) of
	(O a, O b) -> case fixity os a == fixity os b of
		True  -> let Fixity _ as = fixity os a in a == b && as == L
		False -> fixity os a < fixity os b
	(O a, E b) -> False
	(E a, O b) -> False
	(E a, E b) -> False

fixity os o = case lookup o os of
	Just i  -> i
	Nothing -> Fixity 9 L

collapse :: ([ST] -> [ST]) -> [ST] -> [ST]
collapse f xs = case xs of
	E a : O n : E b : xs -> f $ E (operE () n [b, a]) : xs
	O n : E e : xs       -> f $ E (operE () n [e]) : xs
	E e : O n : xs       -> f $ E (operRE () n e) : xs
	O n : xs             -> f $ E (VarE () n) : xs
	E e : []             -> E e : []

operRE a o e = LamE a [n] (AppE a (VarE a o) [VarE a n, e])
	where
		n = Name ["unique"] "x" False 
operE a o es = AppE a (VarE a o) es

pE o e = do
	o <- optionMaybe o
	case o of
		Just o' -> return $ O o'
		Nothing -> fmap E e
	
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

operator = try $ do
	Identifier () n <- mfilter' isIdentifier
	if infixn n
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

isFixty t = case t of
	FixDecl () n -> True
	_            -> False

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
