{-# LANGUAGE MultiWayIf #-}

module Lexer
	( Token (..)
	, ParT (..)
	, module Util
	, tokenStream
	, lexer
	) where

import Control.Monad
import Data.Monoid
import Data.Char
import Text.ParserCombinators.Parsec
import Text.Parsec.Prim (ParsecT)

import Util

opChar = "!#$%^*+-/:;<=>?@^_|~.,"
lAlphaChar = "abcdefghijklmnopqrstuvwxyz"
uAlphaChar = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
numChar = "0123456789"
alphaNumChar = lAlphaChar ++ uAlphaChar ++ numChar
keywords = 
	[ "import"
	, "module"
	, "where"
	, "data"
	, "deriving"
	, "let"
	, "in"
	, "do"
	, "case"
	, "of"
	, "class"
	, "instance"
	, "->"
	, "<-"
	, "|"
	, "="
	, "::"
	]

data Token
	= Keyword String
	| Identifier Name
	| NumLit Integer
	| StrLit String
	| ChrLit Char
	| Lambda
	| Indent Int
	| LParens ParT
	| RParens ParT
	deriving (Show, Eq)

data ParT
	= Parenthesis
	| SquareBracket
	| CurlyBracket
	deriving (Show, Eq)

lexer file = do
	f <- readFile file
	return $ parse tokenStream file f

tokenStream =
	let token
		= liftM2 (,) (fmap fromSP getPosition)
			(   qIdentifier
			<|> infixfn
			<|> numLit
			<|> indent
			<|> lParens <|> rParens
			<|> strLit
			<|> chrLit
			<|> lambda
			)
		<|> (char ' ' >> token)
	in do
		toks <- many token
		eof
		return toks

qIdentifier :: Monad m => ParsecT [Char] st m Token
qIdentifier = do
	Name qs n i <- qualifiedName
	if n `Prelude.elem` keywords
		then return (Keyword n)
		else return (Identifier $ Name qs n i)

qualifiedName :: Monad m => ParsecT [Char] st m Name
qualifiedName = do
	r <- fmap Left identifier <|> fmap Right operator
	case r of
		Right op -> return op
		Left n   -> if
			| isLower (head $ name n) -> return n
			| otherwise               -> do
				t <- optionMaybe (char '.')
				case t of
					Nothing -> return n
					_       -> do
						qn <- qualifiedName
						return (qualify qn (name n))
				

operator :: Monad m => ParsecT [Char] st m Name
operator = do
	op <- many1 (oneOf opChar)
	return (Name [] op True)

identifier :: Monad m => ParsecT [Char] st m Name
identifier = do
	i <- oneOf uAlphaChar <|> oneOf lAlphaChar
	is <- many (oneOf alphaNumChar)
	return (Name [] (i:is) False)

infixfn :: Monad m => ParsecT [Char] st m Token
infixfn = do 
		char '`'
		op <- qualifiedName
		char '`'
		return (Identifier $ op { infixn = True })

numLit :: Monad m => ParsecT [Char] st m Token
numLit = do
	i <- many (oneOf numChar)
	case reads i of
		[(a,_)] -> return (NumLit a)
		_       -> fail "Expected Integer literal"

escSeq :: Monad m => ParsecT [Char] st m Char
escSeq = do
	char '\\'
	c <- anyChar
	case c of
		'\"' -> return '\"'
		'\'' -> return '\''
		'\\' -> return '\\'
		'n'  -> return '\n'
		't'  -> return '\t'
		_    -> fail $ "Unknown Escape sequence \\" ++ c:[]

strLit :: Monad m => ParsecT [Char] st m Token
strLit = do
	char '\"'
	str <- many (escSeq <|> noneOf "\"")
	char '\"'
	return (StrLit str)

chrLit :: Monad m => ParsecT [Char] st m Token
chrLit = do
	char '\''
	chr <- (escSeq <|> noneOf "\'")
	char '\''
	return (ChrLit chr)

indent :: Monad m => ParsecT [Char] st m Token
indent = do
	ls <- many1 $ do
		ls <- char '\n'
		ts <- many  (char '\t')
		return (Indent $ length ts)
	return (last ls)

lParens :: Monad m => ParsecT [Char] st m Token
lParens = do
	p <- oneOf "([{"
	let par = case p of
		'(' -> Parenthesis
		'[' -> SquareBracket
		'{' -> CurlyBracket
	return (LParens par)

rParens :: Monad m => ParsecT [Char] st m Token
rParens = do
	p <- oneOf ")]}"
	let par = case p of
		')' -> Parenthesis
		']' -> SquareBracket
		'}' -> CurlyBracket
	return (RParens  par)

lambda :: Monad m => ParsecT [Char] st m Token
lambda = do
	char '\\'
	return Lambda
