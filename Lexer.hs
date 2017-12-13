{-# LANGUAGE MultiWayIf #-}

module Lexer
	( Token (..)
	, ParT (..)
	, module Util
	, tokenStream
	, lexer
	, deSugar
	) where

import Data.Char
import Text.ParserCombinators.Parsec hiding (Parser)
import Text.Parsec.Prim (ParsecT)
import Data.Functor.Identity

import Util
import AST
import DeSugar

opChar = "!#$%^*+-/:<=>?@^_|~.,"
lAlphaChar = "abcdefghijklmnopqrstuvwxyz"
uAlphaChar = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
numChar = "0123456789\'"
alphaNumChar = lAlphaChar ++ uAlphaChar ++ numChar
keywords = 
	[ "import"
	, "module"
	, "where"
	, "data"
	, "deriving"
	, "do"
	, "case"
	, "of"
	, "let"
	, "in"
	, "class"
	, "instance"
	, "infixl"
	, "infixr"
	, "->"
	, "<-"
	, "="
	, "::"
	, "=>"
	]

type Parser a = ParsecT String [(Name, Fixity)] Identity (a ())

lexer :: String -> IO ([(Name, Fixity)], [Token ()])
lexer str = do
	f <- deSugar str
	case runParser tokenStream [] "" f of
		Right t -> return t
		Left e  -> fail (show e)

tokenStream =
	let token =
			    qIdentifier
			<|> infixfn
			<|> lParens <|> rParens
			<|> strLit
			<|> chrLit
			<|> numLit
			<|> lambda
			<|> semic
	in do
		many whitespace
		toks <- many $ do
			t <- token
			many whitespace
			return t
		eof
		s <- getState
		return (s, toks)

whitespace = char ' ' <|> char '\t' <|> char '\n'

qIdentifier :: Parser Token
qIdentifier = do
	Name qs n i <- qualifiedName
	if n `Prelude.elem` keywords
		then keyword n
		else return $ Identifier () (Name qs n i)

keyword n = case take 5 n of
	"infix" -> do
		d <- case drop 5 n of
			"l" -> return L
			"r" -> return R
			_   -> fail "Fixity declaration"
		f <- char ' ' >> digit
		n <- char ' ' >> qualifiedName
		st <- getState
		setState $ (n, Fixity (read [f]) d) : st
		return $ FixDecl () n
	k       -> return $ Keyword () k

qualifiedName :: ParsecT [Char] [(Name, Fixity)] Identity Name
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

operator :: ParsecT [Char] [(Name, Fixity)] Identity Name
operator = do
	op <- many1 (oneOf opChar)
	return (Name [] op True)

identifier :: ParsecT [Char] [(Name, Fixity)] Identity Name
identifier = do
	i <- oneOf uAlphaChar <|> oneOf lAlphaChar
	is <- many (oneOf alphaNumChar)
	return (Name [] (i:is) False)

infixfn :: Parser Token
infixfn = do 
		char '`'
		op <- qualifiedName
		char '`'
		return $ Identifier () (op { infixn = True })

numLit :: Parser Token
numLit = do
	i <- many (oneOf numChar)
	case reads i of
		[(a,[])] -> return $ LitTok () (IntLit a)
		_        -> fail "Expected Integer literal"

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

strLit :: Parser Token
strLit = do
	char '\"'
	str <- many (escSeq <|> noneOf "\"")
	char '\"'
	return $ LitTok () (StrLit str)

chrLit :: Parser Token
chrLit = do
	char '\''
	chr <- (escSeq <|> noneOf "\'")
	char '\''
	return $ LitTok () (ChrLit chr)

lParens :: Parser Token
lParens = do
	p <- oneOf "([{"
	let par = case p of
		'(' -> Parenthesis
		'[' -> SquareBracket
		'{' -> CurlyBracket
	return $ LParens () par

rParens :: Parser Token
rParens = do
	p <- oneOf ")]}"
	let par = case p of
		')' -> Parenthesis
		']' -> SquareBracket
		'}' -> CurlyBracket
	return $ RParens () par

semic = do
	char ';'
	return $ Semic ()

lambda :: Parser Token
lambda = do
	char '\\'
	return $ Lambda ()
