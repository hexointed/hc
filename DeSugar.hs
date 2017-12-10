{-# LANGUAGE MultiWayIf #-}

module DeSugar (deSugar) where

import Text.ParserCombinators.Parsec hiding (Parser)
import Text.Parsec.Prim (ParsecT)
import Data.Functor.Identity

type Parser = ParsecT String [Int] Identity String

deSugar str = 
	let res = do
		str <- runParser comment [] "" str
		runParser chars [0] "" str
	in case res of
		Right r -> return r
		Left e  -> fail (show e)

comment = many $ lineComment <|> anyChar

lineComment = do
	char '-'
	c <- lookAhead $ anyChar
	case c of
		'-' -> many (noneOf "\n") >> char '\n'
		_   -> return '-'

chars = do
	xs <- many
		$   idP 
		<|> indent 
	return $ concat xs

idP :: Parser
idP = do
	c <- noneOf "\n"
	return [c]

indent :: Parser
indent = do
	is <- getState
	(n, t) <- fmap (\xs -> (length xs, length (last xs))) . many1 $ do
		char '\n'
		many $ char '\t'
	insBracks is t n

insBracks :: [Int] -> Int -> Int -> Parser
insBracks []     _ n = fail "Internal error"
insBracks (i:is) t n = if
	| i <  t -> do
		setState (t:i:is)
		return ("{" ++ replicate n '\n' ++ replicate t '\t')
	| i == t -> return (";" ++ replicate n '\n' ++ replicate t '\t')
	| i >  t -> do
		setState is
		xs <- insBracks is t n
		return ("}" ++ xs)
