module AdventOfCode.Common.Parser (
   module AdventOfCode.Common.Parser,
   module Control.Monad,
   module Control.Applicative
) where

import Control.Monad
import Control.Applicative
import Control.Monad.Fail
import qualified Control.Monad.Fail as MF

import Data.Maybe
import Data.List

--------------------------------------------------------------------------------

newtype Parser a = Parser {apply :: ReadS a}

instance Functor Parser where
   fmap f p = Parser $ \ s ->
      [ (f x, r) | (x, r) <- apply p s ]

instance Applicative Parser where
   pure x = Parser $ \ s -> [(x, s)]
   f <*> p = Parser $ \ s ->
      [ (g x, r') | (x, r) <- apply p s, (g, r') <- apply f r ]

instance Alternative Parser where
   empty = Parser $ const []
   p <|> q = Parser $ \ s -> apply p s ++ apply q s
   -- Default definitions for `many` and `some` didn't halt for Parser:
   many p = pure [] <|> some p   -- empty list OR at least one
   some p = do                   -- one or more
      h <- p                     -- list head
      t <- many p                -- zero or more tail elements
      return (h:t)               -- stick the list together

instance Monad Parser where
   fail _ = empty
   p >>= f = Parser $ \ s -> do { (x,r) <- apply p s; apply (f x) r }

instance MonadPlus Parser
   -- grants access to mfilter et al.

instance MonadFail Parser where
   fail _ = empty

char :: Parser Char
char = Parser $ \s -> if null s then [] else [(head s, tail s)]

string :: Parser String
string = pure "" <|> liftA2 (flip (:)) string char

chars :: Int -> Parser String
chars n = replicateP n char 

match :: String -> Parser String
match lit = do
   s <- chars (length lit)
   guard (s == lit)
   return s

whitespace :: Parser String
whitespace = some $ do
   c <- char
   guard (c `elem` " \n\f\r")
   return c

reader :: Read a => Parser a
reader = Parser reads

greedy :: Parser a -> Parser a
greedy f = Parser $ \s -> let
   result = sortOn (length . snd) $ apply f s
   maxlen = length $ snd $ head result
   in takeWhile ((maxlen==) . length . snd) result

replicateP :: Int -> Parser a -> Parser [a]
replicateP n f = reverse <$> sequence (replicate n f)

parseUsing :: Parser a -> String -> a
parseUsing p s = case parseM p s of
   Just x  -> x
   Nothing -> error "failed to parse"

parseM :: Monad m => Parser a -> String -> m a
parseM p s = case map fst $ filter (null . snd) $ apply p s of
   [x] -> return x
   _   -> Prelude.fail "failed to parse"

parseMaybe :: Parser a -> String -> Maybe a
parseMaybe = parseM
