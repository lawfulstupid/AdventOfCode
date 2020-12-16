module AdventOfCode.Y2020.Common where

import Control.Monad
import Control.Applicative
import Control.Monad.Fail
import GHC.IO



debug :: Bool -> IO () -> ()
debug condition statement = unsafePerformIO $ if condition then statement else return ()

printLines :: Show a => [a] -> IO ()
printLines = mapM_ print

breakAll :: (a -> Bool) -> [a] -> [[a]]
breakAll p [] = [[]]
breakAll p (x:xs) = let
   (a:b) = breakAll p xs
   in if p x then []:(a:b) else (x:a):b



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

whitespace :: Parser String
whitespace = some $ do
   c <- char
   guard (c `elem` " \n\f\r")
   return c

reader :: Read a => Parser a
reader = Parser reads

parseUsing :: Parser a -> String -> a
parseUsing p s = case apply p s of
   [(x,"")] -> x
   _ -> error "Failed to parse"