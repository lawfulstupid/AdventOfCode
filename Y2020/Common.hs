module AdventOfCode.Y2020.Common where

import Control.Monad
import Control.Applicative
import Control.Monad.Fail
import GHC.IO
import Data.Map (Map)
import qualified Data.Map as M
import Data.Tuple (swap)



debug :: Bool -> IO () -> ()
debug condition statement = unsafePerformIO $ if condition then statement else return ()

printLines :: Show a => [a] -> IO ()
printLines = mapM_ print

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

breakAll :: (a -> Bool) -> [a] -> [[a]]
breakAll p [] = [[]]
breakAll p (x:xs) = let
   (a:b) = breakAll p xs
   in if p x then []:(a:b) else (x:a):b

splitAll :: Int -> [a] -> [[a]]
splitAll n [] = []
splitAll n xs = let (a,b) = splitAt n xs in a : splitAll n b

grouping :: Ord k => [(k,a)] -> Map k [a]
grouping = M.fromListWith (++) . map (fmap return)

reverseMap :: (Ord a, Ord b) => Map a [b] -> Map b [a]
reverseMap m = grouping $ map swap (M.assocs m >>= \(k,vs) -> map (k,) vs)

solveToBijection :: (Ord a, Ord b) => Map a [b] -> Map a b
solveToBijection = aux M.empty
   where
   aux :: (Ord a, Ord b) => Map a b -> Map a [b] -> Map a b
   aux out m | M.size m == 0 = out
   aux out m = let
      (final, ambig) = M.partition (\xs -> length xs == 1) m
      confirmedAssocs = M.assocs $ M.map head final
      out' = M.union out $ M.fromList confirmedAssocs
      confirmed = map snd confirmedAssocs
      m' = M.map (filter (\x -> not (x `elem` confirmed))) ambig
      in aux out' m'

binToDec :: Num a => [Bool] -> a
binToDec = foldr (\x y -> (if x then 1 else 0) + 2 * y) 0

readBin :: Num a => String -> a
readBin = binToDec . reverse . map (=='1')

showBin :: (Show a, Integral a) => a -> String
showBin 0 = "0"
showBin n = let (d,m) = n `divMod` 2 in showNonZero d ++ show m
   where
   showNonZero 0 = ""
   showNonZero n = showBin n

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
parseUsing p s = case parseMaybe p s of
   Just x  -> x
   Nothing -> error "Failed to parse"

parseMaybe :: Parser a -> String -> Maybe a
parseMaybe p s = case filter (null . snd) $ apply p s of
   [(x,_)] -> Just x
   _ -> Nothing