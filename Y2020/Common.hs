module AdventOfCode.Y2020.Common where

import Control.Monad
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
