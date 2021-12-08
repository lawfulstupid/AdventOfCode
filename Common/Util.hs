module AdventOfCode.Common.Util where

import GHC.IO (unsafePerformIO)

debug :: Bool -> IO () -> ()
debug condition statement = unsafePerformIO $ if condition then statement else return ()

printLines :: Show a => [a] -> IO ()
printLines = mapM_ print
