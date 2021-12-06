module AdventOfCode.Common.Util where

import GHC.IO (unsafePerformIO)

debug :: Bool -> IO () -> ()
debug condition statement = unsafePerformIO $ if condition then statement else return ()
