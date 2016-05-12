module Main where

-- import ACO.UCSP.Definitions
-- import ACO.UCSP.Implementation

import qualified ACO.UCSP.Test.TestExec1 as Test


main = do putStrLn "Running ACO Test #1"
          res <- Test.run
          putStrLn "Done\n"
          print $ length res


 
