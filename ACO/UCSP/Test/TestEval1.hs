module Main where

import ACO.UCSP.Test.Eval2TeX
import ACO.UCSP.Test.TestExec1


main = writeTexDefsToFile [
    ( "NClasses", show nClasses )
  , ( "NTime", show nTime )
  , ( "NProfessors", show nProfessors)
  , ( "NRooms", show nRooms )
  , ( "NCombinations", show nCombinations )
  ]






