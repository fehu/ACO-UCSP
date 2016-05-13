module ACO.UCSP.Test.Eval2TeX where

import Control.Monad
import System.IO
import System.Environment
import Text.Printf







writeTexDefsToFile defs = do [file] <- getArgs
                             withFile file WriteMode $ flip writeTexDefs defs
                             putStrLn $ "wrote TeX definitions to file " ++ file

writeTexDefs :: Handle -> [(String, String)] -> IO()
writeTexDefs h = mapM_ $ \(key,val) -> hPrintf h pattern key val
  where pattern = "\\def\\%s{%s}\n"


