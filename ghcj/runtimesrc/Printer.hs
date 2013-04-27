module Printer where

import System.IO ( withFile, hPutStr, hPutStrLn
                 , openFile, IOMode(WriteMode)
                 , Handle)
import Data.Char (chr)

import OurPrelude

-- withFile :: FilePath -> IOMode -> (Handle -> IO r) -> IO r
ourPrint :: Show a => a -> IO ()
ourPrint a = do withFile temp_file WriteMode (storeVal)
    where storeVal :: Handle -> IO ()
          storeVal hdl = do hPutStrLn hdl "text/plain"
                            hPutStr hdl $ show a

--writeFile temp_file $ show a
             -- print ("Temp file: "++temp_file++": " ++ show a)
