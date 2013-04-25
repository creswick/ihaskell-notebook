module Printer where

import System.IO (openFile, IOMode(WriteMode))
import Data.Char (chr)

import OurPrelude

ourPrint :: Show a => a -> IO ()
ourPrint a = writeFile temp_file $ show a
             -- print ("Temp file: "++temp_file++": " ++ show a) 
