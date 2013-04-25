module Printer where

import OurPrelude

ourPrint :: Show a => a -> IO ()
ourPrint a = do print ("Temp file: "++temp_file++": " ++ show a)