module Evaluation where

import Control.Monad.State

import GHC
import GhcMonad
import GHC.Paths
import DynFlags
import HscTypes
import StringBuffer
import Data.Time.Clock
import Outputable
-- import Finder
import MonadUtils (liftIO)
import InteractiveEval (RunResult(..))

import Types

runtimeSrcDir :: FilePath
runtimeSrcDir = "./runtimesrc"

mkTargetMod :: String -> String -> IO (ModuleName, Target)
mkTargetMod name code = do now <- getCurrentTime
                           let modName = mkModuleName name
                           return ( modName
                                  , Target { targetId = TargetModule modName
                                           , targetAllowObjCode = False
                                           , targetContents = Just (stringToStringBuffer code, now)
                                           })

mkTargetFile :: FilePath -> IO Target
mkTargetFile path = do return Target { targetId = TargetFile path Nothing
                                     , targetAllowObjCode = False
                                     , targetContents = Nothing
                                     }

mkCustomPrelude :: FilePath -> IO (ModuleName, Target)
mkCustomPrelude tmpFile = mkTargetMod "OurPrelude" ourPreludeSrc
    where ourPreludeSrc = "module OurPrelude where\n"++
                          "import Prelude\n" ++
                          "temp_file = "++show tmpFile

customPrintFnStr :: String
customPrintFnStr = "Printer.ourPrint"

mkModules :: FilePath -> IO [(ModuleName, Target)]
mkModules tmpFile = do (preludeName, ourPrelude) <- mkCustomPrelude tmpFile
                       printer                   <- mkTargetFile (runtimeSrcDir ++ "/Printer.hs")
                       let printerName = mkModuleName "Printer"
                       return [ (preludeName, ourPrelude)
                              , (printerName, printer)]

initSession :: FilePath -> FilePath -> IO Session
initSession tmpFile tmpDir = runGhc (Just libdir) $ do
                dflags <- getSessionDynFlags
                setSessionDynFlags dflags { ghcLink = LinkInMemory
                                          , hscTarget = HscInterpreted
                                          , importPaths = tmpDir:runtimeSrcDir:(importPaths dflags)
                                          -- | Interactive print
                                          -- doesn't seem to work, but
                                          -- parseName (below) does
                                          , interactivePrint = Just customPrintFnStr }
                nflags <- getSessionDynFlags
                MonadUtils.liftIO $ do putStrLn "Import paths"
                                       print $ importPaths nflags
                                       putStrLn "Include paths"
                                       print $ includePaths nflags
                                       putStrLn "libary paths"
                                       print $ libraryPaths nflags

                modules <- MonadUtils.liftIO $ mkModules tmpFile
                mapM_ (addTarget . snd) modules

                sFlag <- load LoadAllTargets
                case sFlag of
                  Failed    -> error "Could not initialize session."
                  Succeeded -> do
                             setContext $ map (IIModule . fst)  modules
                             -- setContext [ IIModule $ mkModuleName "OurPrelude"
                             --            , IIModule $ mkModuleName "Printer"]

                             (name:_) <- parseName customPrintFnStr
                             modifySession (\he -> let new_ic = setInteractivePrintName (hsc_IC he) name
                                                   in he { hsc_IC = new_ic })
                             -- reifyGhc takes a function of (Session -> IO a) and
                             -- returns Ghc a.  we use it here to get and return
                             -- the Session:
                             reifyGhc return

addModule :: String -> Ghc ()
addModule code = let target = buildTarget code
                 in do
                   addTarget target
                   sFlag <- load LoadAllTargets
                   setContext [IIModule $ mkModuleName "TestModule1"]

                   case sFlag of
                     Failed    -> error "Compliation Failed"
                     Succeeded -> return () -- no output when loading modules.

buildTarget :: String -> Target
buildTarget = undefined

evalStmt :: Int -> String -> StateT EvalState Ghc Output
evalStmt cId stmt = do runResult <- lift $ gcatch (runStmt stmt RunToCompletion) errHandler
                       case runResult of
                         RunBreak {}      -> return $ CompileError "RunBreak"
                         (RunException e) -> return (CompileError $ show e)
                         (RunOk _)        -> do tmpFile <- gets estateTmpFile
                                                itVal <- lift $ MonadUtils.liftIO $ getItVal tmpFile
                                                return Output { outputCellNo = cId
                                                              , outputData = itVal }
    where errHandler e = return $ RunException e

-- | Get the value of 'it' from the temp file.
getItVal :: FilePath -> IO String
getItVal path = readFile path

runResultToStr :: RunResult -> String
runResultToStr (RunOk ns)      = "RunOk " ++ (show $ length ns)
runResultToStr (RunException e) = "RunException " ++ show e
runResultToStr RunBreak {}     = "RunBreak"

-- showOut flags name = let ctx = initSDocContext flags defaultDumpStyle
--                      in runSDoc (ppr name) ctx
-- printOut flags name = print $ showOut flags name

-- evalModule :: Int -> String -> State EvalState Output
-- evalModule cId code = do 
