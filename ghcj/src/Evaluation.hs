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

mkTarget :: String -> String -> IO Target
mkTarget name code = do now <- getCurrentTime
                        return Target { targetId = TargetModule $ mkModuleName name
                                      , targetAllowObjCode = False
                                      , targetContents = Just (stringToStringBuffer code, now) 
                                      }

mkTargetFile :: FilePath -> IO Target
mkTargetFile path = do return Target { targetId = TargetFile path Nothing
                                     , targetAllowObjCode = False
                                     , targetContents = Nothing
                                     }

tmpFileSrc = "temp_file = \"the_file\""

ourPreludeSrc = "module OurPrelude where\nimport Prelude\n" ++ tmpFileSrc

initSession :: IO Session
initSession = runGhc (Just libdir) $ do 
                dflags <- getSessionDynFlags 
                setSessionDynFlags dflags { ghcLink = LinkInMemory
                                          , hscTarget = HscInterpreted 
                                          -- | Interactive print
                                          -- doesn't seem to work, but
                                          -- parseName (below) does
                                          , interactivePrint = Just "OurPrelude.ourPrint" }

                ourTarget <- MonadUtils.liftIO $ mkTarget "OurPrelude" ourPreludeSrc
                addTarget ourTarget

                printTarget <- MonadUtils.liftIO $ mkTargetFile "Printer.hs"
                addTarget printTarget

                sFlag <- load LoadAllTargets
                setContext [IIModule $ mkModuleName "OurPrelude", IIModule $ mkModuleName "Printer"]

                (name:_) <- parseName "Printer.ourPrint"
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

-- evalModule :: String -> State EvalState Output
-- evalModule code = do 

evalStmt :: String -> StateT EvalState Ghc Output
evalStmt stmt = do runResult <- lift $ gcatch (runStmt stmt RunToCompletion) handler
                   return Output { outputCellNo = 0
                                 , outputData = runResultToStr runResult }
    where handler e = return $ RunException e


-- showOut flags name = let ctx = initSDocContext flags defaultDumpStyle
--                      in runSDoc (ppr name) ctx
-- printOut flags name = print $ showOut flags name

-- eval :: HscEnv -> Target -> String -> IO (RunResult, HscEnv)
-- eval session target stmt = defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
--                      runGhc (Just libdir) $ do
--                        setSession session
--                        dflags <- getSessionDynFlags 
--                        setSessionDynFlags dflags { ghcLink = LinkInMemory
--                                                  , hscTarget = HscInterpreted }
--                        addTarget target
--                        sFlag <- load LoadAllTargets
--                        setContext [IIModule $ mkModuleName "TestModule1"]
                       
--                        case sFlag of
--                          Failed    -> error "Compliation Failed"
--                          Succeeded -> do result <- runStmt stmt RunToCompletion
--                                          newSession <- getSession
--                                          return (result, newSession)

runResultToStr :: RunResult -> String
runResultToStr (RunOk ns)      = "RunOk " ++ (show $ length ns)
runResultToStr (RunException e) = "RunException " ++ show e
runResultToStr RunBreak {}     = "RunBreak"