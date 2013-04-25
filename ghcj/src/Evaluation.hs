module Evaluation where

import Control.Monad.State

import GHC
import GhcMonad
import GHC.Paths
import DynFlags
import StringBuffer
import Data.Time.Clock
-- import Outputable
-- import Finder
import MonadUtils (liftIO)

import Types

mkTarget :: String -> IO Target
mkTarget code = do now <- getCurrentTime
                   return Target { targetId = TargetModule $ mkModuleName "OurPrelude"
                                 , targetAllowObjCode = False
                                 , targetContents = Just (stringToStringBuffer code, now) 
                                 }

initSession :: IO Session
initSession = runGhc (Just libdir) $ do 
                dflags <- getSessionDynFlags 
                setSessionDynFlags dflags { ghcLink = LinkInMemory
                                          , hscTarget = HscInterpreted }
                ourTarget <- MonadUtils.liftIO $ mkTarget "module OurPrelude where\nimport Prelude"
                addTarget ourTarget
                sFlag <- load LoadAllTargets
                setContext [IIModule $ mkModuleName "OurPrelude"]

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
evalStmt stmt = do runResult <- lift $ runStmt stmt RunToCompletion
                   return Output { outputCellNo = 0
                                 , outputData = runResultToStr runResult }



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
runResultToStr RunOk {}        = "RunOk"
runResultToStr RunException {} = "RunException"
runResultToStr RunBreak {}     = "RunBreak"