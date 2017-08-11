module Main where

import Data.IORef
import TimeTracker.Interface
import TimeTracker.Types
import TimeTracker.IO.Database


main :: IO ()
main = let eventCallback pid _ name = putStrLn ("PID called from Haskell: " ++ (show pid) ++ ", name: " ++ name)
           procRegex = ".*"
           cwdRegex = ".*"
           procM = addProcMatcher eventCallback procRegex False cwdRegex
        in do
            putStrLn "Haskell main started"



continueCallback = do
            counter <- newIORef 0
            let continueCallback = \_ -> do
                    modifyIORef' counter (+1)
                    count <- readIORef counter
                    putStrLn ("listenUntilCallback' called, count is " ++ (show count))
                    if count > 5 then return (boolToCInt False)
                                 else return (boolToCInt True)
            return continueCallback

logCallback :: Integer -> Integer -> String -> DbMonad ()
logCallback pid procEventTypeInt progName = 
        let procEventData = case intToProcMatchEventType procEventTypeInt
                                of Other     -> Other
                                   NoEvent   -> NoEvent
                                   ProcStart -> ProcStart pid
                                   ProcEnd   -> ProcEnd -1 -- TODO: need to get the actual exit code
        in do
            now <- liftIO getCurrentTime
            insertProcEvents [(procEventData, now, progName)]


dbMonadAction = 
        let procRegex = ".*"
            cwdRegex = ".*"
            procM x = addProcMatcher x procRegex False cwdRegex
        in asIOAction logCallback >>= 
            (liftIO . procM) >> 
            liftIO . runProgramLogger (procM >> listenUntilCallback continueCallback) >>= \res ->
            liftIO . putStrLn $ "res = " ++ (show res)
