module Main where

import Data.IORef
import TimeTracker.Interface
import TimeTracker.Types


main :: IO ()
main = let eventCallback pid _ name = putStrLn ("PID called from Haskell: " ++ (show pid) ++ ", name: " ++ name)
           procRegex = ".*"
           cwdRegex = ".*"
           procM = addProcMatcher eventCallback procRegex False cwdRegex
        in do
            putStrLn "Haskell main started"
            counter <- newIORef 0
            let continueCallback = \_ -> do
                    modifyIORef' counter (+1)
                    count <- readIORef counter
                    putStrLn ("listenUntilCallback' called, count is " ++ (show count))
                    if count > 5 then return (boolToCInt False)
                                 else return (boolToCInt True)

            res <- runProgramLogger (procM >> listenUntilCallback continueCallback)
            putStrLn $ "res = " ++ (show res)
