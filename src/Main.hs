module Main where

import TimeTracker.FFI


main :: IO ()
main = let eventCallback pid _ = putStrLn ("PID called from Haskell: " ++ (show pid))
           procRegex = ".*"
           cwdRegex = ".*"
           procM = addProcMatcher eventCallback procRegex False cwdRegex
        in do
            putStrLn "Haskell main started"
            counter <- newIORef 0
            let listenUntilCallback' = do
                modifyIORef' counter (+1)
                count <- readIORef counter
                putStrLn ("listenUntilCallback' called, count is " ++ (show count))
                if count > 5 then False
                             else True

            runProgramLogger procM
            return ()
