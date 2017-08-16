module TimeTracker.Callbacks where


-- | run a certain number of times
countCallback :: Int -> IO FFI.StopListeningCallback
countCallback limit = do
            counter <- newIORef 0
            let continueCallback' = \_ -> do
                    modifyIORef' counter (+1)
                    count <- readIORef counter
                    putStrLn ("listenUntilCallback' called, count is " ++ (show count))
                    if count > limit then return (boolToCInt False)
                                 else return (boolToCInt True)
            return continueCallback'


logCallback :: (String -> IO ()) -> Integer -> Integer -> String -> DbMonad ()
logCallback logError pid procEventTypeInt progName = 
        let procEventData = FFI.intToProcMatchEventType (fromInteger procEventTypeInt) >>= 
                   \x -> case x of FFI.Other     -> Just Other
                                   FFI.NoEvent   -> Just NoEvent
                                   FFI.ProcStart -> Just . ProcStart . fromInteger $ pid
                                   FFI.ProcEnd   -> Just . ProcEnd . fromInteger $ -1 -- TODO: need to get the actual exit code
        in do
            -- TODO: fix line length
            case procEventData of Nothing -> liftIO $ logError ("Received unknown event type from progName" ++ progName)
                                  Just ev -> insertProcEvents [(ev, progName)]

dbMonadAction :: (String -> IO ()) -> DbMonad ()
dbMonadAction logError = 
        let procRegex = ".*"
            cwdRegex = ".*"
            procM x = addProcMatcher x procRegex False cwdRegex

            logCallback' :: IORef PidCache -> DbMonad (EventCallback)
            logCallback' ref = callbackAsIO (logCallback logError) >>= liftIO . return . withPidCache ref

        in (liftIO initPidCache >>= logCallback') >>= \c ->
            liftIO $ do
                continueCallback' <- continueCallback
                let programLoggerAction = procM c >> listenUntilCallback continueCallback'
                res <- runProgramLogger programLoggerAction
                putStrLn $ "res = " ++ (show res)
