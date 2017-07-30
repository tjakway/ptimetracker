{-# LANGUAGE ScopedTypeVariables #-}
module TimeTracker.Interface where

import qualified TimeTracker.FFI as FFI
import TimeTracker.Types
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Alloc (free)

newEventCallback :: FFI.EventCallback -> ProgramLoggerM FFI.EventCallbackFunPtr
newEventCallback f = ProgramLoggerM $ \s -> do
    fptr <- FFI.wrapEventCallback f
    let eventCallbacks' = fptr : (eventCallbacks s)
    return (fptr, s { eventCallbacks = eventCallbacks' } )

newStopListeningCallback :: FFI.StopListeningCallback -> ProgramLoggerM FFI.StopListeningCallbackFunPtr
newStopListeningCallback f = ProgramLoggerM $ \s -> do
    fptr <- FFI.wrapStopListeningCallback f
    let stopListeningCallbacks' = fptr : (stopListeningCallbacks s)
    return (fptr, s { stopListeningCallbacks = stopListeningCallbacks' } )


-- standards-conformant
-- see https://stackoverflow.com/questions/5369770/bool-to-int-conversion
boolToCInt :: Bool -> CInt
boolToCInt True  = 1
boolToCInt False = 0


-- ***********************************************************************
--TODO: maybe compose with a function to turn the CInt return types ->
--a haskell data type indicating status?

-- TODO: make the strings optional then pass nullPtr if either are Nothing

addProcMatcher :: EventCallback -> String -> Bool -> String -> ProgramLoggerM ()
addProcMatcher callback procRegex matchOnlyProgName cwdRegex = do
        fPtr <- newEventCallback callback
        (procRegexCStr, matchOnlyProgName', cwdRegexCStr) <- marshall
        sPtr <- getAPIState
        liftS $ (FFI.addProcMatcher sPtr fPtr procRegexCStr matchOnlyProgName' cwdRegexCStr 
                >> free procRegexCStr
                >> free cwdRegexCStr)

    where marshall :: ProgramLoggerM (CString, CInt, CString)
          marshall = ProgramLoggerM $ \s -> do
            procRegexCStr <- newCString procRegex
            cwdRegexCStr  <- newCString cwdRegex
            let matchOnlyProgName' = boolToCInt matchOnlyProgName
            return ((procRegexCStr, matchOnlyProgName', cwdRegexCStr), s)


listenUntilCallback :: FFI.StopListeningCallback -> ProgramLoggerM (CInt)
listenUntilCallback f = do
    fPtr <- newStopListeningCallback f
    sPtr <- getAPIState
    x <- liftS $ FFI.listenUntilCallback sPtr fPtr
    return x

listenUntilElapsed :: CULong -> ProgramLoggerM (CInt)
listenUntilElapsed t = do
        sPtr <- getAPIState
        liftS $ FFI.listenUntilElapsed sPtr t


-- **** cnMsg functions ****

data ProcEventData = Other
                   | NoEvent
                   | ProcStart PidT 
                   | ProcEnd ExitCode

getProcEventData :: Ptr () -> IO (Maybe ProcEventData)
getProcEventData cnHdr
                | cnHdr == nullPtr = return Nothing
                | otherwise = do
                        eventType <- FFI.cnMsgGetProcMatchEventType cnHdr
                        case eventType of
                            Other     -> return Other
                            NoEvent   -> return NoEvent 
                            ProcStart -> cnMsgGetProcessPid cnHdr >>= (return . ProcStart)
                            ProcEnd   -> cnMsgGetExitCode cnHdr >>= (return . ProcEnd)
