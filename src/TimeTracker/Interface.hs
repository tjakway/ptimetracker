{-# LANGUAGE ScopedTypeVariables #-}
module TimeTracker.Interface where

import qualified TimeTracker.FFI as FFI
import TimeTracker.ProcEventType
import TimeTracker.Types
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Marshal.Alloc (free)
import Control.Monad.State.Strict

-- a haskell-friendly version of CEventCallback, with the C types
-- marshalled
type EventCallback = Int -> Int -> String -> IO ()

-- Automatically handles marshalling of the passed name
newEventCallback :: EventCallback -> ProgramLoggerM FFI.CEventCallbackFunPtr
newEventCallback f = do
    s <- get
    fptr <- liftIO $ FFI.wrapCEventCallback f'
    let eventCallbacks' = fptr : (eventCallbacks s)
    put (s { eventCallbacks = eventCallbacks' })
    return fptr

    where f' :: (CInt -> CInt -> CString -> IO ())
          f' a b c = peekCString c >>= f (fromIntegral a) (fromIntegral b)

newStopListeningCallback :: FFI.StopListeningCallback -> 
                            ProgramLoggerM FFI.StopListeningCallbackFunPtr
newStopListeningCallback f = do
    s <- get
    fptr <- liftIO $ FFI.wrapStopListeningCallback f
    let stopListeningCallbacks' = fptr : (stopListeningCallbacks s)
    put (s { stopListeningCallbacks = stopListeningCallbacks' })
    return fptr


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
        liftIO $ (FFI.addProcMatcher sPtr fPtr procRegexCStr matchOnlyProgName' cwdRegexCStr 
                >> free procRegexCStr
                >> free cwdRegexCStr)

    where marshall :: ProgramLoggerM (CString, CInt, CString)
          marshall = do
            procRegexCStr <- liftIO $ newCString procRegex
            cwdRegexCStr  <- liftIO $ newCString cwdRegex
            let matchOnlyProgName' = boolToCInt matchOnlyProgName
            return (procRegexCStr, matchOnlyProgName', cwdRegexCStr)

listenForever :: ProgramLoggerM (CInt)
listenForever = getAPIState >>= liftIO . FFI.listenForMessagesForever

listenUntilCallback :: FFI.StopListeningCallback -> ProgramLoggerM (CInt)
listenUntilCallback f = do
    fPtr <- newStopListeningCallback f
    sPtr <- getAPIState
    x <- liftIO $ FFI.listenUntilCallback sPtr fPtr
    return x

listenUntilElapsed :: CULong -> ProgramLoggerM (CInt)
listenUntilElapsed t = do
        sPtr <- getAPIState
        liftIO $ FFI.listenUntilElapsed sPtr t


-- **** cnMsg functions ****

type TickEventId = Int

data ProcEventData = Other
                   | NoEvent
                   | ProcStart FFI.PidT 
                   | ProcEnd FFI.ExitCode
                   | Tick TickEventId

isTickEvent :: ProcEventData -> Bool
isTickEvent (Tick _) = True
isTickEvent _ = False

typeOfProcEventData :: ProcEventData -> ProcEventType
typeOfProcEventData Other         = ProcEventType "Other"   1
typeOfProcEventData NoEvent       = ProcEventType "NoEvent" 2
typeOfProcEventData (ProcStart _) = ProcEventType "ProcStart" 3
typeOfProcEventData (ProcEnd _)   = ProcEventType "ProcStart" 4
typeOfProcEventData (Tick eventId) = ProcEventType "Tick" eventId

-- explicit show instance to ignore constructor args
instance Show ProcEventData where
        show = procEventTypeName . typeOfProcEventData

-- can't make it an enum because it has non-nullary constructors
-- (this is just fromEnum)
procEventDataToInt :: ProcEventData -> Int
procEventDataToInt = procEventTypeId . typeOfProcEventData

getProcEventData :: Ptr () -> IO (Maybe ProcEventData)
getProcEventData cnHdr
                | cnHdr == nullPtr = return Nothing
                | otherwise = do
                        maybeEventType <- FFI.intToProcMatchEventType <$> 
                                            FFI.cnMsgGetProcMatchEventType cnHdr
                        case maybeEventType of
                            Just e ->  Just <$> unpack e
                            Nothing -> return Nothing

    where unpack e = case e of
                        FFI.Other     -> return Other
                        FFI.NoEvent   -> return NoEvent 
                        FFI.ProcStart -> FFI.cnMsgGetProcessPid cnHdr >>= (return . ProcStart)
                        FFI.ProcEnd   -> FFI.cnMsgGetExitCode cnHdr >>= (return . ProcEnd)
