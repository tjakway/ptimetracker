module TimeTracker.FFI where

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr

type CBool = CInt

type CEventCallback = CInt -> CInt -> CString -> IO ()

-- TODO: write wrapper functions for StopListeningCallback
-- so we can inspect the Ptr () -- which is a cn_msg* 
type StopListeningCallback = Ptr () -> IO (CBool)

type PidT = CInt
type CProcMatchEventType = CInt

type ExitCode = CInt

--type synonyms, as defined in APIState.h
type CEventCallbackFunPtr = FunPtr (CEventCallback)
type ErrorCallbackFunPtr = FunPtr (CString -> IO ())
type StopListeningCallbackFunPtr = FunPtr (StopListeningCallback)

type APIStatePtr = Ptr ()

-- | returns a void pointer to api state
foreign import ccall "APIState.h initializeAPIState"
    initializeAPIState :: IO (APIStatePtr)

foreign import ccall "APIState.h freeAPIState"
    freeAPIState :: APIStatePtr -> IO ()

foreign import ccall "APIState.h addProcMatcher"
    addProcMatcher :: APIStatePtr -> CEventCallbackFunPtr -> 
                      CString -> CInt -> CString -> IO () -- the CInt is a C99 bool

foreign import ccall "APIState.h execMatches"
    execMatches :: APIStatePtr -> CInt -> CProcMatchEventType -> IO ()

foreign import ccall "APIState.h setAPIStateErrorCallback"
    setAPIStateErrorCallback :: APIStatePtr -> ErrorCallbackFunPtr -> IO ()

foreign import ccall "APIState.h startListening"
    startListening :: APIStatePtr -> IO (CInt)

foreign import ccall "APIState.h listenForMessagesForever"
    listenForMessagesForever :: APIStatePtr -> IO (CInt)

foreign import ccall "APIState.h listenUntilElapsed"
    listenUntilElapsed :: APIStatePtr -> CULong -> IO (CInt)

foreign import ccall "APIState.h listenUntilCallback"
    listenUntilCallback :: APIStatePtr -> StopListeningCallbackFunPtr -> IO (CInt)

foreign import ccall "wrapper"
    wrapCEventCallback :: (CInt -> CInt -> CString -> IO ()) -> IO (CEventCallbackFunPtr)

foreign import ccall "wrapper"
    wrapErrorCallback :: (CString -> IO ()) -> IO (FunPtr (CString -> IO ()))

foreign import ccall "wrapper"
    wrapStopListeningCallback :: StopListeningCallback -> IO (StopListeningCallbackFunPtr)

-- Redirecting log functions

-- | redirect regular-priority log messages
foreign import ccall "APIState.h apiSetOutFd"
    apiSetOutFd :: APIStatePtr -> CInt -> IO ()

-- | redirect error-priority log messages
foreign import ccall "APIState.h apiSetErrFd"
    apiSetErrFd :: APIStatePtr -> CInt -> IO ()


-- cnMsg functions
foreign import ccall "APIState.h cnMsgGetProcMatchEventType"
    cnMsgGetProcMatchEventType :: Ptr () -> IO CInt

foreign import ccall "APIState.h cnMsgGetProcessPid"
    cnMsgGetProcessPid :: Ptr () -> IO PidT

foreign import ccall "APIState.h cnMsgGetExitCode"
    cnMsgGetExitCode :: Ptr () -> IO ExitCode


-- TODO: work this into the FFI

--matches the C enum of the same name
data ProcMatchEventType = Other
                        | NoEvent 
                        | ProcStart 
                        | ProcEnd
                        deriving (Eq)

procMatchEventTypeToInt :: ProcMatchEventType -> CProcMatchEventType
procMatchEventTypeToInt Other     = -2
procMatchEventTypeToInt NoEvent   = -1
procMatchEventTypeToInt ProcStart =  1
procMatchEventTypeToInt ProcEnd   =  2

intToProcMatchEventType :: CProcMatchEventType -> Maybe ProcMatchEventType
intToProcMatchEventType i = case i of
                                -2 -> Just Other
                                -1 -> Just NoEvent
                                1  -> Just ProcStart
                                2  -> Just ProcEnd
                                _  -> Nothing

