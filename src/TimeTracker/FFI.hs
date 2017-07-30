module TimeTracker.FFI where

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr

type CBool = CInt

type EventCallback = CInt -> CInt -> CString -> IO ()

-- TODO: write wrapper functions for StopListeningCallback
-- so we can inspect the Ptr () -- which is a cn_msg* 
type StopListeningCallback = Ptr () -> IO (CBool)

--type synonyms, as defined in APIState.h
type EventCallbackFunPtr = FunPtr (EventCallback)
type ErrorCallbackFunPtr = FunPtr (CString -> IO ())
type StopListeningCallbackFunPtr = FunPtr (StopListeningCallback)

type APIStatePtr = Ptr ()
type PidT = CInt
type CProcMatchEventType = CInt

-- | returns a void pointer to api state
foreign import ccall "APIState.h initializeAPIState"
    initializeAPIState :: IO (APIStatePtr)

foreign import ccall "APIState.h freeAPIState"
    freeAPIState :: APIStatePtr -> IO ()

foreign import ccall "APIState.h addProcMatcher"
    addProcMatcher :: APIStatePtr -> EventCallbackFunPtr -> 
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
    wrapEventCallback :: (CInt -> CInt -> IO ()) -> IO (FunPtr (CInt -> CInt -> IO ()))

foreign import ccall "wrapper"
    wrapErrorCallback :: (CString -> IO ()) -> IO (FunPtr (CString -> IO ()))

foreign import ccall "wrapper"
    wrapStopListeningCallback :: StopListeningCallback -> IO (StopListeningCallbackFunPtr)


-- cnMsg functions
foreign import ccall "APIState.h cnMsgGetProcMatchEventType"
    cnMsgGetProcMatchEventType :: Ptr () -> IO CInt

foreign import ccall "APIState.h cnMsgGetProcessPid"
    cnMsgGetProcessPid :: Ptr () -> IO CUInt

foreign import ccall "APIState.h cnMsgGetExitCode"
    cnMsgGetExitCode :: Ptr () -> IO CUInt


-- TODO: work this into the FFI

--matches the C enum of the same name
data ProcMatchEventType = Other
                        | NoEvent 
                        | ProcStart 
                        | ProcEnd

procMatchEventTypeToInt :: ProcMatchEventType -> CProcMatchEventType
procMatchEventTypeToInt NoEvent   = -1
procMatchEventTypeToInt ProcStart =  1
procMatchEventTypeToInt ProcEnd   =  2
