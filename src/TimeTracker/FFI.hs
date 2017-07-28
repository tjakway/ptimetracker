module TimeTracker.FFI where

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr

type CBool = CInt

type EventCallback = CInt -> CInt -> IO ()

--type synonyms, as defined in APIState.h
type EventCallbackFunPtr = FunPtr (CInt -> CInt -> IO ())
type ErrorCallbackFunPtr = FunPtr (CString -> IO ())
type StopListeningCallback = FunPtr (Ptr CInt -> IO (CBool))

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
    listenUntilCallback :: APIStatePtr -> StopListeningCallback -> IO (CInt)

foreign import ccall "wrapper"
    wrapEventCallback :: (CInt -> CInt -> IO ()) -> IO (FunPtr (CInt -> CInt -> IO ()))

foreign import ccall "wrapper"
    wrapErrorCallback :: (CString -> IO ()) -> IO (FunPtr (CString -> IO ()))

foreign import ccall "wrapper"
    wrapStopListeningCallback :: (Ptr CInt -> IO (CBool)) -> IO (FunPtr (Ptr CInt -> IO (CBool)))


--matches the C enum of the same name
data ProcMatchEventType = NoEvent 
                        | ProcStart 
                        | ProcEnd

procMatchEventTypeToInt :: ProcMatchEventType -> CProcMatchEventType
procMatchEventTypeToInt NoEvent   = -1
procMatchEventTypeToInt ProcStart =  1
procMatchEventTypeToInt ProcEnd   =  2
