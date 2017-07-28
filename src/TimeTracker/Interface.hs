{-# LANGUAGE ScopedTypeVariables #-}
module TimeTracker.Interface where

import qualified TimeTracker.FFI as FFI
import TimeTracker.Types
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Alloc (free)
import Foreign.Ptr (freeHaskellFunPtr, nullPtr)

newEventCallback :: FFI.EventCallback -> ProgramLoggerM FFI.EventCallbackFunPtr
newEventCallback f = ProgramLoggerM $ \s -> do
    fptr <- FFI.wrapEventCallback f
    let eventCallbacks' = fptr : (eventCallbacks s)
    return (fptr, s { eventCallbacks = eventCallbacks' } )


-- standards-conformant
-- see https://stackoverflow.com/questions/5369770/bool-to-int-conversion
boolToCInt :: Bool -> CInt
boolToCInt True  = 1
boolToCInt False = 0


addProcMatcher :: FFI.EventCallback -> String -> Bool -> String -> ProgramLoggerM ()
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
