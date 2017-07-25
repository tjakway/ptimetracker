#ifndef APISTATE_H
#define APISTATE_H

#include <sys/types.h> //for pid_t

#ifndef __cplusplus
#include <stdbool.h>
#endif

#ifdef __cplusplus
namespace ptimetracker {


/**
 * opaque class to hold all the state the C API needs
 */
class APIState;

}

extern "C" {
#endif

    enum ProcMatchEventType {
        PROC_START=1,
        PROC_END=2
    };

    typedef const void (*EventCallback)(int, enum ProcMatchEventType);
    typedef void (*ErrorCallback)(const char*);

    void addProcMatcher(void* state, EventCallback eventCallback,
            const char* procRegexStr, bool matchOnlyProgName, const char* cwdRegexStr);

    void execMatches(void*, pid_t, enum ProcMatchEventType);

    void setAPIStateErrorCallback(void*, ErrorCallback);

    void* initializeAPIState();

    void freeAPIState(void*);

    int startListening(void*);

#ifdef __cplusplus
}
#endif

#endif
