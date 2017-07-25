#ifndef APISTATE_H
#define APISTATE_H

namespace ptimetracker {


/**
 * opaque class to hold all the state the C API needs
 */
class APIState;

}

extern "C" {
    enum ProcMatchEventType {
        PROC_START=1,
        PROC_END=2
    };

    typedef const void (*EventCallback)(int, int);
    typedef void (*ErrorCallback)(const char*);

    void setErrorCallback(void* state, ErrorCallback errorCallback);

    void addProcMatcher(void* state, EventCallback eventCallback,
            const char* procRegexStr, bool matchOnlyProgName, const char* cwdRegexStr);
}

#endif
