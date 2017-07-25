#include "APIState.h"

#include "proc_functions.hpp"
#include <vector>
#include <algorithm>

namespace ptimetracker {

class APIState 
{
    //don't bother writing getters/setters as members because we're writing them in the extern C block
public:
    std::vector<ProcMatcher> regexes;
    ErrorCallback errorCallback;

    APIState()
        : errorCallback(nullptr),
          regexes()
    {}

    void onError(const char* msg)
    {
        if(errorCallback != nullptr)
        {
            errorCallback(msg);
        }
    }
};

}

extern "C" {
    //TODO: will bool behave properly in C?  don't need to include <stdbool.h>?
    void addProcMatcher(void* state, EventCallback eventCallback, const char* procRegexStr, bool matchOnlyProgName, const char* cwdRegexStr) 
    {
        ptimetracker::APIState* s = (ptimetracker::APIState*)state;
        s->regexes.emplace_back(eventCallback, procRegexStr, matchOnlyProgName, cwdRegexStr);
    }

    void execMatches(void* state, pid_t pid, ProcMatchEventType eventType) 
    {
        ptimetracker::APIState* s = (ptimetracker::APIState*)state;

        //only cache values across one call, both to not waste space and because it's very unlikely the kernel will recycle PIDs
        ptimetracker::ProcInfo info;

        for(ptimetracker::ProcMatcher& m : s->regexes) {
            m.execMatch(pid, eventType, &info);
        }
    }

    void setErrorCallback(void* state, ErrorCallback errorCallback)
    {
        ptimetracker::APIState* s = (ptimetracker::APIState*)state;
        s->errorCallback = errorCallback;
    }

    void* initializeAPIState() 
    {
        ptimetracker::APIState* s = new ptimetracker::APIState();
        return s;
    }

    void freeAPIState(void* s)
    {
        ptimetracker::APIState* state = (ptimetracker::APIState*)s;
        delete state;
    }
}
