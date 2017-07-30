#include "APIState.h"

#include "proc_functions.hpp"
#include "exec-notify.h"
#include "Util.h"
#include <vector>
#include <algorithm>
#include <mutex>

#include <unistd.h>
#include <cstdlib>
#include <cstring>
#include <errno.h>

namespace ptimetracker {

NEW_EXCEPTION_TYPE(WriteSyscallException);

class APIState 
{
    int outFd, errFd;
    std::mutex outFdMutex, errFdMutex;

    /**
     * must be called before another syscall
     */
    void checkWriteCall(ssize_t ret)
    {
        if(ret < 0)
        {
            const std::string errMsg(strerror(errno));
            throw WriteSyscallException("Error in write syscall: " + errMsg);
        }
    }

    //don't bother writing getters/setters as members because we're writing them in the extern C block
public:
    std::vector<ProcMatcher> regexes;
    ErrorCallback errorCallback;

    APIState()
        : errorCallback(nullptr),
          regexes()
    {
        outFd = dup(STDOUT_FILENO);
        errFd = dup(STDERR_FILENO);
    }

    void onError(const char* msg)
    {
        if(errorCallback != nullptr)
        {
            errorCallback(msg);
        }
    }

    // ***Logging functions***

    //see http://en.cppreference.com/w/cpp/thread/lock_guard
    void setOutFd(int newFd)
    {
        std::lock_guard<std::mutex> lock(outFdMutex);
        outFd = dup2(outFd, newFd);
    }

    void setErrFd(int newFd)
    {
        std::lock_guard<std::mutex> lock(errFdMutex);
        errFd = dup2(errFd, newFd);
    }

    /**
     * **WARNING** NOT SAFE
     * s must be null-terminated
     */
    void writeLog(const char* s)
    {
        std::lock_guard<std::mutex> lock(outFdMutex);

        checkWriteCall(write(outFd, s, strlen(s)));
    }

    void writeLog(std::string s)
    {
        writeLog(s.c_str());
    }

    /**
     * **WARNING** NOT SAFE
     * s must be null-terminated
     */
    void writeErr(const char* s)
    {
        std::lock_guard<std::mutex> lock(errFdMutex);

        checkWriteCall(write(errFd, s, strlen(s)));
    }

    void writeErr(std::string s)
    {
        writeErr(s.c_str());
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
            m.execMatch(state, pid, eventType, &info);
        }
    }

    void setAPIStateErrorCallback(void* state, ErrorCallback errorCallback)
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

    /**
     * begin listening for proc events in a loop
     * This function won't return until the process ends
     */
    int startListening(void* state)
    {
        return 1;
            //TODO
        //return register_proc_msg_handler(state);
    }


    //threadsafe
    void apiSetOutFd(void* s, int newFd)
    {
        ptimetracker::APIState* state = (ptimetracker::APIState*)s;
        state->setOutFd(newFd);
    }

    //threadsafe
    void apiSetErrFd(void* s, int newFd)
    {
        ptimetracker::APIState* state = (ptimetracker::APIState*)s;
        state->setErrFd(newFd);
    }

    //probably won't need these
    void apiWriteLog(void* s, const char* msg)
    {
        ptimetracker::APIState* state = (ptimetracker::APIState*)s;
        state->writeLog(msg);
    }

    void apiWriteErr(void* s, const char* msg)
    {
        ptimetracker::APIState* state = (ptimetracker::APIState*)s;
        state->writeErr(msg);
    }
}
