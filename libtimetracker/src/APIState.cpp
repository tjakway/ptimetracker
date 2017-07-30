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

//cn_msg exceptions
NEW_EXCEPTION_TYPE(CnMsgWrongProcEventType);
NEW_EXCEPTION_TYPE(CnMsgNullPointer);

void cnMsgCheckNull(cn_msg* p, const char* funcName)
{
    const std::string fName(funcName);
    if(p == nullptr)
    {
        throw CnMsgNullPointer("null cn_msg* in " + fName);
    }
    if(p->what == nullptr)
    {
        throw CnMsgNullPointer("null cn_msg->what in " + fName);
    }
}

const char* cnMsgProcEventStr(cn_msg* cn_hdr)
{
    cnMsgCheckNull(cn_hdr, __func__);

    struct proc_event ev* = (struct proc_event*)cn_hdr->data;
    switch(ev->what) {
	case PROC_EVENT_FORK: 
                return "PROC_EVENT_FORK";

	case PROC_EVENT_EXEC:
                return "PROC_EVENT_EXEC";
	case PROC_EVENT_EXIT:
                return "PROC_EVENT_EXIT";

	case PROC_EVENT_UID:
                return "PROC_EVENT_UID";
	default:
                return "Unknown";
    }
}

} //namespace ptimetracker

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

    /** cn_msg* functions **/

    enum ProcMatchEventType cnMsgGetProcMatchEventType(cn_msg* cn_hdr)
    {
        cnMsgCheckNull(cn_hdr, __func__);
        struct proc_event ev* = (struct proc_event*)cn_hdr->data;

	switch(ev->what){
	case PROC_EVENT_FORK: //should we track forks?
                return OTHER;

	case PROC_EVENT_EXEC:
                return PROC_START;
	case PROC_EVENT_EXIT:
                return PROC_END;

	case PROC_EVENT_UID:
                return OTHER;
	default:
                return OTHER;
	}
    }

    unsigned int cnMsgGetProcessPid(cn_msg* cn_hdr)
    {
        cnMsgCheckNull(cn_hdr, __func__);

        struct proc_event ev* = (struct proc_event*)cn_hdr->data;
        if(ev->what == PROC_EVENT_EXEC || ev->what == PROC_EVENT_EXIT)
        {
            return ev->event_data.process_pid;
        }
        else
        {
            auto msg = "Wrong event type.  Expected PROC_EVENT_EXEC or PROC_EVENT_EXIT but" +
                " got " + std::string(cnMsgProcEventStr(ev->what));
            throw CnMsgWrongProcEventType(msg)
        }
    }

    unsigned int cnMsgGetExitCode(cn_msg* cn_hdr)
    {
        cnMsgCheckNull(cn_hdr);
        struct proc_event ev* = (struct proc_event*)cn_hdr->data;

        if(ev->what == PROC_EVENT_EXIT)
        {
            return ev->event_data.exit_code;
        }
        else
        {
            auto msg = "Wrong event type.  Expected PROC_EVENT_EXIT but" +
                " got " + std::string(cnMsgProcEventStr(ev->what));
            throw CnMsgWrongProcEventType(msg)
        }
    }
}
