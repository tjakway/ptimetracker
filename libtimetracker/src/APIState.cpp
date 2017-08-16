#include "APIState.h"

#include "proc_functions.hpp"
#include "exec-notify.h"
#include "Util.h"
#include <vector>
#include <algorithm>
#include <mutex>
#include <string>

#include <cstdlib>
#include <cstring>
#include <errno.h>

#include <unistd.h>
#include <fcntl.h>
#include <signal.h>

#include <sys/socket.h>
#include <sys/types.h>

#include <linux/connector.h>
#include <linux/netlink.h>
#include <linux/cn_proc.h>

namespace ptimetracker {

NEW_EXCEPTION_TYPE(WriteSyscallException);
NEW_EXCEPTION_TYPE(CloseFdException);

}

namespace {
    void closeOrThrow(const int fd, const std::string& fdName)
    {
        if(close(fd) != 0)
        {
            const std::string errMsg(strerror(errno));
            throw ptimetracker::CloseFdException("Error while closing file descriptor: " 
                    + fdName);
        }
    }
}


namespace ptimetracker {

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
        closeOrThrow(outFd, "outFd");
        outFd = newFd;
    }

    void setErrFd(int newFd)
    {
        std::lock_guard<std::mutex> lock(errFdMutex);
        closeOrThrow(errFd, "errFd");
        errFd = newFd;
    }

    /**
     * **WARNING** NOT SAFE: s must be null-terminated
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
     * **WARNING** NOT SAFE: s must be null-terminated
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

    proc_event *ev = (struct proc_event*)p->data;
    if(ev == nullptr)
    {
        throw CnMsgNullPointer("null cn_msg->data in " + fName);
    }
}

const char* cnMsgProcEventStr(cn_msg* cn_hdr)
{
    ptimetracker::cnMsgCheckNull(cn_hdr, __func__);

    proc_event *ev = (struct proc_event*)cn_hdr->data;
    switch(ev->what) {
        case proc_event::PROC_EVENT_FORK: 
                return "PROC_EVENT_FORK";

	case proc_event::PROC_EVENT_EXEC:
                return "PROC_EVENT_EXEC";
	case proc_event::PROC_EVENT_EXIT:
                return "PROC_EVENT_EXIT";

	case proc_event::PROC_EVENT_UID:
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
        ptimetracker::cnMsgCheckNull(cn_hdr, __func__);
        struct proc_event *ev = (struct proc_event*)cn_hdr->data;

	switch(ev->what){
	case proc_event::PROC_EVENT_FORK: //should we track forks?
                return OTHER;

	case proc_event::PROC_EVENT_EXEC:
                return PROC_START;
	case proc_event::PROC_EVENT_EXIT:
                return PROC_END;

	case proc_event::PROC_EVENT_UID:
                return OTHER;
	default:
                return OTHER;
	}
    }

    unsigned int cnMsgGetProcessPid(cn_msg* cn_hdr)
    {
        ptimetracker::cnMsgCheckNull(cn_hdr, __func__);

        struct proc_event *ev = (struct proc_event*)cn_hdr->data;
        if(ev->what == proc_event::PROC_EVENT_EXEC)
        {
            return ev->event_data.exec.process_pid;
        }
        else if(ev->what == proc_event::PROC_EVENT_EXIT)
        {
            return ev->event_data.exit.process_pid;
        }
        else
        {
            std::string msg = std::string("Wrong event type.  Expected PROC_EVENT_EXEC or PROC_EVENT_EXIT but")
                + std::string(" got ")
                + std::string(ptimetracker::cnMsgProcEventStr(cn_hdr));
            throw ptimetracker::CnMsgWrongProcEventType(msg);
        }
    }

    unsigned int cnMsgGetExitCode(cn_msg* cn_hdr)
    {
        ptimetracker::cnMsgCheckNull(cn_hdr, __func__);
        struct proc_event *ev = (struct proc_event*)cn_hdr->data;

        if(ev->what == proc_event::PROC_EVENT_EXIT)
        {
            return ev->event_data.exit.exit_code;
        }
        else
        {
            auto msg = std::string("Wrong event type.  Expected PROC_EVENT_EXIT but") +
                std::string(" got ") + std::string(ptimetracker::cnMsgProcEventStr(cn_hdr));
            throw ptimetracker::CnMsgWrongProcEventType(msg);
        }
    }
}
