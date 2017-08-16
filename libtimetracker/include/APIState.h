#ifndef APISTATE_H
#define APISTATE_H

#include <unistd.h>
#include <fcntl.h>
#include <signal.h>

#include <sys/socket.h>
#include <sys/types.h>

#include <linux/connector.h>
#include <linux/netlink.h>
#include <linux/cn_proc.h>

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
        OTHER=-2, //events we don't track
        NO_EVENT=-1,
        PROC_START=1,
        PROC_END=2,
    };

    /**
     * pass the pid, event type, and program name
     */
    typedef void (*EventCallback)(int, enum ProcMatchEventType, const char*);
    typedef void (*ErrorCallback)(const char*);

    void addProcMatcher(void* state, EventCallback eventCallback,
            const char* procRegexStr, bool matchOnlyProgName, const char* cwdRegexStr);

    void execMatches(void*, pid_t, enum ProcMatchEventType);

    void setAPIStateErrorCallback(void*, ErrorCallback);

    void* initializeAPIState();

    void freeAPIState(void*);

    //listening functions
    int listenForMessagesForever(void*);
    int listenUntilElapsed(void*, unsigned long);
    int listenUntilCallback(void*, bool (*callback)(struct cn_msg*));

    void apiSetOutFd(void*, int newFd);
    void apiSetErrFd(void*, int newFd);

    void apiWriteLog(void*, const char*);
    void apiWriteErr(void*, const char*);


    /** Functions to extract data from cn_msg* **/
    /** note that not all will be valid at once!
     * (depends on cn_hdr->data->what)
     */
    enum ProcMatchEventType cnMsgGetProcMatchEventType(struct cn_msg*);

    //only used in PROC_EVENT_FORK
//    unsigned int cnMsgGetParentPid(cn_msg*);
//    unsigned int cnMsgGetChildPid(cn_msg*);

    unsigned int cnMsgGetProcessPid(struct cn_msg*);
    unsigned int cnMsgGetExitCode(struct cn_msg*);
#ifdef __cplusplus
}
#endif

#endif
