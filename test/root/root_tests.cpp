#include "gtest/gtest.h"
#include "proc_functions.hpp"
#include "Util.h"

#include <iostream>
#include <string>
#include <memory>

#include <cstdlib>

#include <sys/types.h>
#include <unistd.h>
#include <signal.h>
#include <dirent.h>


namespace {
    static bool errorSet = false;
    void errorCallback(const char* msg) {
        std::cerr << "error received: " << msg << std::endl;
        errorSet = true;
    }

    static pid_t callbackPid = -1;
    static std::unique_ptr<ProcMatchEventType> callbackEventType;

    void eventCallback(int pid, ProcMatchEventType eventType)
    {
        callbackPid = pid;
        ProcMatchEventType* t = new ProcMatchEventType;
        *t = eventType;
        callbackEventType = std::unique_ptr<ProcMatchEventType>(t);
    }

    bool callbacksCalled()
    {
        return callbackPid != -1 && 
            (callbackEventType != nullptr && *callbackEventType != 0);
    }

    void launchProg(const char* path)
    {
        pid_t pid = fork();

        ASSERT_TRUE(pid > -1);

        if(pid > 0)
        {
            //TODO: fix this race condition...
            sleep(1);
            execl(path, (const char*)nullptr, (char*)nullptr);
        }
    }

    void signalSelf(int seconds)
    {
        const pid_t selfPid = getpid();

        pid_t pid = fork();
        ASSERT_TRUE(pid > -1);

        //if we're not the parent...
        if(pid > 0)
        {
            //wait a duration then break the parent out of the wake loop
            sleep(seconds);
            kill(selfPid, SIGINT);
        }
    }
}

namespace ptimetracker {

TEST(pTimeTrackerRootTests, testLaunchTrue)
{
    const char* PROG_NAME = "/bin/true";
    const char* PROG_NAME_REGEX = PROG_NAME;

    void* state = initializeAPIState();
    setAPIStateErrorCallback(state, errorCallback);
    
    addProcMatcher(state, 
            eventCallback, 
            PROG_NAME_REGEX, 
            //ignore argv
            true, 
            nullptr);

    //set up to break us out of the wait loop
    signalSelf(2);

    launchProg(PROG_NAME);

    int rc = startListening(state);

    ASSERT_FALSE(errorSet);
    ASSERT_TRUE(callbacksCalled());

    freeAPIState(state);
}

}
