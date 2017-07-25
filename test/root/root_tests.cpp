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
    void errorCallback() {
        errorSet = true;
    }

    static pid_t callbackPid = -1;
    static std::unique_ptr<ProcMatchEventType> callbackEventType;

    void eventCallback(pid_t pid, ProcMatchEventType eventType)
    {
        callbackPid = pid;
        callbackEventType = make_unique<ProcMatchEventType>(eventType);
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
            exec(path);
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
            PROG_NAME_REGEX.c_str(), 
            //ignore argv
            true, 
            nullptr);

    launchProg(PROG_NAME);

    ASSERT_TRUE(callbacksCalled());

    freeAPIState(state);
}

}
