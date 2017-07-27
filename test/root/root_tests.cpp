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


#define WAIT_TIME_MILLIS 5000

namespace {
    NEW_EXCEPTION_TYPE(ForkErrorInRootTestsException)

        //prototypes
    void listenAndWait(void* state, unsigned long millis);

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

    void launchProgAndWait(void* state, const char* path)
    {
        pid_t pid = fork();

        if(pid == 0)
        {
            //child should wait until we're sure the parent is listening then launch the program
            //
            //TODO: fix this race condition...
            sleep(1);
            execl(path, path, (char*)nullptr);

        } else if(pid > 0) {
            //parent should assert the program was launched
            listenAndWait(state, WAIT_TIME_MILLIS);

            std::clog << "Done waiting" << std::endl;

            //make sure the child has exited
            const pid_t childPid = pid;
            int status;
            const pid_t retPid = waitpid(childPid, &status, 0);

            const int childExitStatus = WEXITSTATUS(childPid);
            const bool wasSignaled = WIFSIGNALED(childPid);
            const bool wasStopped = WIFSTOPPED(childPid);

            ASSERT_EQ(childPid, retPid);
            //make sure the child process exited normally
            ASSERT_EQ(childExitStatus, 0);
            ASSERT_FALSE(wasSignaled);
            ASSERT_FALSE(wasStopped);
        } else {
            throw ForkErrorInRootTestsException("in launchProgAndWait");
        }
    }

    void listenAndWait(void* state, unsigned long millis)
    {
        ASSERT_EQ(listenUntilElapsed(state, millis), 0);
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


    launchProgAndWait(state, PROG_NAME);


    ASSERT_FALSE(errorSet);
    ASSERT_TRUE(callbacksCalled());

    freeAPIState(state);
}

}
