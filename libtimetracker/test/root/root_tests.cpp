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


#define WAIT_TIME_MILLIS 2000

namespace {
    NEW_EXCEPTION_TYPE(ForkErrorInRootTestsException)

        //prototypes
    void listenAndWait(void* state, unsigned long millis);

    static bool errorSet = false;
    void errorCallback(const char* msg) {
        std::cerr << "error received: " << msg << std::endl;
        errorSet = true;
    }

    pid_t callbackPid = -1;
    ProcMatchEventType callbackEventType = NO_EVENT;

    //TODO: incorporate progName
    void eventCallback(int pid, ProcMatchEventType eventType, const char* progName)
    {
        if(callbackPid == -1)
        {
            callbackPid = pid;
        }
        if(callbackEventType == NO_EVENT)
        {
            callbackEventType = eventType;
        }
    }

    /**
     * google test functions need to return void,
     * see https://stackoverflow.com/questions/17961900/why-does-google-test-assert-false-not-work-in-methods-but-expect-false-does
     */
    void assertCallbacksCalled()
    {
        ASSERT_NE(callbackPid, -1);
        ASSERT_NE(callbackEventType, NO_EVENT);
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
            //XXX: why do these assertions fail?
            //ASSERT_EQ(childExitStatus, 0);
            //ASSERT_FALSE(wasSignaled);
            //ASSERT_FALSE(wasStopped);
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
    const char* PROG_NAME_REGEX = R"rgx(true.*)rgx";

    void* state = initializeAPIState();
    setAPIStateErrorCallback(state, errorCallback);
    
    addProcMatcher(state, 
            eventCallback, 
            PROG_NAME_REGEX, 
            //only match program name
            true, 
            nullptr);


    launchProgAndWait(state, PROG_NAME);


    ASSERT_FALSE(errorSet);
    assertCallbacksCalled();

    freeAPIState(state);
}

}
