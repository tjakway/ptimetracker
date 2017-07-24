#include "gtest/gtest.h"
#include "proc_functions.hpp"

#include <iostream>
#include <string>

#include <cstdlib>

#include <sys/types.h>
#include <unistd.h>
#include <signal.h>
#include <dirent.h>

namespace {
    class OpenDirException : public std::runtime_error
    {
    public:
        OpenDirException(std::string const& dir)
            : std::runtime_error("Error while opening " + dir)
        {}
    };

    bool dirExists(std::string dirName)
    {
        DIR* dir = opendir(dirName.c_str());
        if(dir) {
            return true;
        } else if (errno == ENOENT) {
            return false;
        } else {
            throw OpenDirException(dirName);
        }
    }

    std::string getCwd()
    {
        const long size = pathconf(".", _PC_PATH_MAX);
        const char* cwd = new char[size];

        const char* ret = getcwd(cwd, (size_t)size);

        std::string retStr(cwd);

        delete[] cwd;

        return retStr;
    }
}

namespace ptimetracker {

TEST(pTimeTrackerTests, testCwd)
{
    pid_t pid = fork();
    if(pid == -1) {
        std::cerr << "Error in fork, pid = -1" << std::endl;
        ASSERT_TRUE(false);
    }
    else if(pid == 0) {
        //we're the child process
        kill(getpid(), SIGSTOP);
        exit(1);
    } else {
        pid_t child = pid;

        //run assertions
        auto childCwd = ProcInfo::readCwd(child);

        ASSERT_TRUE(dirExists(childCwd));
        ASSERT_TRUE(childCwd == getCwd());

        //signal the child process to continue
        //it will exit normally
        kill(child, SIGCONT);
    }
}

}
