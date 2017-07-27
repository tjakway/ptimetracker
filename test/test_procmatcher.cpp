#include "gtest/gtest.h"
#include "proc_functions.hpp"
#include "Util.h"

#include <iostream>
#include <string>
#include <regex>

#include <cstdlib>

#include <sys/types.h>
#include <unistd.h>
#include <signal.h>
#include <dirent.h>

namespace ptimetracker {

//make sure C++ std::regex works...
TEST(RegexTests, matchAnything)
{
    const std::string regexStr = R"rgx(.*)rgx";
    const std::regex r(regexStr, ProcMatcher::REGEX_FLAGS);

    const std::string testStr("a;sljf12#$lskfj(/");

    ASSERT_TRUE(std::regex_match(testStr, r));

    ASSERT_TRUE(regexMatch(testStr, r));
}

TEST(RegexTests, matchBinTrue)
{
    const std::regex r(R"rgx(\s*/bin/true\s*)rgx", ProcMatcher::REGEX_FLAGS);

    ASSERT_TRUE(std::regex_match("/bin/true", r));
}

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
        ASSERT_TRUE(childCwd == ProcInfo::readCwd(getpid()));

        //signal the child process to continue
        //it will exit normally
        kill(child, SIGCONT);
    }
}

}
