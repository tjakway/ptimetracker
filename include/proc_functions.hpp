#ifndef PROC_FUNCTIONS_HPP
#define PROC_FUNCTIONS_HPP

#include "APIState.h"

#include <regex>
#include <memory>

#include <sys/types.h> //for pid_t

namespace ptimetracker {

class ProcInfo;

/**
 * contains all the data to match one process
 */
    //TODO: add a function pointer callback so we can pass a Haskell function that's called whenever it matches
class ProcMatcher
{
    /**
     * cwdRegex is optional
     */
    std::unique_ptr<std::regex> procRegex, cwdRegex;
    const bool matchOnlyProgName;

    bool procMatches(pid_t, ProcInfo*);
    bool cwdMatches(pid_t, ProcInfo*);

    EventCallback eventCallback;

    bool matches(pid_t, ProcInfo*);

public:
    ProcMatcher(EventCallback eventCallback, const char* procRegexStr, bool matchOnlyProgName, const char* cwdRegexStr);

    /**
     * runs the saved callback if it matches this PID
     */
    void execMatch(pid_t, ProcMatchEventType, ProcInfo* info = nullptr);
};


class ProcInfo 
{
    static void checkRealpathErrno(char* allocedPath);
public:
    static std::string readCmdLine(pid_t pid, ProcInfo* info = nullptr);
    static std::string readProcName(pid_t pid, ProcInfo* info = nullptr);
    static std::string readCwd(pid_t pid, ProcInfo* info = nullptr);

    std::unique_ptr<std::string> cmdLine, procName, cwd;
};


}

#endif
