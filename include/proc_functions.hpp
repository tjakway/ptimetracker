#ifndef PROC_FUNCTIONS_HPP
#define PROC_FUNCTIONS_HPP

#include <regex>
#include <memory>

#include <sys/types.h> //for pid_t

namespace ptimetracker {

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

    bool procMatches(pid_t);
    bool cwdMatches(pid_t);

    const void (*callback)(int);

    bool matches(pid_t);

public:
    ProcMatcher(const void (*callback)(int), const char* procRegexStr, bool matchOnlyProgName, const char* cwdRegexStr);

    /**
     * runs the saved callback if it matches this PID
     */
    void execMatch(pid_t);
};

}

#endif
