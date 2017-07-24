#ifndef PROC_FUNCTIONS_HPP
#define PROC_FUNCTIONS_HPP

#include <regex>
#include <string>

#include <sys/types.h> //for pid_t

namespace ptimetracker {

/**
 * contains all the data to match one process
 */
class ProcMatcher
{
    /**
     * cwdRegex is optional
     */
    const std::regex procRegex, cwdRegex;
    const bool matchOnlyProgName;

public:
    ProcMatcher(std::regex procRegex, bool matchOnlyProgName, std::regex cwdRegex)
        : cwdRegex(cwdRegex), procRegex(procRegex), matchOnlyProgName(matchOnlyProgName)
        {}

    ProcMatcher(std::regex procRegex, bool matchOnlyProgName)
        : procRegex(procRegex), matchOnlyProgName(matchOnlyProgName), cwdRegex()
        {}

    /**
     * ought to be read lazily
     */
    bool matches(pid_t);
};

}

#endif
