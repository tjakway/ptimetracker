#ifndef PROC_FUNCTIONS_HPP
#define PROC_FUNCTIONS_HPP

#include <regex>
#include <string>

#include <sys/types.h> //for pid_t

namespace ptimetracker {

class ProcMatcher
{
    const std::regex cwdRegex, procRegex;
    const bool matchOnlyProgName;

public:
    ProcMatcher(std::regex cwdRegex, std::regex procRegex, bool matchOnlyProgName)
        : cwdRegex(cwdRegex), procRegex(procRegex), matchOnlyProgName(matchOnlyProgName)
        {}
};


}

#endif
