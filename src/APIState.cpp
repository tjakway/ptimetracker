#include "APIState.h"

#include "proc_functions.hpp"
#include <vector>

namespace ptimetracker {

class APIState 
{
    //don't bother writing getters/setters as members because we're writing them in the extern C block
public:
    std::vector<ProcMatcher> regexes;
};

}

extern "C" {
    //TODO: add an error pointer? (char**)
    //TODO: will bool behave properly in C?  don't need to include <stdbool.h>?
    void addProcMatcher(void* state, const char* procRegexStr, bool matchOnlyProgName, const char* cwdRegexStr) 
    {
        ptimetracker::APIState* s = (ptimetracker::APIState*)state;
        s->regexes.emplace_back(procRegexStr, matchOnlyProgName, cwdRegexStr);
    }
}
