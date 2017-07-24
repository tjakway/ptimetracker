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

extern "C" {
    //TODO: add an error pointer? (char**)
    void addProcMatcher(void* state, )
}

}
