#ifndef APISTATE_H
#define APISTATE_H

namespace ptimetracker {


/**
 * opaque class to hold all the state the C API needs
 */
class APIState;

}

extern "C" {
    void addProcMatcher(void* state, const void (*callback)(int), const char* procRegexStr, bool matchOnlyProgName, const char* cwdRegexStr);
}

#endif
