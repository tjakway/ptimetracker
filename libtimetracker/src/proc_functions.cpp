#include "proc_functions.hpp"
#include "Util.h"

#include <vector>
#include <string>
#include <sstream>
#include <fstream>
#include <cstdlib>
#include <regex>
#include <memory>
#include <stdexcept>
#include <errno.h>


namespace {

    //see https://stackoverflow.com/questions/6159665/a-standard-way-in-c-to-define-an-exception-class-and-to-throw-exceptions
    NEW_EXCEPTION_TYPE(RealpathException);
}

namespace ptimetracker {
    const std::regex::flag_type ProcMatcher::REGEX_FLAGS = 
        std::regex::egrep | std::regex::optimize;

    ProcMatcher::ProcMatcher(EventCallback callback, const char* procRegexStr, bool matchOnlyProgName, const char* cwdRegexStr)
        : eventCallback(callback), matchOnlyProgName(matchOnlyProgName)
    {
        if(procRegexStr != nullptr) {
            procRegex = std::unique_ptr<std::regex>(new std::regex(procRegexStr, 
                        REGEX_FLAGS));
        }

        if(cwdRegexStr != nullptr) {
            cwdRegex = std::unique_ptr<std::regex>(new std::regex(cwdRegexStr,
                        REGEX_FLAGS));
        }
    }

    bool ProcMatcher::procMatches(void* state, pid_t pid, ProcInfo* info)
    {
        if(procRegex.get() == nullptr) {
            return true;
        }
        else {
            const auto progName = ProcInfo::readProcName(state, pid, info);

            //check whether we're testing just the program name or the entire invocation
            if(matchOnlyProgName) {
                return regexMatch(progName, *procRegex);
            }
            else {
                const auto fullCmdLine = progName + " " + 
                    ProcInfo::readCmdLine(state, pid, info);
                return regexMatch(fullCmdLine, *procRegex);
            }
        }
    }

    bool ProcMatcher::cwdMatches(void* state, pid_t pid, ProcInfo* info)
    {
        if(cwdRegex.get() == nullptr) {
            return true;
        }
        else {
            return regexMatch(ProcInfo::readCwd(state, pid), *cwdRegex);
        }
    }


    bool ProcMatcher::matches(void* state, pid_t pid, ProcInfo* info)
    {
        return procMatches(state, pid, info) && cwdMatches(state, pid, info);
    }

    /**
     * optionally takes a pointer to cache state
     */
    void ProcMatcher::execMatch(void* state, pid_t pid, ProcMatchEventType eventType, ProcInfo* info)
    {
        if(matches(state, pid, info)) {
            if(info->procName.get() != nullptr)
            {
                eventCallback(pid, eventType, info->procName.get()->c_str());
            }
            else 
            {
                std::string procName = ProcInfo::readProcName(state, pid, nullptr);
                eventCallback(pid, eventType, procName.c_str());
            }
        }
    }


    void ProcInfo::checkRealpathErrno(char* allocedPath) {
        if(allocedPath == nullptr) {
            //there's an error, find out what it is
            //don't just check errno because it might have been set from a previous call

            throw RealpathException(std::string(strerror(errno)));
        }
    }

    std::string ProcInfo::readCmdLine(void* state, pid_t pid, ProcInfo* info)
    {
        if(info != nullptr && info->cmdLine.get() != nullptr) {
            return *info->cmdLine;
        }

        // /proc/<pid>/cmdline is separated by NULLs
        // replace them with spaces
        std::string cmdLine = 
            std::regex_replace(
                    readFile("/proc/" + std::to_string(pid) + "/cmdline"),
                    std::regex(R"(\0)"),
                    " ");

        if(info != nullptr) {
            info->cmdLine = std::unique_ptr<std::string>(new std::string(cmdLine));
        }

        return cmdLine;
    }

    std::string ProcInfo::readProcName(void* state, pid_t pid, ProcInfo* info)
    {
        //check cache
        if(info != nullptr && info->procName.get() != nullptr) {
            return *info->procName;
        }

        std::string procName = readFile("/proc/" + std::to_string(pid) + "/comm");

        if(info != nullptr) {
            info->procName = std::unique_ptr<std::string>(new std::string(procName));
        }

        return procName;
    }

    std::string ProcInfo::readCwd(void* state, pid_t pid, ProcInfo* info)
    {
        //check cache
        if(info != nullptr && info->cwd.get() != nullptr) {
            return *info->cwd;
        }

        //use realpath to read the destination of the symlink as an absolute path
        std::string cwdPath = "/proc/" + std::to_string(pid) + "/cwd";

        std::string absCwd;
        char* allocedPath = nullptr;
        try {
            allocedPath = realpath(cwdPath.c_str(), nullptr);
            checkRealpathErrno(allocedPath);

            absCwd = allocedPath;
            if(info != nullptr) {
                info->cwd = std::unique_ptr<std::string>(new std::string(absCwd));
            }

        } catch(RealpathException const& e)
        {
            auto msg = "Caught RealpathException in " + std::string(__func__)
                    + ", returning empty string\n";
            apiWriteErr(state, msg.c_str());
            absCwd = "";
        }

        //realpath will allocate a c string when the 2nd arg is NULL
        if(allocedPath != nullptr)
        {
            free(allocedPath);
        }

        return absCwd;
    }

}
