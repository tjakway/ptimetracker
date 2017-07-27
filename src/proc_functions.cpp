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
    std::string readFile(std::string inFileName)
    {
        //see https://stackoverflow.com/questions/2602013/read-whole-ascii-file-into-c-stdstring

        std::ifstream inFile;
        //open the input file
        inFile.open(inFileName);

        std::stringstream strStream;
        strStream << inFile.rdbuf();//read the file
        return strStream.str();//str holds the content of the file
        //destructor will close the file
    }

    //see https://stackoverflow.com/questions/6159665/a-standard-way-in-c-to-define-an-exception-class-and-to-throw-exceptions
    class RealpathException : public std::runtime_error
    {
    public:
        RealpathException(std::string const& message)
            : std::runtime_error(message + " Was thrown")
        {}
    };

}

namespace ptimetracker {
    ProcMatcher::ProcMatcher(EventCallback callback, const char* procRegexStr, bool matchOnlyProgName, const char* cwdRegexStr)
        : eventCallback(callback), matchOnlyProgName(matchOnlyProgName)
    {
        if(procRegexStr != nullptr) {
            procRegex = std::unique_ptr<std::regex>(new std::regex(procRegexStr));
        }

        if(cwdRegexStr != nullptr) {
            cwdRegex = std::unique_ptr<std::regex>(new std::regex(cwdRegexStr));
        }
    }

    bool ProcMatcher::procMatches(pid_t pid, ProcInfo* info)
    {
        if(procRegex.get() == nullptr) {
            return true;
        }
        else {
            //check whether we're testing just the program name or the entire invocation
            if(matchOnlyProgName) {
                return std::regex_match(ProcInfo::readProcName(pid, info), *procRegex);
            }
            else {
                return std::regex_match(ProcInfo::readCmdLine(pid, info), *procRegex);
            }
        }
    }

    bool ProcMatcher::cwdMatches(pid_t pid, ProcInfo* info)
    {
        if(cwdRegex.get() == nullptr) {
            return true;
        }
        else {
            return std::regex_match(ProcInfo::readCwd(pid), *cwdRegex);
        }
    }


    bool ProcMatcher::matches(pid_t pid, ProcInfo* info)
    {
        return procMatches(pid, info) && cwdMatches(pid, info);
    }

    /**
     * optionally takes a pointer to cache state
     */
    void ProcMatcher::execMatch(pid_t pid, ProcMatchEventType eventType, ProcInfo* info)
    {
        if(matches(pid, info)) {
            eventCallback(pid, eventType);
        }
    }


    void ProcInfo::checkRealpathErrno(char* allocedPath) {
        if(allocedPath == nullptr) {
            //there's an error, find out what it is
            //don't just check errno because it might have been set from a previous call

            throw RealpathException(std::string(strerror(errno)));
        }
    }

    std::string ProcInfo::readCmdLine(pid_t pid, ProcInfo* info)
    {
        if(info != nullptr && info->cmdLine.get() != nullptr) {
            return *info->cmdLine;
        }

        std::string cmdLine = readFile("/proc/" + std::to_string(pid) + "/cmdline");

        if(info != nullptr) {
            info->cmdLine = std::unique_ptr<std::string>(new std::string(cmdLine));
        }

        return cmdLine;
    }

    std::string ProcInfo::readProcName(pid_t pid, ProcInfo* info)
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

    std::string ProcInfo::readCwd(pid_t pid, ProcInfo* info)
    {
        //check cache
        if(info != nullptr && info->cwd.get() != nullptr) {
            return *info->cwd;
        }

        //use realpath to read the destination of the symlink as an absolute path
        std::string cwdPath = "/proc/" + std::to_string(pid) + "/cwd";

        char* allocedPath = realpath(cwdPath.c_str(), nullptr);
        checkRealpathErrno(allocedPath);

        std::string absCwd = allocedPath;

        if(info != nullptr) {
            info->cwd = std::unique_ptr<std::string>(new std::string(absCwd));
        }

        //realpath will allocate a c string when the 2nd arg is NULL
        free(allocedPath);
        return absCwd;
    }

}
