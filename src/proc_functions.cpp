#include "proc_functions.hpp"

#include <vector>
#include <string>
#include <sstream>
#include <fstream>
#include <cstdlib>
#include <regex>
#include <memory>

/**
 * see https://stackoverflow.com/questions/12580432/why-does-c11-have-make-shared-but-not-make-unique
 */
template<typename T, typename ...Args>
std::unique_ptr<T> make_unique( Args&& ...args )
{
    return std::unique_ptr<T>( new T( std::forward<Args>(args)... ) );
}

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

    class ProcInfo 
    {
    public:
        static std::string readCmdLine(pid_t pid)
        {
            return readFile("/proc/" + std::to_string(pid) + "/cmdline");
        }
        static std::string readProcName(pid_t pid)
        {
            return readFile("/proc/" + std::to_string(pid) + "/comm");
        }
        static std::string readCwd(pid_t pid)
        {
            //use realpath to read the destination of the symlink as an absolute path
            std::string cwdPath = "/proc/" + std::to_string(pid) + "/cwd";

            char* allocedPath = realpath(cwdPath.c_str(), nullptr);

            std::string absCwd = allocedPath;

            //realpath will allocate a c string when the 2nd arg is NULL
            free(allocedPath);
            return absCwd;
        }
    };
}

namespace ptimetracker {
    ProcMatcher::ProcMatcher(const char* procRegexStr, bool matchOnlyProgName, const char* cwdRegexStr)
        : matchOnlyProgName(matchOnlyProgName)
    {
        if(procRegexStr != nullptr) {
            procRegex = make_unique<std::regex>(procRegexStr);
        }

        if(cwdRegexStr != nullptr) {
            cwdRegex = make_unique<std::regex>(cwdRegexStr);
        }
    }

    bool ProcMatcher::procMatches(pid_t pid)
    {
        if(procRegex.get() == nullptr) {
            return true;
        }
        else {
            //check whether we're testing just the program name or the entire invocation
            if(matchOnlyProgName) {
                return std::regex_match(ProcInfo::readProcName(pid), *procRegex);
            }
            else {
                return std::regex_match(ProcInfo::readCmdLine(pid), *procRegex);
            }
        }
    }

    bool ProcMatcher::cwdMatches(pid_t pid)
    {
        if(cwdRegex.get() == nullptr) {
            return true;
        }
        else {
            return std::regex_match(ProcInfo::readCwd(pid), *cwdRegex);
        }
    }


    bool ProcMatcher::matches(pid_t pid)
    {
        return procMatches(pid) && cwdMatches(pid);
    }
}

