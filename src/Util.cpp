#include "Util.h"

#include <regex>
#include <string>

#include <sys/types.h>
#include <unistd.h>
#include <dirent.h>

namespace ptimetracker {
bool dirExists(std::string dirName)
{
    DIR* dir = opendir(dirName.c_str());
    if(dir) {
        return true;
    } else if (errno == ENOENT) {
        return false;
    } else {
        throw OpenDirException(dirName);
    }
}

std::string getCwd()
{
    const long size = pathconf(".", _PC_PATH_MAX);
    char* cwd = new char[size];

    const char* ret = getcwd(cwd, (size_t)size);

    std::string retStr(cwd);

    delete[] cwd;

    return retStr;
}

int returnOnException(std::function<int(void)> f) {
    try {
        return f();
    }
    catch(...) {
        std::exception_ptr e = std::current_exception();
        std::cerr <<(e ? e.__cxa_exception_type()->name() : "null") << std::endl;
        return 1;
    }
}

bool regexMatch(std::string str, std::regex r)
{
    std::smatch base_match;
    std::regex_match(str, base_match, r);
    return !base_match.empty();
}

bool regexMatch(const char* str, std::regex r)
{
    std::string cppStr(str);
    return regexMatch(cppStr, r);
}

}
