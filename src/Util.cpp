#include "Util.h"
#include "proc_functions.hpp"

#include <regex>
#include <string>
#include <fstream>
#include <iostream>
#include <limits>

#include <sys/types.h>
#include <unistd.h>
#include <dirent.h>

namespace {
    static std::regex matchNewlines(R"rgx(\n)rgx", ptimetracker::ProcMatcher::REGEX_FLAGS);
}

namespace ptimetracker {

std::string readFile(std::string inFileName)
{
    //see https://stackoverflow.com/questions/2602013/read-whole-ascii-file-into-c-stdstring
    std::ifstream ifs;
    //open the input file
    ifs.open(inFileName);

    //don't read it if it doesn't exist
    if(!ifs.good())
    {
        std::clog << "Warning: reading empty file " << inFileName << std::endl;
        return std::string();
    }

    std::stringstream strStream;
    strStream << ifs.rdbuf();
    auto retStr = strStream.str();
    if(retStr.empty())
        std::clog << "Warning: reading empty file " << inFileName << std::endl;
    return stripNewlines(retStr);
}

std::string stripNewlines(std::string in)
{
    return std::regex_replace(in, matchNewlines, "");
}

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
