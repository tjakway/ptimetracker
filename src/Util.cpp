#include "Util.h"

#include <regex>
#include <string>
#include <fstream>

#include <sys/types.h>
#include <unistd.h>
#include <dirent.h>

namespace ptimetracker {

NEW_EXCEPTION_TYPE(ReadFileException)

std::string readFile(std::string inFileName)
{
    //see https://stackoverflow.com/questions/2602013/read-whole-ascii-file-into-c-stdstring
    std::ifstream ifs;
    //open the input file
    ifs.open(inFileName);


    //see http://www.cplusplus.com/reference/fstream/ifstream/rdbuf/
    //get pointer to associated buffer object
    std::filebuf* pbuf = ifs.rdbuf();

    //get file size using buffer's members
    std::size_t size = pbuf->pubseekoff (0,ifs.end,ifs.in);
    pbuf->pubseekpos (0,ifs.in);

    //allocate memory to contain file data
    char* buffer=new char[size];

    //get file data
    pbuf->sgetn (buffer,size);

    ifs.close();

    std::string retStr(buffer);
    delete[] buffer;

    return retStr;
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
