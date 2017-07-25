#include "Util.h"

#include <sys/types.h>
#include <unistd.h>
#include <dirent.h>

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
