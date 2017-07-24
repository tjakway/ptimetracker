#include "proc_functions.hpp"

#include <vector>
#include <string>
#include <sstream>
#include <fstream>
#include <cstdlib>

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

    //TODO: define matches()
    //TODO: write static function to read relevant /proc/ info
    //probably read /proc/<pid>/stat (the file used by ps)


}

