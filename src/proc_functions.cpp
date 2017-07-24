#include "proc_functions.hpp"

#include <vector>
#include <string>
#include <sstring>
#include <iostream>

namespace {
    std::string readFile(std::string filename)
    {
        //see https://stackoverflow.com/questions/2602013/read-whole-ascii-file-into-c-stdstring

        std::ifstream inFile;
        //open the input file
        inFile.open(inFileName);

        std::stringstream strStream;
        strStream << inFile.rdbuf();//read the file
        return strStream.str();//str holds the content of the file
    }

    class ProcInfo 
    {
        static std::string readCmdLine(pid_t pid)
        {

        }
        static std::string readProcName(pid_t pid)
        {

        }
        static std::string readCwd(pid_t pid)
        {

        }
    };
}

namespace ptimetracker {

    //TODO: define matches()
    //TODO: write static function to read relevant /proc/ info
    //probably read /proc/<pid>/stat (the file used by ps)


}

