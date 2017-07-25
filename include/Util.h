#ifndef UTIL_H_
#define UTIL_H_

#include <string>
#include <stdexcept>

class OpenDirException : public std::runtime_error
{
public:
    OpenDirException(std::string const& dir)
        : std::runtime_error("Error while opening " + dir)
    {}
};

bool dirExists(std::string);

std::string getCwd();


#endif
