#ifndef PTIMETRACKER_UTIL_H_
#define PTIMETRACKER_UTIL_H_

#include <string>
#include <stdexcept>
#include <memory>
#include <utility>
#include <iostream>


//exception-handling macros
#define NEW_EXCEPTION_TYPE(A) \
    class A : public std::runtime_error \
    { \
    public: \
        A(std::string const& message) \
            : std::runtime_error(message) \
        {} \
    };


namespace ptimetracker {

int returnOnException(std::function<int(void)> f);

class OpenDirException : public std::runtime_error
{
public:
    OpenDirException(std::string const& dir)
        : std::runtime_error("Error while opening " + dir)
    {}
};

bool dirExists(std::string);

std::string getCwd();

}

#endif
