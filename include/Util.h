#ifndef UTIL_H_
#define UTIL_H_

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

int returnOnError(std::function<int(void)> f) {
    try {
        return f();
    }
    catch(...) {
        std::exception_ptr e = std::current_exception();
        std::cerr <<(e ? e.__cxa_exception_type()->name() : "null") << std::endl;
        return 1;
    }
}

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
