#ifndef PTIMETRACKER_UTIL_H_
#define PTIMETRACKER_UTIL_H_

#include <string>
#include <stdexcept>
#include <memory>
#include <utility>
#include <iostream>
#include <regex>


namespace ptimetracker {

class TimeTrackerException : public std::runtime_error
{
public:
    virtual std::string getExceptionTypeName() = 0;

    TimeTrackerException(std::string const& message)
        : std::runtime_error(message)
    {}
};


std::string readFile(std::string);
std::string stripNewlines(std::string);

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


bool regexMatch(std::string, std::regex);
bool regexMatch(const char*, std::regex);

}

//stringification macros
//see https://stackoverflow.com/questions/2653214/stringification-of-a-macro-value
//DON'T name it str() or there WILL be name conflicts!
//any name that no one would write in a regular C/C++ file will do...
#define XSTRINGIFY_MACRO__(a) STRINGIFY_MACRO__(a)
#define STRINGIFY_MACRO__(a) #a

//exception-handling macros
#define NEW_EXCEPTION_TYPE(A) \
    class A : public ptimetracker::TimeTrackerException \
    { \
    public: \
        virtual std::string getExceptionTypeName() { return XSTRINGIFY_MACRO__(A); } \
            \
        A(std::string const& message) \
            : ptimetracker::TimeTrackerException(message) \
        {} \
    };

#endif
