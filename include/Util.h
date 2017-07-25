#ifndef UTIL_H_
#define UTIL_H_

#include <string>
#include <stdexcept>
#include <memory>

/**
 * see https://stackoverflow.com/questions/12580432/why-does-c11-have-make-shared-but-not-make-unique
 */
template<typename T, typename ...Args>
std::unique_ptr<T> make_unique( Args&& ...args )
{
    return std::unique_ptr<T>( new T( std::forward<Args>(args)... ) );
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


#endif
