cmake_minimum_required(VERSION 3.1)

project(PTIMETRACKER)



set(MAIN_TARGET ptimetracker)
set(MAIN_TARGET_LIB timetracker)
#set(TEST_TARGET test_ptimetracker)

#see https://stackoverflow.com/questions/10851247/how-to-activate-c-11-in-cmake
set(CMAKE_C_STANDARD 11)
set(CMAKE_CXX_STANDARD 11)

set(PTIMETRACKER_SRCS_NOT_MAIN src/exec-notify.c src/proc_functions.cpp src/APIState.cpp)
set(PTIMETRACKER_SRCS ${PTIMETRACKER_SRCS_NOT_MAIN} src/main.c)
set(PTIMETRACKER_MAIN_SRC src/main.c)
#set(PTIMETRACKER_TEST_SRCS test/2080_test.c)

include_directories(${CMAKE_SOURCE_DIR}/include)

#TODO: only depend on check if we're building in debug mode
#find_package(Check REQUIRED)
#include_directories(${CHECK_INCLUDE_DIR})

add_library(${MAIN_TARGET_LIB} ${PTIMETRACKER_SRCS_NOT_MAIN})

add_executable(${MAIN_TARGET} ${PTIMETRACKER_MAIN_SRC})
target_link_libraries(${MAIN_TARGET} ${MAIN_TARGET_LIB})


#add_executable(${TEST_TARGET} ${DELAYBIND_SRCS_NOT_MAIN} ${DELAYBIND_TEST_SRCS})
#target_compile_definitions(${TEST_TARGET} PRIVATE ${LIBSSH_DEFINITIONS})
#target_link_libraries(${TEST_TARGET} ${LIBSSH_LIBRARIES} ${CHECK_LIBRARIES})
