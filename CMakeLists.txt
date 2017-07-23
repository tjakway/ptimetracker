cmake_minimum_required(VERSION 2.8.11)

project(PTIMETRACKER)

set(MAIN_TARGET ptimetracker)
set(MAIN_TARGET_LIB ptimetrackerlib)
#set(TEST_TARGET test_ptimetracker)

set(CMAKE_C_STANDARD 11)
set(PTIMETRACKER_SRCS_NOT_MAIN src/exec-notify.c)
set(PTIMETRACKER_SRCS ${PTIMETRACKER_SRCS_NOT_MAIN} src/main.cpp)
set(PTIMETRACKER_MAIN_SRC src/main.cpp)
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
