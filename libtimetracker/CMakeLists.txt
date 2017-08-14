cmake_minimum_required(VERSION 3.1)

project(PTIMETRACKER)

set(MAIN_TARGET ptimetracker_c)
set(MAIN_TARGET_LIB timetracker)
set(TEST_TARGET test_ptimetracker)
set(ROOT_TEST_TARGET root_test_ptimetracker)

#see https://stackoverflow.com/questions/10851247/how-to-activate-c-11-in-cmake
set(CMAKE_C_STANDARD 11)
set(CMAKE_CXX_STANDARD 11)

set(PTIMETRACKER_SRCS_NOT_MAIN src/exec-notify.c src/proc_functions.cpp src/APIState.cpp src/Util.cpp src/register.cpp)
set(PTIMETRACKER_MAIN_SRC src/main.c)
set(PTIMETRACKER_SRCS ${PTIMETRACKER_SRCS_NOT_MAIN} ${PTIMETRACKER_MAIN_SRC})
set(PTIMETRACKER_TEST_SRCS test/test_procmatcher.cpp)
set(PTIMETRACKER_ROOT_TEST_SRCS test/root/root_tests.cpp)

include_directories(${CMAKE_SOURCE_DIR}/include)


#build google test
#gtest is directly checked into this repository to reduce build script complexity
#see http://stackoverflow.com/questions/9689183/cmake-googletest
#builds gtest in a subdirectory of the binary dir
SET(BUILD_GTEST ON CACHE BOOL "Builds the googletest subproject")
add_subdirectory("${CMAKE_SOURCE_DIR}/lib/googletest-release-1.8.0" ${CMAKE_BINARY_DIR}/gtest-bin)
set(PTIMETRACKER_GTEST_OPTIONS -Wp,-w -w)
target_compile_options(gtest PRIVATE ${PTIMETRACKER_GTEST_OPTIONS})
target_compile_options(gtest_main PRIVATE ${PTIMETRACKER_GTEST_OPTIONS})

include_directories(SYSTEM ${gtest_SOURCE_DIR}/include ${gtest_SOURCE_DIR})
include_directories(SYSTEM ${GTEST_INCLUDE_DIRS})
include_directories(${CMAKE_SOURCE_DIR}/test)

add_library(${MAIN_TARGET_LIB} STATIC ${PTIMETRACKER_SRCS_NOT_MAIN})
set_property(TARGET ${MAIN_TARGET_LIB} PROPERTY POSITION_INDEPENDENT_CODE ON)

add_executable(${MAIN_TARGET} ${PTIMETRACKER_MAIN_SRC})
target_link_libraries(${MAIN_TARGET} ${MAIN_TARGET_LIB})

add_executable(${TEST_TARGET} ${PTIMETRACKER_TEST_SRCS})
target_link_libraries(${TEST_TARGET} gtest gtest_main)
target_link_libraries(${TEST_TARGET} ${MAIN_TARGET_LIB})

add_executable(${ROOT_TEST_TARGET} ${PTIMETRACKER_ROOT_TEST_SRCS})
target_link_libraries(${ROOT_TEST_TARGET} gtest gtest_main)
target_link_libraries(${ROOT_TEST_TARGET} ${MAIN_TARGET_LIB})
