cmake_minimum_required(VERSION 3.14)
project(Toka VERSION 0.4.0 LANGUAGES C CXX)

# Version Control Info
find_package(Git)
if(GIT_FOUND)
    # Get the commit count for build number
    execute_process(
        COMMAND ${GIT_EXECUTABLE} rev-list --count HEAD
        WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
        OUTPUT_VARIABLE TOKA_BUILD_NUMBER
        OUTPUT_STRIP_TRAILING_WHITESPACE
    )
    
    # Get the latest commit hash
    execute_process(
        COMMAND ${GIT_EXECUTABLE} rev-parse --short HEAD
        WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
        OUTPUT_VARIABLE TOKA_COMMIT_HASH
        OUTPUT_STRIP_TRAILING_WHITESPACE
    )
else()
    set(TOKA_BUILD_NUMBER 0)
    set(TOKA_COMMIT_HASH "unknown")
endif()

# Determine Target Triple
if(LLVM_DEFAULT_TARGET_TRIPLE)
    set(TOKA_TARGET_TRIPLE "${LLVM_DEFAULT_TARGET_TRIPLE}")
else()
    set(TOKA_TARGET_TRIPLE "${CMAKE_SYSTEM_PROCESSOR}-${CMAKE_SYSTEM_NAME}")
endif()

# Configure the Version header
configure_file(
    ${CMAKE_SOURCE_DIR}/include/toka/Version.h.in
    ${CMAKE_SOURCE_DIR}/include/toka/Version.h
)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Find LLVM
find_package(LLVM REQUIRED CONFIG)

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

include_directories(include)

add_subdirectory(src)
