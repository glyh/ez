# Meta Information ####
cmake_minimum_required(VERSION 3.15...3.31)

include(FetchContent)

project(
  EzBackEnd
  VERSION 0.1
  DESCRIPTION "Backend for Ez"
  LANGUAGES C CXX)

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

# https://github.com/protocolbuffers/protobuf/issues/14500
set(CMAKE_MODULE_LINKER_FLAGS "-Wl,--copy-dt-needed-entries")
set(CMAKE_SHARED_LINKER_FLAGS "-Wl,--copy-dt-needed-entries")
set(CMAKE_STATIC_LINKER_FLAGS "-Wl,--copy-dt-needed-entries")

add_executable(ezback)

set(INCLUDE_DIR ${CMAKE_SOURCE_DIR}/include)
include_directories(${INCLUDE_DIR})

set(SRC_DIR ${CMAKE_SOURCE_DIR}/src)
file(GLOB_RECURSE SOURCES "${SRC_DIR}/*.cpp")
target_sources(ezback PRIVATE ${SOURCES})

# Protobuf

find_package(Protobuf REQUIRED)
set(PB_GEN ${CMAKE_SOURCE_DIR}/proto_gen/)
include_directories(${PB_GEN})
target_sources(ezback PRIVATE "${PB_GEN}/ez_ir.pb.cc")
target_link_libraries(ezback protobuf::libprotobuf)

# https://github.com/protocolbuffers/protobuf/issues/15604#issuecomment-1929929148
if(Protobuf_VERSION VERSION_GREATER_EQUAL 4)
  find_package(absl REQUIRED)

  target_link_libraries(ezback absl::log_internal_check_op)
endif()

# LLVM

find_package(LLVM REQUIRED CONFIG)
message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

include_directories(${LLVM_INCLUDE_DIRS})
separate_arguments(LLVM_DEFINITIONS_LIST NATIVE_COMMAND ${LLVM_DEFINITIONS})
add_definitions(${LLVM_DEFINITIONS_LIST})

# On Arch Linux, all LLVM components are linked together
target_link_libraries(ezback -lLLVM)

# argparse
FetchContent_Declare(argparse
                     GIT_REPOSITORY https://github.com/p-ranav/argparse.git)
FetchContent_MakeAvailable(argparse)
target_link_libraries(ezback argparse)
