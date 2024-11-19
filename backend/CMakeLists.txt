# Meta Information ####
cmake_minimum_required(VERSION 3.15...3.31)

project(
  EzBackEnd
  VERSION 0.1
  DESCRIPTION "Backend for Ez"
  LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
# https://github.com/protocolbuffers/protobuf/issues/14500
set(CMAKE_MODULE_LINKER_FLAGS "-Wl,--copy-dt-needed-entries")
set(CMAKE_SHARED_LINKER_FLAGS "-Wl,--copy-dt-needed-entries")
set(CMAKE_STATIC_LINKER_FLAGS "-Wl,--copy-dt-needed-entries")

# Dependencies

find_package(Protobuf REQUIRED)

# Compiling & Linking

set(INCLUDE_DIR ${CMAKE_SOURCE_DIR}/include)
set(PB_GEN ${CMAKE_SOURCE_DIR}/proto_gen/)
set(SRC_DIR ${CMAKE_SOURCE_DIR}/src)

file(GLOB_RECURSE SOURCES "${SRC_DIR}/*.cpp")

include_directories(${INCLUDE_DIR} ${PB_GEN})

add_executable(ezback ${SOURCES} "${PB_GEN}/ez_ir.pb.cc")
target_link_libraries(ezback PUBLIC protobuf::libprotobuf)

# https://github.com/protocolbuffers/protobuf/issues/15604#issuecomment-1929929148
if(Protobuf_VERSION VERSION_GREATER_EQUAL 4)
  find_package(absl REQUIRED)

  target_link_libraries(ezback PUBLIC absl::log_internal_check_op)
endif()
