# Meta Information ####
cmake_minimum_required(VERSION 3.15...3.31)

project(
  EzStd
  VERSION 0.1
  DESCRIPTION "Stdlib for Ez"
  LANGUAGES C)

set(CMAKE_C_STANDARD 23)
set(CMAKE_C_STANDARD_REQUIRED ON)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

add_library(ezstd SHARED)

set(INCLUDE_DIR ${CMAKE_SOURCE_DIR}/include)
include_directories(${INCLUDE_DIR})

set(SRC_DIR ${CMAKE_SOURCE_DIR}/src)
file(GLOB_RECURSE SOURCES "${SRC_DIR}/*.c")
target_sources(ezstd PRIVATE ${SOURCES})

# Dependency: Boehm GC

find_library(GC gc)
target_link_libraries(ezstd PRIVATE ${GC})
