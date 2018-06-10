cmake_minimum_required(VERSION 2.8)

project(BZIP2 C)

file(GLOB BZIP2_SRC "*.c")
file(GLOB BZIP2_HEADERS "*.h")

add_library(bz2 STATIC ${BZIP2_SRC} ${BZIP2_HEADERS})
set_target_properties(bz2 PROPERTIES POSITION_INDEPENDENT_CODE TRUE)