cmake_minimum_required(VERSION 2.8.12)
project(predictor)

set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_FLAGS -fPIC)
set(CMAKE_CXX_FLAGS -Wall)
set(CMAKE_CXX_FLAGS -pedantic)
set(CMAKE_CXX_FLAGS -O3)
set(CMAKE_BUILD_TYPE Release)

set(SOURCE_DIR "src/predictor")
# include directory with headers developed in the scope of that project
include_directories(${SOURCE_DIR})

# all source files developed in the scope of that project
set(PREDICTOR_SOURCES ${SOURCE_DIR}/predictor.cpp ${SOURCE_DIR}/sampler.cpp
  ${SOURCE_DIR}/continuation.cpp ${SOURCE_DIR}/compressors.cpp ${SOURCE_DIR}/builders.cpp)
file(GLOB PREDICTOR_HEADERS "${SOURCE_DIR}/*.h")

# include third-party library for high-precision floating-point arithmetic
include_directories(lib/ttmath)

# include third-party library for parsing command-line arguments
include_directories(lib/tclap)

file(GLOB EXTERNAL_HEADERS "lib/ttmath/*.h" "lib/tclap/*.h")

# include third-party headers and static libraries of compression algorithms
include_directories(lib/ppmd lib/rp lib/lcacomp lib/zstd lib/bzip2)

# because zlib generates header
include_directories(./lib/zlib)
include_directories(${PROJECT_BINARY_DIR}/lib/zlib)

# add implementations of compression algorithms
add_subdirectory(lib/bzip2)
add_subdirectory(lib/lcacomp)
add_subdirectory(lib/ppmd)
add_subdirectory(lib/rp)
add_subdirectory(lib/zlib)
add_subdirectory(lib/zstd)
set(COMPRESSION_LIBRARIES bz2 lca ppmd rp z zstd)

# build Python binding
add_subdirectory(lib/pybind11)
include_directories(lib/pybind11)
pybind11_add_module(predictor ${SOURCE_DIR}/pyexport.cpp ${PREDICTOR_SOURCES} ${PREDICTOR_HEADERS}
  ${EXTERNAL_HEADERS})
target_link_libraries(predictor PRIVATE ${COMPRESSION_LIBRARIES})

enable_testing()

find_package(Threads REQUIRED)
add_subdirectory(lib/googletest)
set(GTEST_INCLUDE_DIR "lib/googletest/googletest/include")
set(GTEST_LIB gtest_main gtest)
set(GMOCK_INCLUDE_DIR "lib/googletest/googlemock/include")
set(GMOCK_LIB gmock_main gmock)
include_directories(${GTEST_INCLUDE_DIR})
include_directories(${GMOCK_INCLUDE_DIR})

set(PREDICTOR_TESTS tests/test_predictor.cpp)
add_executable(pred_test ${PREDICTOR_TESTS} ${PREDICTOR_SOURCES} ${PREDICTOR_HEADERS}
  ${EXTERNAL_HEADERS})
target_link_libraries(pred_test rp z ppmd lca zstd bz2 ${GTEST_LIB} ${GMOCK_LIB})
add_test(pred_test pred_tests)

set(BIGNUMS_TESTS tests/test_bignums.cpp)
add_executable(bignums_test ${BIGNUMS_TESTS} src/predictor/bignums.h)
target_link_libraries(bignums_test ${GTEST_LIB} ${GMOCK_LIB})
add_test(bignums_test bignums_test)
