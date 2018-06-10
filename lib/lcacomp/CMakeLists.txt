cmake_minimum_required(VERSION 3.5)

project(lcacomp)

set(LCA_SOURCES lcacomp.cpp txt2cfg_online.cpp cfg2enc.cpp bits.cpp)
set(LCA_HEADERS bits.h cfg2enc.h txt2cfg_online.h lcacommon.h lcacomp.h)

add_library(lca STATIC ${LCA_SOURCES} ${LCA_HEADERS})
set_target_properties(lca PROPERTIES POSITION_INDEPENDENT_CODE TRUE)