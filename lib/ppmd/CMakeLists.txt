include_directories("libpmd")
include_directories("Lib")

add_library(ppmd STATIC ppmd.cpp ppmd.h)
set_target_properties(ppmd PROPERTIES POSITION_INDEPENDENT_CODE TRUE)