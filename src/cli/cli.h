#ifndef __CLI__
#define __CLI__
#include <compilerState.h>


void printHelp();

bool32 parseCompilerArguments(const int32 argC, const char* argV[], struct CompilerCommand* command);

#endif//__CLI__
