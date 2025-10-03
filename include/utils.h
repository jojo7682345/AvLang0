#ifndef __UTILS__
#define __UTILS__

#include <types.h>
#include "compilerState.h"

uint64 cstrlen(const char* const str);
bool32 streq(const char* strA, const char* strB);

void printStr(const char* str);

void printChar(char c);

void copyMem(const void* restrict src, const void* restrict dst, const uint64 size);

#define min(a, b) ((a)>(b)?(b):(a))
#define max(a, b) ((a)>(b)?(a):(b))
#define clamp(x, minX, maxX) (min((maxX), max((minX), (x))))

void printHex64(uint64 hex);
void printHex32(uint32 hex);
void printHex16(uint16 hex);
void printHex8(uint8 hex);


void printErr(const char* message);
void printError(const char* message);
void printWarning(const char* message);
void printInfo(const char* message);

void printStatus(const char* currentArgument, struct CompilerCommandStatusFlags status);
#endif//__UTILS__
