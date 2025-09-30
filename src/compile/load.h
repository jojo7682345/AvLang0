#ifndef __LOAD_H__
#define __LOAD_H__

#include <types.h>
#include <compilerState.h>

struct StringBuffer {
	const char* const data;
	uint64 length;
};

bool32 loadFile(struct CompilerCommand command, struct StringBuffer* buffer);

#endif//__LOAD_H__
