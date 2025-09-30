#include "compilerState.h"


bool32 checkMemZero(void* mem, uint64 size){
	for(uint64 i = 0; i < size; i++){
		if(((byte*)mem)[i]!=0){
			return false;
		}
	}
	return true;
}

bool32 checkError(struct CompilerCommandStatusFlags status){
	return !checkMemZero(&status.errors, sizeof(status.errors));
}
bool32 checkWarning(struct CompilerCommandStatusFlags status){
	return !checkMemZero(&status.warnings, sizeof(status.warnings));
}
bool32 checkInfo(struct CompilerCommandStatusFlags status){
	return !checkMemZero(&status.infos, sizeof(status.infos));
}

bool32 checkAnomaly(struct CompilerCommandStatusFlags status){
	if(checkError(status) || checkWarning(status) || checkInfo(status)){
		return true;
	}
	return false;
}