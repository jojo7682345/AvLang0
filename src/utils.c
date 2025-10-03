#include <utils.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>


uint64 cstrlen(const char* const str){
	if(str==NULL){ return 0; }
	size_t size = 0;
	while(str[size++]);
	return size-1;
}

bool32 streq(const char* strA, const char* strB){
	uint64 lenA = cstrlen(strA);
	uint64 lenB = cstrlen(strB);
	uint64 cmpLen = min(lenA, lenB);
	for(uint64 i = 0; i < cmpLen; i++){
		if(strA[i]!=strB[i]){
			return false;
		}
	}
	return true;
}

void printStr(const char* const str){
	int a = write(0, str, cstrlen(str));
	(void)a;
}


