#include <utils.h>

void copyMem(const void* restrict src, const void* restrict dst, const uint64 size){
	uint64 remainder = size % sizeof(uint64);
	uint64 longs = size / sizeof(uint64);

	for(uint64 i = 0; i < longs; i++){
		((uint64*)dst)[i] = ((uint64*)src)[i];
	}

	for(uint64 i = size - remainder; i < size; i++){
		((uint8*)dst)[i] = ((uint8*)src)[i];
	}
}