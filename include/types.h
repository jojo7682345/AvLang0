#ifndef __TYPES__
#define __TYPES__
#include "stdint.h"

typedef uint8_t byte;
typedef uint8_t uint8;
typedef uint16_t uint16;
typedef uint32_t uint32;
typedef uint64_t uint64;

typedef int8_t sbyte;
typedef int8_t int8;
typedef int16_t int16;
typedef int32_t int32;
typedef int64_t int64;

typedef float float32;
typedef double float64;

typedef uint8 bool8;
typedef uint16 bool16;
#ifndef true
#define true (1)
#define false (0)
#endif
typedef uint32 bool32;

#ifndef nullptr
#define nullptr ((void*) 0)
#endif

#ifndef NULL
#define NULL (0)
#endif

typedef uint8 atomicByte;
typedef uint8 atomicUint8;
typedef uint16 atomicUint16;
typedef uint32 atomicUint32;
typedef int8 atomicSint8;
typedef int16 atomicSint16;
typedef int32 atomicSint32;
typedef int8 atomicSbyte;

typedef float atomicFloat32;

#define ALWAYS_INLINE __attribute__((always_inline)) inline
#endif//__TYPES__