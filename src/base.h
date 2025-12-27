#pragma once

#include "base_ctx_crack.h"

#include <stdint.h>

#define CLR_RED   "\x1b[31m"
#define CLR_GRN   "\x1b[32m"
#define CLR_YEL   "\x1b[33m"
#define CLR_BLU   "\x1b[34m"
#define CLR_MAG   "\x1b[35m"
#define CLR_CYN   "\x1b[36m"
#define CLR_WHT   "\x1b[37m"
#define CLR_RESET "\x1b[0m"

// TODO: Make sure terminal supports colors

// BASE TYPES
typedef uint8_t  u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;

typedef int8_t  s8;
typedef int16_t s16;
typedef int32_t s32;
typedef int64_t s64;

typedef s8  b8;
typedef s32 b32;
typedef s32 bool;

typedef size_t    usize;
typedef uintptr_t uintptr;

typedef float  f32;
typedef double f64;

#define true  1
#define false 0

///////////////////////////////////
// :helpers

#if !defined(BUILD_DEBUG)
# define BUILD_DEBUG 1
#endif

#define statement(s) do { s } while (0)

#if COMPILER_MSVC
# include <intrin.h>
# define trap() __debugbreak()
#elif COMPILER_CLANG || COMPILER_GCC
# define trap() __builtin_trap()
#else
# error Unknown trap intrinsic for this compiler.
#endif

#if BUILD_DEBUG
#define assert(x)                                                                \
  do                                                                             \
  {                                                                              \
    if (!(x))                                                                    \
    {                                                                            \
      printf(CLR_RED "Assertion hit: " CLR_RESET "%s:%d\n", __FILE__, __LINE__); \
      printf(CLR_YEL "\t%s\n" CLR_RESET, stringify(x));                          \
      trap();                                                                    \
    }                                                                            \
  } while (0);
#else
# define assert(c)
#endif

#define shift_args(argc, argv) ((argc)--, *(argv)++)

#define static_assert(c) global u8 glue(static, __LINE__)[(c)?1:-1]

#define _stringify(s) #s
#define stringify(s) _stringify(s)
#define _glue(a, b) a##b
#define glue(a, b) _glue(a, b)

#define array_count(arr) (sizeof(arr) / sizeof(*(arr)))

#define int_from_ptr(p) (unsigned long long)((char*)p - (char*)0)
#define ptr_from_int(n) (void*)((char*)0 + (n))

#define member(T, m) (((T*)0)->m)
#define offset_of_member(T, m) int_from_ptr(&member(T, m))
#define cast_from_member(T, m)

#define MIN(a, b) (((a)<(b))?(a):(b))
#define MAX(a, b) (((a)>(b))?(a):(b))
#define clamp(a, x, b) (((x)<(a))?(a):\
                        ((b)<(x))?(b):(x))
#define clamp_top(a, b) MIN(a, b)
#define clamp_bot(a, b) MAX(a, b)

#define c_linkage_begin extern "C" {
#define c_linkage_end   }
#define c_linkage extern "C"

#include <string.h>
#define mem_zero(p, z)       memset((p), 0, (z))
#define mem_zero_struct(p)   mem_zero((p), sizeof(*(p)))
#define mem_zero_array(p)    mem_zero((p), sizeof(p))
#define mem_zero_typed(p, c) mem_zero((p), sizeof(*(p))*(c))

#define mem_equal(a, b, z) (memcmp((a), (b), (z)) == 0)

#define mem_copy(d, s, z)       memmove((d), s, (z))
#define mem_copy_struct(d, s)   mem_copy((d), (s), MIN(sizeof(*(d)), sizeof(*(s))))
#define mem_copy_array(d, s)    mem_copy((d), (s), MIN(sizeof(d), sizeof(s)))
#define mem_copy_typed(d, s, c) mem_copy((d), (s), MIN(sizeof(*(d)), sizeof(*(s)))*(c))

#define KB(n) ((u64)(n) << 10)
#define MB(n) ((u64)(n) << 20)
#define GB(n) ((u64)(n) << 30)
#define TB(n) ((u64)(n) << 40)

#define align_up_pow2(n, p) (((u64)(n) + ((u64)(p) - 1)) & (~((u64)(p) - 1)))

////////////////////////////
// LINKED LIST MACROS
#define dll_push_back_np(f,l,n,next,prev) ((f)==0?\
                                           ((f)=(l)=(n),(n)->next=(n)->prev=0):\
                                           ((n)->prev=(l),(l)->next=(n),(l)=(n),(n)->next=0))
#define dll_push_back(f,l,n) dll_push_back_np(f,l,n,next,prev)

#define dll_push_front(f,l,n) dll_push_back_np(f,l,n,prev,next)

#define dll_remove_np(f,l,n,next,prev) (((f)==(n)?\
                                         ((f)=(f)->next,(f)->prev=0):\
                                         (l)==(n)?\
                                         ((l)=(l)->prev,(l)->next=0):\
                                         ((n)->next->prev=(n)->prev,\
                                         (n)->prev->next=(n)->next)))
#define dll_remove(f,l,n) dll_remove_np(f,l,n,next,prev)

////////////////////////////
// :keywords

#define local_persist static
#define internal      static
#define global        static

#if COMPILER_MSVC || (COMPILER_CLANG && OS_WINDOWS)
# pragma section(".rdata$", read)
# define read_only __declspec(allocate(".rdata$"))
#elif (COMPILER_CLANG && OS_LINUX)
# define read_only __attribute__((section(".rodata")))
#else
# define read_only
#endif

#if COMPILER_MSVC
# define thread_local __declspec(thread)
#elif COMPILER_CLANG || COMPILER_GCC
# define thread_local __thread
#else
# error thread_local not defined for this compiler.
#endif

#if COMPILER_MSVC
# define force_inline __forceinline
#elif COMPILER_CLANG || COMPILER_GCC
# define force_inline __attribute__((always_inline))
#else
# error force_inline not defined for this compiler.
#endif

#if COMPILER_MSVC
# define no_inline __declspec(noinline)
#elif COMPILER_CLANG || COMPILER_GCC
# define no_inline __attribute__((noinline))
#else
# error no_inline not defined for this compiler.
#endif
