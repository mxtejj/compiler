#include <stdint.h>

//////////////////////////
// CLANG OS/Arch detection

#if defined(__clang__)

# define COMPILER_CLANG 1
# if defined(_WIN32)
#  define OS_WINDOWS 1
# elif defined(__linux__)
#  define OS_LINUX 1
# elif defined(	__APPLE__) && defined(__MACH__)
#  define OS_MAC 1
# else
#  error Compiler/OS combo not supported.
# endif

# if defined(__amd64__)
#  define ARCH_X64 1
# elif defined(__i386__) // TODO: Verify this works on clang.
#  define ARCH_X86 1
# elif defined(__arm__) // TODO: Verify this works on clang.
#  define ARCH_ARM32 1
# elif defined(__aarch64__) // TODO: Verify this works on clang.
#  define ARCH_ARM64
# else
#  error Architecture not supported.
# endif

/////////////////////////
// MSVC OS/Arch detection

#elif defined(_MSC_VER)

# define COMPILER_MSVC 1

# if defined(_WIN32)
#  define OS_WINDOWS 1
# else
#  error Compiler/OS combo not supported.
# endif

# if defined(_M_AMD64)
#  define ARCH_X64 1
# elif defined(_M_IX86)
#  define ARCH_X86 1
# elif defined(_M_ARM)
#  define ARCH_ARM32 1
# elif defined(_M_ARM64) // TODO: Verify this works on MSVC.
#  define ARCH_ARM64 1
# else
#  error Architecture not supported.
# endif

/////////////////////////
// GCC OS/Arch detection

#elif defined(__GNUC__)

# define COMPILER_GCC 1
# if defined(_WIN32)
#  define OS_WINDOWS 1
# elif defined(__linux__)
#  define OS_LINUX 1
# elif defined(	__APPLE__) && defined(__MACH__)
#  define OS_MAC 1
# else
#  error Compiler/OS combo not supported.
# endif

# if defined(__amd64__)
#  define ARCH_X64 1
# elif defined(__i386__) // TODO: Verify this works on GCC.
#  define ARCH_X86 1
# elif defined(__arm__) // TODO: Verify this works on GCC.
#  define ARCH_ARM32 1
# elif defined(__aarch64__) // TODO: Verify this works on GCC.
#  define ARCH_ARM64
# else
#  error Architecture not supported.
# endif

#else
# error Compiler is not supported.
#endif

///////////////////////////////////
// Zero fill missing context macros

#if !defined(COMPILER_MSVC)
# define COMPILER_MSVC 0
#endif
#if !defined(COMPILER_CLANG)
# define COMPILER_CLANG 0
#endif
#if !defined(COMPILER_GCC)
# define COMPILER_GCC 0
#endif

#if !defined(OS_WINDOWS)
# define OS_WINDOWS 0
#endif
#if !defined(OS_LINUX)
# define OS_LINUX 0
#endif
#if !defined(OS_MAC)
# define OS_MAC 0
#endif

#if !defined(ARCH_X64)
# define ARCH_X64 0
#endif
#if !defined(ARCH_X86)
# define ARCH_X86 0
#endif
#if !defined(ARCH_ARM)
# define ARCH_ARM32 0
#endif
#if !defined(ARCH_ARM64)
# define ARCH_ARM64 0
#endif

//- Keywords
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

//- Primitive types
typedef uint8_t  u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;
typedef int8_t   s8;
typedef int16_t  s16;
typedef int32_t  s32;
typedef int64_t  s64;
typedef float    f32;
typedef double   f64;
typedef s8       b8;
typedef s16      b16;
typedef s32      b32;
typedef s64      b64;
typedef b8       bool;
typedef uintptr_t uintptr;

#define true 1
#define false 0

typedef struct {
  u8 *data;
  u64 length;
} string;

typedef u8 *cstring;

//
//- Safe Operations for All Integer Types
// Eliminates UB from: signed overflow, shift >= bitwidth, negative shifts,
// division by zero, and makes implementation-defined behavior explicit.
//

//=============================================================================
//- s8 Operations (use s16 intermediate to avoid overflow)
//=============================================================================
force_inline s8 add_s8(s8 a, s8 b) { return (s8)((s16)a + (s16)b); }
force_inline s8 sub_s8(s8 a, s8 b) { return (s8)((s16)a - (s16)b); }
force_inline s8 mul_s8(s8 a, s8 b) { return (s8)((s16)a * (s16)b); }
force_inline s8 div_s8(s8 a, s8 b) { 
  if (b == 0) return 0;
  if (a == INT8_MIN && b == -1) return INT8_MIN;
  return a / b;
}
force_inline s8 mod_s8(s8 a, s8 b) {
  if (b == 0) return 0;
  if (a == INT8_MIN && b == -1) return 0;
  return a % b;
}
force_inline s8 neg_s8(s8 a) { return (s8)(-(s16)a); }
force_inline s8 shl_s8(s8 a, s8 b) { return (b >= 0 && b < 8) ? (s8)((u8)a << b) : 0; }
force_inline s8 shr_s8(s8 a, s8 b) {
  if (b < 0 || b >= 8) return a < 0 ? -1 : 0;
  // Arithmetic right shift with sign extension
  return (s8)((s16)a >> b);
}
force_inline s8 and_s8(s8 a, s8 b) { return a & b; }
force_inline s8 or_s8(s8 a, s8 b) { return a | b; }
force_inline s8 xor_s8(s8 a, s8 b) { return a ^ b; }
force_inline s8 not_s8(s8 a) { return ~a; }
force_inline bool lt_s8(s8 a, s8 b) { return a < b; }
force_inline bool gt_s8(s8 a, s8 b) { return a > b; }
force_inline bool lte_s8(s8 a, s8 b) { return a <= b; }
force_inline bool gte_s8(s8 a, s8 b) { return a >= b; }
force_inline bool eq_s8(s8 a, s8 b) { return a == b; }
force_inline bool neq_s8(s8 a, s8 b) { return a != b; }

//=============================================================================
//- u8 Operations (no overflow UB for unsigned, but provided for consistency)
//=============================================================================
force_inline u8 add_u8(u8 a, u8 b) { return a + b; }  // Wrapping defined for unsigned
force_inline u8 sub_u8(u8 a, u8 b) { return a - b; }
force_inline u8 mul_u8(u8 a, u8 b) { return (u8)((u16)a * (u16)b); }
force_inline u8 div_u8(u8 a, u8 b) { return b == 0 ? 0 : a / b; }
force_inline u8 mod_u8(u8 a, u8 b) { return b == 0 ? 0 : a % b; }
force_inline u8 shl_u8(u8 a, u8 b) { return (b < 8) ? (a << b) : 0; }
force_inline u8 shr_u8(u8 a, u8 b) { return (b < 8) ? (a >> b) : 0; }
force_inline u8 and_u8(u8 a, u8 b) { return a & b; }
force_inline u8 or_u8(u8 a, u8 b) { return a | b; }
force_inline u8 xor_u8(u8 a, u8 b) { return a ^ b; }
force_inline u8 not_u8(u8 a) { return ~a; }
force_inline bool lt_u8(u8 a, u8 b) { return a < b; }
force_inline bool gt_u8(u8 a, u8 b) { return a > b; }
force_inline bool lte_u8(u8 a, u8 b) { return a <= b; }
force_inline bool gte_u8(u8 a, u8 b) { return a >= b; }
force_inline bool eq_u8(u8 a, u8 b) { return a == b; }
force_inline bool neq_u8(u8 a, u8 b) { return a != b; }

//=============================================================================
//- s16 Operations (use s32 intermediate)
//=============================================================================
force_inline s16 add_s16(s16 a, s16 b) { return (s16)((s32)a + (s32)b); }
force_inline s16 sub_s16(s16 a, s16 b) { return (s16)((s32)a - (s32)b); }
force_inline s16 mul_s16(s16 a, s16 b) { return (s16)((s32)a * (s32)b); }
force_inline s16 div_s16(s16 a, s16 b) {
  if (b == 0) return 0;
  if (a == INT16_MIN && b == -1) return INT16_MIN;
  return a / b;
}
force_inline s16 mod_s16(s16 a, s16 b) {
  if (b == 0) return 0;
  if (a == INT16_MIN && b == -1) return 0;
  return a % b;
}
force_inline s16 neg_s16(s16 a) { return (s16)(-(s32)a); }
force_inline s16 shl_s16(s16 a, s16 b) { return (b >= 0 && b < 16) ? (s16)((u16)a << b) : 0; }
force_inline s16 shr_s16(s16 a, s16 b) {
  if (b < 0 || b >= 16) return a < 0 ? -1 : 0;
  // Arithmetic right shift with sign extension
  return (s16)((s32)a >> b);
}
force_inline s16 and_s16(s16 a, s16 b) { return a & b; }
force_inline s16 or_s16(s16 a, s16 b) { return a | b; }
force_inline s16 xor_s16(s16 a, s16 b) { return a ^ b; }
force_inline s16 not_s16(s16 a) { return ~a; }
force_inline bool lt_s16(s16 a, s16 b) { return a < b; }
force_inline bool gt_s16(s16 a, s16 b) { return a > b; }
force_inline bool lte_s16(s16 a, s16 b) { return a <= b; }
force_inline bool gte_s16(s16 a, s16 b) { return a >= b; }
force_inline bool eq_s16(s16 a, s16 b) { return a == b; }
force_inline bool neq_s16(s16 a, s16 b) { return a != b; }

//=============================================================================
//- u16 Operations
//=============================================================================
force_inline u16 add_u16(u16 a, u16 b) { return a + b; }
force_inline u16 sub_u16(u16 a, u16 b) { return a - b; }
force_inline u16 mul_u16(u16 a, u16 b) { return (u16)((u32)a * (u32)b); }
force_inline u16 div_u16(u16 a, u16 b) { return b == 0 ? 0 : a / b; }
force_inline u16 mod_u16(u16 a, u16 b) { return b == 0 ? 0 : a % b; }
force_inline u16 shl_u16(u16 a, u16 b) { return (b < 16) ? (a << b) : 0; }
force_inline u16 shr_u16(u16 a, u16 b) { return (b < 16) ? (a >> b) : 0; }
force_inline u16 and_u16(u16 a, u16 b) { return a & b; }
force_inline u16 or_u16(u16 a, u16 b) { return a | b; }
force_inline u16 xor_u16(u16 a, u16 b) { return a ^ b; }
force_inline u16 not_u16(u16 a) { return ~a; }
force_inline bool lt_u16(u16 a, u16 b) { return a < b; }
force_inline bool gt_u16(u16 a, u16 b) { return a > b; }
force_inline bool lte_u16(u16 a, u16 b) { return a <= b; }
force_inline bool gte_u16(u16 a, u16 b) { return a >= b; }
force_inline bool eq_u16(u16 a, u16 b) { return a == b; }
force_inline bool neq_u16(u16 a, u16 b) { return a != b; }

//=============================================================================
//- s32 Operations (use s64 intermediate)
//=============================================================================
force_inline s32 add_s32(s32 a, s32 b) { return (s32)((s64)a + (s64)b); }
force_inline s32 sub_s32(s32 a, s32 b) { return (s32)((s64)a - (s64)b); }
force_inline s32 mul_s32(s32 a, s32 b) { return (s32)((s64)a * (s64)b); }
force_inline s32 div_s32(s32 a, s32 b) {
  if (b == 0) return 0;
  if (a == INT32_MIN && b == -1) return INT32_MIN;
  return a / b;
}
force_inline s32 mod_s32(s32 a, s32 b) {
  if (b == 0) return 0;
  if (a == INT32_MIN && b == -1) return 0;
  return a % b;
}
force_inline s32 neg_s32(s32 a) { return (s32)(-(s64)a); }
force_inline s32 shl_s32(s32 a, s32 b) { return (b >= 0 && b < 32) ? (s32)((u32)a << b) : 0; }
force_inline s32 shr_s32(s32 a, s32 b) {
  if (b < 0 || b >= 32) return a < 0 ? -1 : 0;
  u32 ua = (u32)a;
  if (a >= 0) return (s32)(ua >> b);
  return (s32)((ua >> b) | ~(~0u >> b));
}
force_inline s32 and_s32(s32 a, s32 b) { return a & b; }
force_inline s32 or_s32(s32 a, s32 b) { return a | b; }
force_inline s32 xor_s32(s32 a, s32 b) { return a ^ b; }
force_inline s32 not_s32(s32 a) { return ~a; }
force_inline bool lt_s32(s32 a, s32 b) { return a < b; }
force_inline bool gt_s32(s32 a, s32 b) { return a > b; }
force_inline bool lte_s32(s32 a, s32 b) { return a <= b; }
force_inline bool gte_s32(s32 a, s32 b) { return a >= b; }
force_inline bool eq_s32(s32 a, s32 b) { return a == b; }
force_inline bool neq_s32(s32 a, s32 b) { return a != b; }

//=============================================================================
//- u32 Operations
//=============================================================================
force_inline u32 add_u32(u32 a, u32 b) { return a + b; }
force_inline u32 sub_u32(u32 a, u32 b) { return a - b; }
force_inline u32 mul_u32(u32 a, u32 b) { return (u32)((u64)a * (u64)b); }
force_inline u32 div_u32(u32 a, u32 b) { return b == 0 ? 0 : a / b; }
force_inline u32 mod_u32(u32 a, u32 b) { return b == 0 ? 0 : a % b; }
force_inline u32 shl_u32(u32 a, u32 b) { return (b < 32) ? (a << b) : 0; }
force_inline u32 shr_u32(u32 a, u32 b) { return (b < 32) ? (a >> b) : 0; }
force_inline u32 and_u32(u32 a, u32 b) { return a & b; }
force_inline u32 or_u32(u32 a, u32 b) { return a | b; }
force_inline u32 xor_u32(u32 a, u32 b) { return a ^ b; }
force_inline u32 not_u32(u32 a) { return ~a; }
force_inline bool lt_u32(u32 a, u32 b) { return a < b; }
force_inline bool gt_u32(u32 a, u32 b) { return a > b; }
force_inline bool lte_u32(u32 a, u32 b) { return a <= b; }
force_inline bool gte_u32(u32 a, u32 b) { return a >= b; }
force_inline bool eq_u32(u32 a, u32 b) { return a == b; }
force_inline bool neq_u32(u32 a, u32 b) { return a != b; }

//=============================================================================
//- Portable 128-bit Integer Type (for s64/u64 overflow prevention)
//=============================================================================

// 128-bit signed integer (stored as two 64-bit parts)
typedef struct {
  u64 lo;  // Low 64 bits
  s64 hi;  // High 64 bits (signed for s128)
} s128;

// 128-bit unsigned integer
typedef struct {
  u64 lo;  // Low 64 bits
  u64 hi;  // High 64 bits
} u128;

// Create s128 from s64
force_inline s128 s128_from_s64(s64 val) {
  s128 result;
  result.lo = (u64)val;
  result.hi = val < 0 ? -1 : 0;  // Sign extension
  return result;
}

// Create u128 from u64
force_inline u128 u128_from_u64(u64 val) {
  u128 result;
  result.lo = val;
  result.hi = 0;
  return result;
}

// s128 addition
force_inline s128 s128_add(s128 a, s128 b) {
  s128 result;
  result.lo = a.lo + b.lo;
  result.hi = a.hi + b.hi + (result.lo < a.lo ? 1 : 0);  // Carry
  return result;
}

// s128 subtraction
force_inline s128 s128_sub(s128 a, s128 b) {
  s128 result;
  result.lo = a.lo - b.lo;
  result.hi = a.hi - b.hi - (a.lo < b.lo ? 1 : 0);  // Borrow
  return result;
}

// s128 multiplication (simple version - just low 128 bits of result)
force_inline s128 s128_mul(s128 a, s128 b) {
  // Convert to unsigned for multiplication, then reinterpret
  u64 a_lo = a.lo;
  u64 b_lo = b.lo;
  
  // Split into 32-bit parts for multiplication
  u64 a_lo_lo = a_lo & 0xFFFFFFFFull;
  u64 a_lo_hi = a_lo >> 32;
  u64 b_lo_lo = b_lo & 0xFFFFFFFFull;
  u64 b_lo_hi = b_lo >> 32;
  
  u64 p0 = a_lo_lo * b_lo_lo;
  u64 p1 = a_lo_lo * b_lo_hi;
  u64 p2 = a_lo_hi * b_lo_lo;
  u64 p3 = a_lo_hi * b_lo_hi;
  
  u64 carry = ((p0 >> 32) + (p1 & 0xFFFFFFFFull) + (p2 & 0xFFFFFFFFull)) >> 32;
  
  s128 result;
  result.lo = p0 + (p1 << 32) + (p2 << 32);
  result.hi = p3 + (p1 >> 32) + (p2 >> 32) + carry;
  result.hi += a.hi * b_lo + a_lo * b.hi;  // Cross terms
  
  return result;
}

// s128 negation
force_inline s128 s128_neg(s128 a) {
  s128 zero = {0, 0};
  return s128_sub(zero, a);
}

// Convert s128 back to s64 (truncate high bits)
force_inline s64 s128_to_s64(s128 val) {
  return (s64)val.lo;
}

// u128 addition
force_inline u128 u128_add(u128 a, u128 b) {
  u128 result;
  result.lo = a.lo + b.lo;
  result.hi = a.hi + b.hi + (result.lo < a.lo ? 1 : 0);
  return result;
}

// u128 multiplication
force_inline u128 u128_mul(u128 a, u128 b) {
  u64 a_lo = a.lo;
  u64 b_lo = b.lo;
  
  u64 a_lo_lo = a_lo & 0xFFFFFFFFull;
  u64 a_lo_hi = a_lo >> 32;
  u64 b_lo_lo = b_lo & 0xFFFFFFFFull;
  u64 b_lo_hi = b_lo >> 32;
  
  u64 p0 = a_lo_lo * b_lo_lo;
  u64 p1 = a_lo_lo * b_lo_hi;
  u64 p2 = a_lo_hi * b_lo_lo;
  u64 p3 = a_lo_hi * b_lo_hi;
  
  u64 carry = ((p0 >> 32) + (p1 & 0xFFFFFFFFull) + (p2 & 0xFFFFFFFFull)) >> 32;
  
  u128 result;
  result.lo = p0 + (p1 << 32) + (p2 << 32);
  result.hi = p3 + (p1 >> 32) + (p2 >> 32) + carry;
  result.hi += a.hi * b_lo + a_lo * b.hi;
  
  return result;
}

// Convert u128 back to u64
force_inline u64 u128_to_u64(u128 val) {
  return val.lo;
}

//=============================================================================
//- s64 Operations (using portable s128)
//=============================================================================
force_inline s64 add_s64(s64 a, s64 b) { 
  return s128_to_s64(s128_add(s128_from_s64(a), s128_from_s64(b)));
}
force_inline s64 sub_s64(s64 a, s64 b) {
  return s128_to_s64(s128_sub(s128_from_s64(a), s128_from_s64(b)));
}
force_inline s64 mul_s64(s64 a, s64 b) {
  return s128_to_s64(s128_mul(s128_from_s64(a), s128_from_s64(b)));
}
force_inline s64 neg_s64(s64 a) {
  return s128_to_s64(s128_neg(s128_from_s64(a)));
}
force_inline s64 div_s64(s64 a, s64 b) {
  if (b == 0) return 0;
  if (a == INT64_MIN && b == -1) return INT64_MIN;
  return a / b;
}
force_inline s64 mod_s64(s64 a, s64 b) {
  if (b == 0) return 0;
  if (a == INT64_MIN && b == -1) return 0;
  return a % b;
}
force_inline s64 shl_s64(s64 a, s64 b) { return (b >= 0 && b < 64) ? (s64)((u64)a << b) : 0; }
force_inline s64 shr_s64(s64 a, s64 b) {
  if (b < 0 || b >= 64) return a < 0 ? -1 : 0;
  u64 ua = (u64)a;
  if (a >= 0) return (s64)(ua >> b);
  return (s64)((ua >> b) | ~(~0ull >> b));
}
force_inline s64 and_s64(s64 a, s64 b) { return a & b; }
force_inline s64 or_s64(s64 a, s64 b) { return a | b; }
force_inline s64 xor_s64(s64 a, s64 b) { return a ^ b; }
force_inline s64 not_s64(s64 a) { return ~a; }
force_inline bool lt_s64(s64 a, s64 b) { return a < b; }
force_inline bool gt_s64(s64 a, s64 b) { return a > b; }
force_inline bool lte_s64(s64 a, s64 b) { return a <= b; }
force_inline bool gte_s64(s64 a, s64 b) { return a >= b; }
force_inline bool eq_s64(s64 a, s64 b) { return a == b; }
force_inline bool neq_s64(s64 a, s64 b) { return a != b; }

//=============================================================================
//- u64 Operations (using portable u128)
//=============================================================================
force_inline u64 add_u64(u64 a, u64 b) { return a + b; }  // Unsigned wrapping is well-defined
force_inline u64 sub_u64(u64 a, u64 b) { return a - b; }
force_inline u64 mul_u64(u64 a, u64 b) { 
  return u128_to_u64(u128_mul(u128_from_u64(a), u128_from_u64(b)));
}
force_inline u64 div_u64(u64 a, u64 b) { return b == 0 ? 0 : a / b; }
force_inline u64 mod_u64(u64 a, u64 b) { return b == 0 ? 0 : a % b; }
force_inline u64 shl_u64(u64 a, u64 b) { return (b < 64) ? (a << b) : 0; }
force_inline u64 shr_u64(u64 a, u64 b) { return (b < 64) ? (a >> b) : 0; }
force_inline u64 and_u64(u64 a, u64 b) { return a & b; }
force_inline u64 or_u64(u64 a, u64 b) { return a | b; }
force_inline u64 xor_u64(u64 a, u64 b) { return a ^ b; }
force_inline u64 not_u64(u64 a) { return ~a; }
force_inline bool lt_u64(u64 a, u64 b) { return a < b; }
force_inline bool gt_u64(u64 a, u64 b) { return a > b; }
force_inline bool lte_u64(u64 a, u64 b) { return a <= b; }
force_inline bool gte_u64(u64 a, u64 b) { return a >= b; }
force_inline bool eq_u64(u64 a, u64 b) { return a == b; }
force_inline bool neq_u64(u64 a, u64 b) { return a != b; }

//=============================================================================
//- Logical Operations (bool)
//=============================================================================
force_inline bool not_bool(bool val) { return !val; }
force_inline bool and_bool(bool lhs, bool rhs) { return lhs && rhs; }
force_inline bool or_bool(bool lhs, bool rhs) { return lhs || rhs; }

//=============================================================================
//- Conversion Operations (make implementation-defined behavior explicit)
//=============================================================================
force_inline bool s32_to_bool(s32 val) { return val != 0; }
force_inline s32 bool_to_s32(bool val) { return val ? 1 : 0; }

//=============================================================================
//- Floating Point Operations (no UB but provided for uniformity)
//=============================================================================
force_inline f32 add_f32(f32 a, f32 b) { return a + b; }
force_inline f32 sub_f32(f32 a, f32 b) { return a - b; }
force_inline f32 mul_f32(f32 a, f32 b) { return a * b; }
force_inline f32 div_f32(f32 a, f32 b) { return a / b; }  // Division by 0.0 is well-defined (inf/nan)
force_inline f32 neg_f32(f32 val) { return -val; }
force_inline bool lt_f32(f32 a, f32 b) { return a < b; }
force_inline bool gt_f32(f32 a, f32 b) { return a > b; }
force_inline bool lte_f32(f32 a, f32 b) { return a <= b; }
force_inline bool gte_f32(f32 a, f32 b) { return a >= b; }
force_inline bool eq_f32(f32 a, f32 b) { return a == b; }
force_inline bool neq_f32(f32 a, f32 b) { return a != b; }

force_inline f64 add_f64(f64 a, f64 b) { return a + b; }
force_inline f64 sub_f64(f64 a, f64 b) { return a - b; }
force_inline f64 mul_f64(f64 a, f64 b) { return a * b; }
force_inline f64 div_f64(f64 a, f64 b) { return a / b; }
force_inline f64 neg_f64(f64 val) { return -val; }
force_inline bool lt_f64(f64 a, f64 b) { return a < b; }
force_inline bool gt_f64(f64 a, f64 b) { return a > b; }
force_inline bool lte_f64(f64 a, f64 b) { return a <= b; }
force_inline bool gte_f64(f64 a, f64 b) { return a >= b; }
force_inline bool eq_f64(f64 a, f64 b) { return a == b; }
force_inline bool neq_f64(f64 a, f64 b) { return a != b; }

//=============================================================================
//- Memory Operations
//=============================================================================
// #define ZERO_INIT(type) ((type){0})
