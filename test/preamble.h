#include <stdint.h>
#include <stdio.h>

#define COMPILER_MSVC  0
#define COMPILER_CLANG 0
#define COMPILER_GCC   0

#if defined(__clang__)
#  undef  COMPILER_CLANG
#  define COMPILER_CLANG 1
#elif defined(_MSC_VER)
#  undef  COMPILER_MSVC
#  define COMPILER_MSVC 1
#elif defined(__GNUC__)
#  undef  COMPILER_GCC
#  define COMPILER_GCC 1
#else
#  error "Unsupported compiler"
#endif

/////////////////////////
// OS detection

#define OS_WINDOWS 0
#define OS_LINUX   0
#define OS_MAC     0

#if defined(_WIN32)
#  undef  OS_WINDOWS
#  define OS_WINDOWS 1
#elif defined(__linux__)
#  undef  OS_LINUX
#  define OS_LINUX 1
#elif defined(__APPLE__) && defined(__MACH__)
#  undef  OS_MAC
#  define OS_MAC 1
#else
#  error "Unsupported OS"
#endif

/////////////////////////
// Architecture detection

#define ARCH_X64    0
#define ARCH_X86    0
#define ARCH_ARM32  0
#define ARCH_ARM64  0

#if defined(_M_AMD64) || defined(_M_X64) || defined(__x86_64__)
#  undef  ARCH_X64
#  define ARCH_X64 1
#elif defined(_M_IX86) || defined(__i386__)
#  undef  ARCH_X86
#  define ARCH_X86 1
#elif defined(_M_ARM) || defined(__arm__)
#  undef  ARCH_ARM32
#  define ARCH_ARM32 1
#elif defined(_M_ARM64) || defined(__aarch64__)
#  undef  ARCH_ARM64
#  define ARCH_ARM64 1
#else
#  error "Unsupported architecture"
#endif

#if ARCH_X86 || ARCH_ARM32
#  error "Only 64-bit architectures supported"
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
typedef int8_t   i8;
typedef int16_t  i16;
typedef int32_t  i32;
typedef int64_t  i64;
typedef float    f32;
typedef double   f64;
typedef i8       b8;
typedef i16      b16;
typedef i32      b32;
typedef i64      b64;
typedef b8       bool;
typedef uintptr_t uintptr;

#define true 1
#define false 0

typedef struct {
  u8 *data;
  u64 len;
} string;

typedef u8  *cstring;
typedef u16 *cstring16;

#define STR(s) ((string){(u8*)(s), sizeof(s)-1})

//
//- Safe Operations for All Integer Types
// Eliminates UB from: signed overflow, shift >= bitwidth, negative shifts,
// division by zero, and makes implementation-defined behavior explicit.
//

//=============================================================================
//- i8 Operations (use i16 intermediate to avoid overflow)
//=============================================================================
force_inline i8 add_i8(i8 a, i8 b) { return (i8)((i16)a + (i16)b); }
force_inline i8 sub_i8(i8 a, i8 b) { return (i8)((i16)a - (i16)b); }
force_inline i8 mul_i8(i8 a, i8 b) { return (i8)((i16)a * (i16)b); }
force_inline i8 div_i8(i8 a, i8 b) { 
  if (b == 0) return 0;
  if (a == INT8_MIN && b == -1) return INT8_MIN;
  return a / b;
}
force_inline i8 mod_i8(i8 a, i8 b) {
  if (b == 0) return 0;
  if (a == INT8_MIN && b == -1) return 0;
  return a % b;
}
force_inline i8 neg_i8(i8 a) { return (i8)(-(i16)a); }
force_inline i8 shl_i8(i8 a, i8 b) { return (b >= 0 && b < 8) ? (i8)((u8)a << b) : 0; }
force_inline i8 shr_i8(i8 a, i8 b) {
  if (b < 0 || b >= 8) return a < 0 ? -1 : 0;
  // Arithmetic right shift with sign extension
  return (i8)((i16)a >> b);
}
force_inline i8 and_i8(i8 a, i8 b) { return a & b; }
force_inline i8 or_i8(i8 a, i8 b) { return a | b; }
force_inline i8 xor_i8(i8 a, i8 b) { return a ^ b; }
force_inline i8 not_i8(i8 a) { return ~a; }
force_inline bool lt_i8(i8 a, i8 b) { return a < b; }
force_inline bool gt_i8(i8 a, i8 b) { return a > b; }
force_inline bool lte_i8(i8 a, i8 b) { return a <= b; }
force_inline bool gte_i8(i8 a, i8 b) { return a >= b; }
force_inline bool eq_i8(i8 a, i8 b) { return a == b; }
force_inline bool neq_i8(i8 a, i8 b) { return a != b; }

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
//- i16 Operations (use i32 intermediate)
//=============================================================================
force_inline i16 add_i16(i16 a, i16 b) { return (i16)((i32)a + (i32)b); }
force_inline i16 sub_i16(i16 a, i16 b) { return (i16)((i32)a - (i32)b); }
force_inline i16 mul_i16(i16 a, i16 b) { return (i16)((i32)a * (i32)b); }
force_inline i16 div_i16(i16 a, i16 b) {
  if (b == 0) return 0;
  if (a == INT16_MIN && b == -1) return INT16_MIN;
  return a / b;
}
force_inline i16 mod_i16(i16 a, i16 b) {
  if (b == 0) return 0;
  if (a == INT16_MIN && b == -1) return 0;
  return a % b;
}
force_inline i16 neg_i16(i16 a) { return (i16)(-(i32)a); }
force_inline i16 shl_i16(i16 a, i16 b) { return (b >= 0 && b < 16) ? (i16)((u16)a << b) : 0; }
force_inline i16 shr_i16(i16 a, i16 b) {
  if (b < 0 || b >= 16) return a < 0 ? -1 : 0;
  // Arithmetic right shift with sign extension
  return (i16)((i32)a >> b);
}
force_inline i16 and_i16(i16 a, i16 b) { return a & b; }
force_inline i16 or_i16(i16 a, i16 b) { return a | b; }
force_inline i16 xor_i16(i16 a, i16 b) { return a ^ b; }
force_inline i16 not_i16(i16 a) { return ~a; }
force_inline bool lt_i16(i16 a, i16 b) { return a < b; }
force_inline bool gt_i16(i16 a, i16 b) { return a > b; }
force_inline bool lte_i16(i16 a, i16 b) { return a <= b; }
force_inline bool gte_i16(i16 a, i16 b) { return a >= b; }
force_inline bool eq_i16(i16 a, i16 b) { return a == b; }
force_inline bool neq_i16(i16 a, i16 b) { return a != b; }

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
//- i32 Operations (use i64 intermediate)
//=============================================================================
force_inline i32 add_i32(i32 a, i32 b) { return (i32)((i64)a + (i64)b); }
force_inline i32 sub_i32(i32 a, i32 b) { return (i32)((i64)a - (i64)b); }
force_inline i32 mul_i32(i32 a, i32 b) { return (i32)((i64)a * (i64)b); }
force_inline i32 div_i32(i32 a, i32 b) {
  if (b == 0) return 0;
  if (a == INT32_MIN && b == -1) return INT32_MIN;
  return a / b;
}
force_inline i32 mod_i32(i32 a, i32 b) {
  if (b == 0) return 0;
  if (a == INT32_MIN && b == -1) return 0;
  return a % b;
}
force_inline i32 neg_i32(i32 a) { return (i32)(-(i64)a); }
force_inline i32 shl_i32(i32 a, i32 b) { return (b >= 0 && b < 32) ? (i32)((u32)a << b) : 0; }
force_inline i32 shr_i32(i32 a, i32 b) {
  if (b < 0 || b >= 32) return a < 0 ? -1 : 0;
  u32 ua = (u32)a;
  if (a >= 0) return (i32)(ua >> b);
  return (i32)((ua >> b) | ~(~0u >> b));
}
force_inline i32 and_i32(i32 a, i32 b) { return a & b; }
force_inline i32 or_i32(i32 a, i32 b) { return a | b; }
force_inline i32 xor_i32(i32 a, i32 b) { return a ^ b; }
force_inline i32 not_i32(i32 a) { return ~a; }
force_inline bool lt_i32(i32 a, i32 b) { return a < b; }
force_inline bool gt_i32(i32 a, i32 b) { return a > b; }
force_inline bool lte_i32(i32 a, i32 b) { return a <= b; }
force_inline bool gte_i32(i32 a, i32 b) { return a >= b; }
force_inline bool eq_i32(i32 a, i32 b) { return a == b; }
force_inline bool neq_i32(i32 a, i32 b) { return a != b; }

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
//- Portable 128-bit Integer Type (for i64/u64 overflow prevention)
//=============================================================================

// 128-bit signed integer (stored as two 64-bit parts)
typedef struct {
  u64 lo;  // Low 64 bits
  i64 hi;  // High 64 bits (signed for i128)
} i128;

// 128-bit unsigned integer
typedef struct {
  u64 lo;  // Low 64 bits
  u64 hi;  // High 64 bits
} u128;

// Create i128 from i64
force_inline i128 i128_from_i64(i64 val) {
  i128 result;
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

// i128 addition
force_inline i128 i128_add(i128 a, i128 b) {
  i128 result;
  result.lo = a.lo + b.lo;
  result.hi = a.hi + b.hi + (result.lo < a.lo ? 1 : 0);  // Carry
  return result;
}

// i128 subtraction
force_inline i128 i128_sub(i128 a, i128 b) {
  i128 result;
  result.lo = a.lo - b.lo;
  result.hi = a.hi - b.hi - (a.lo < b.lo ? 1 : 0);  // Borrow
  return result;
}

// i128 multiplication (simple version - just low 128 bits of result)
force_inline i128 i128_mul(i128 a, i128 b) {
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
  
  i128 result;
  result.lo = p0 + (p1 << 32) + (p2 << 32);
  result.hi = p3 + (p1 >> 32) + (p2 >> 32) + carry;
  result.hi += a.hi * b_lo + a_lo * b.hi;  // Cross terms
  
  return result;
}

// i128 negation
force_inline i128 i128_neg(i128 a) {
  i128 zero = {0, 0};
  return i128_sub(zero, a);
}

// Convert i128 back to i64 (truncate high bits)
force_inline i64 i128_to_i64(i128 val) {
  return (i64)val.lo;
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
//- i64 Operations (using portable i128)
//=============================================================================
force_inline i64 add_i64(i64 a, i64 b) { 
  return i128_to_i64(i128_add(i128_from_i64(a), i128_from_i64(b)));
}
force_inline i64 sub_i64(i64 a, i64 b) {
  return i128_to_i64(i128_sub(i128_from_i64(a), i128_from_i64(b)));
}
force_inline i64 mul_i64(i64 a, i64 b) {
  return i128_to_i64(i128_mul(i128_from_i64(a), i128_from_i64(b)));
}
force_inline i64 neg_i64(i64 a) {
  return i128_to_i64(i128_neg(i128_from_i64(a)));
}
force_inline i64 div_i64(i64 a, i64 b) {
  if (b == 0) return 0;
  if (a == INT64_MIN && b == -1) return INT64_MIN;
  return a / b;
}
force_inline i64 mod_i64(i64 a, i64 b) {
  if (b == 0) return 0;
  if (a == INT64_MIN && b == -1) return 0;
  return a % b;
}
force_inline i64 shl_i64(i64 a, i64 b) { return (b >= 0 && b < 64) ? (i64)((u64)a << b) : 0; }
force_inline i64 shr_i64(i64 a, i64 b) {
  if (b < 0 || b >= 64) return a < 0 ? -1 : 0;
  u64 ua = (u64)a;
  if (a >= 0) return (i64)(ua >> b);
  return (i64)((ua >> b) | ~(~0ull >> b));
}
force_inline i64 and_i64(i64 a, i64 b) { return a & b; }
force_inline i64 or_i64(i64 a, i64 b) { return a | b; }
force_inline i64 xor_i64(i64 a, i64 b) { return a ^ b; }
force_inline i64 not_i64(i64 a) { return ~a; }
force_inline bool lt_i64(i64 a, i64 b) { return a < b; }
force_inline bool gt_i64(i64 a, i64 b) { return a > b; }
force_inline bool lte_i64(i64 a, i64 b) { return a <= b; }
force_inline bool gte_i64(i64 a, i64 b) { return a >= b; }
force_inline bool eq_i64(i64 a, i64 b) { return a == b; }
force_inline bool neq_i64(i64 a, i64 b) { return a != b; }

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
force_inline bool i32_to_bool(i32 val) { return val != 0; }
force_inline i32 bool_to_i32(bool val) { return val ? 1 : 0; }

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
