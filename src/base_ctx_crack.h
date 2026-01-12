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
