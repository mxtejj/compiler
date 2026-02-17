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
