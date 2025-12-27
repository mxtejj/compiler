#include <stdio.h>
#include <stdarg.h>

#if !_WIN32
# define RADDBG_MARKUP_STUBS
#endif
#define RADDBG_MARKUP_IMPLEMENTATION
#include "raddbg_markup.h"

// HEADER INCLUDES
#include "base.h"
#include "arena.h"
#include "lexer.h"
#include "string.h"
#include "os.h"
#include "parser.h"

// SOURCE INCLUDES
#include "arena.c"
#include "lexer.c"
#include "string.c"
#include "ast.c"
#include "parser.c"
#ifdef _WIN32
# include "os_win32.c"
#else
# error "OS layer not implemented"
#endif

internal void
usage(const char *program_name)
{
  printf("Usage: %s [INPUT]\n", program_name);
}

#if _WIN32
# define WIN32_LEAN_AND_MEAN
# include <windows.h>

internal void
win32_enable_vt_mode()
{
  HANDLE hOut = GetStdHandle(STD_OUTPUT_HANDLE);
  DWORD dwMode = 0;

  if (hOut == INVALID_HANDLE_VALUE)   return;
  if (!GetConsoleMode(hOut, &dwMode)) return;

  dwMode |= ENABLE_VIRTUAL_TERMINAL_PROCESSING;
  SetConsoleMode(hOut, dwMode);
}
#endif

Arena *g_arena;

internal const char *
tprint(const char *fmt, ...)
{
  local_persist char buf[256];
  va_list args;
  va_start(args, fmt);
  int n = vsnprintf(buf, sizeof(buf), fmt, args);
  va_end(fmt);
  buf[n] = 0;
  return buf;
}

int
main(int argc, char **argv)
{
#if 0
  const char *program_name = shift_args(argc, argv);
  if (argc == 0)
  {
    usage(program_name);
    return 0;
  }
#endif

#if _WIN32
  win32_enable_vt_mode();
#endif

  g_arena = arena_alloc(GB(1), MB(8), 0);

  String source = S(
    "struct Person\n"
    "{\n"
    "  name: string;\n"
    "  age:  int;\n"
    "}\n"
    "\n"
    "\n"
    "fn main()\n"
    "{\n"
   // "  var a = 18446744073709551616;\n"
    "  var hex = 0xCAFEBABE;\n"
    "  var bin = 0b10010101010;\n"
    "  var oct = 0o10;\n"
    "  123.456;\n"
    "  1e+600;\n"
    "  1e3;\n"     // 1000
    "  1e-3;\n"    // 0.001
    "  1.5e2;\n"   // 150
    "  3.14e0;\n"  // 3.14
    "  0.5e+10;\n"
    "  1e10;\n"
    "  .5;\n"
    "  42E-1;\n"
    "  0e0;\n"
    // "  .5e2;\n" // TODO: Support leading decimal (.5), not trailing (5.)?
    "  5.e-1;\n"
    "  0.0e0;\n"
    // ERRORS
    // "  .;\n" // TODO: this gets lexed as '.' token
    // "  e10;\n"
    // "  1e;\n"
    // "  1e+;\n"
    // "  1.2.3;\n" // TODO: FLOAT(1.2) '.' INT(3)
    // "  1ee10;\n"
    "  print(\"Hello KK!\");\n"
    "}\n"
  );

  {
    // READ SOURCE FILE
    Lexer l = lexer_init(source);

    Token t;
    while ((t = lexer_next(&l)).kind != TOKEN_EOF)
    {
      printf("%.*s %.*s", strf(str_from_token_kind(t.kind)), strf(t.lexeme));
      switch (t.kind)
      {
      case TOKEN_INTEGER_LITERAL:
        printf(" (value=%llu)", t.value.integer);
        break;
      case TOKEN_FLOAT_LITERAL:
        printf(" (value=%f)", t.value.floating);
        break;
      case TOKEN_STRING_LITERAL:
        printf(" (value=%.*s)", strf(t.value.string));
        break;
      }
      printf("\n");
    }
  }

  printf("\n");

  if (0)
  {
    // LEXER TEST
    Lexer l = lexer_init(source);
    Token t;

    #define expect_token(x) \
      do { \
        t = lexer_next(&l); \
        printf("%3d: expecting %s, got %.*s (%.*s)\n", __LINE__, #x, strf(str_from_token_kind(t.kind)), strf(t.lexeme)); \
        assert(t.kind == (x)); \
      } while (0); \

    expect_token(TOKEN_STRUCT);
    expect_token(TOKEN_IDENT);
    expect_token('{');

    // name: string
    expect_token(TOKEN_IDENT);
    expect_token(':');
    expect_token(TOKEN_STRING);
    expect_token(';');

    // age:  int;
    expect_token(TOKEN_IDENT);
    expect_token(':');
    expect_token(TOKEN_INT);
    expect_token(';');

    expect_token('}');

    // fn main()
    expect_token(TOKEN_FN);
    expect_token(TOKEN_IDENT);
    expect_token('(');
    expect_token(')');
    expect_token('{');

    // var hex = ..;
    expect_token(TOKEN_VAR);
    expect_token(TOKEN_IDENT);
    expect_token('=');
    expect_token(TOKEN_INTEGER_LITERAL);
    expect_token(';');

    // var bin = ..;
    expect_token(TOKEN_VAR);
    expect_token(TOKEN_IDENT);
    expect_token('=');
    expect_token(TOKEN_INTEGER_LITERAL);
    expect_token(';');

    // var oct = ..;
    expect_token(TOKEN_VAR);
    expect_token(TOKEN_IDENT);
    expect_token('=');
    expect_token(TOKEN_INTEGER_LITERAL);
    expect_token(';');

    // var pi = ..;
    expect_token(TOKEN_VAR);
    expect_token(TOKEN_IDENT);
    expect_token('=');
    expect_token(TOKEN_FLOAT_LITERAL);
    expect_token(';');

    // print(...);
    expect_token(TOKEN_IDENT);
    expect_token('(');
    expect_token(TOKEN_STRING_LITERAL);
    expect_token(')');
    expect_token(';');

    expect_token('}');

    assert(lexer_next(&l).kind == TOKEN_EOF);

    #undef expect_token
    printf("[Lexer Test] OK\n");
  }

  printf("\n");

  {
    typedef struct Expr_Test_Case Expr_Test_Case;
    struct Expr_Test_Case
    {
      String input;
      String output;
    };

    Expr_Test_Case expr_tests[] =
    {
      // literals
      { .input = S("1"),         .output = S("1") },
      { .input = S("42"),        .output = S("42") },
      { .input = S("true"),      .output = S("true") },
      { .input = S("false"),     .output = S("false") },
      { .input = S("nil"),       .output = S("nil") },
      { .input = S("\"hello\""), .output = S("hello") },

      // unary
      { .input = S("-1"),      .output = S("(- 1)") },
      { .input = S("!true"),   .output = S("(! true)") },
      { .input = S("!!false"), .output = S("(! (! false))") },
      { .input = S("--1"),     .output = S("(- (- 1))") },
      { .input = S("-!false"), .output = S("(- (! false))") },

      // binary precedence
      { .input = S("1 + 2 * 3"), .output = S("(+ 1 (* 2 3))") },
      { .input = S("1 * 2 + 3"), .output = S("(+ (* 1 2) 3)") },
      { .input = S("4 / 2 + 8"), .output = S("(+ (/ 4 2) 8)") },

      // ternary
      { .input = S("a > b ? a : b"),                             .output = S("(?: (> a b) a b)") },
      { .input = S("a > b ?  a > c ? a : c  :  b > c ? b : c"),  .output = S("(?: (> a b) (?: (> a c) a c) (?: (> b c) b c))") },
      { .input = S("a > b ? (a > c ? a : c) : (b > c ? b : c)"), .output = S("(?: (> a b) (group (?: (> a c) a c)) (group (?: (> b c) b c)))") },

      // paren override precedence
      { .input = S("(1 + 2) * 3"),       .output = S("(* (group (+ 1 2)) 3)") },
      { .input = S("1 * (2 + 3)"),       .output = S("(* 1 (group (+ 2 3)))") },
      { .input = S("((1))"),             .output = S("(group (group 1))") },

      // associativity
      { .input = S("1 - 2 - 3"),         .output = S("(- (- 1 2) 3)") },
      { .input = S("8 / 4 / 2"),         .output = S("(/ (/ 8 4) 2)") },

      // comparisons
      { .input = S("1 < 2"),             .output = S("(< 1 2)") },
      { .input = S("1 < 2 + 3"),         .output = S("(< 1 (+ 2 3))") },
      { .input = S("1 + 2 < 3 * 4"),     .output = S("(< (+ 1 2) (* 3 4))") },

      // equality chains
      // allowed syntactically - semantic phase can reject later
      { .input = S("1 == 2"),            .output = S("(== 1 2)") },
      { .input = S("1 + 2 == 3 * 4"),    .output = S("(== (+ 1 2) (* 3 4))") },
      { .input = S("1 == 2 == 3"),       .output = S("(== (== 1 2) 3)") },

      // mixed everything
      { .input = S("1 + 2 * 3 == 7"),    .output = S("(== (+ 1 (* 2 3)) 7)") },
      { .input = S("(1 + 2) * (3 + 4)"), .output = S("(* (group (+ 1 2)) (group (+ 3 4)))") },
      { .input = S("!(1 + 2 * 3 < 10)"), .output = S("(! (group (< (+ 1 (* 2 3)) 10)))") },

      // identifiers
      { .input = S("a"),                 .output = S("a") },
      { .input = S("a + b * c"),         .output = S("(+ a (* b c))") },
      { .input = S("(x + y) * z"),       .output = S("(* (group (+ x y)) z)") },

      { .input = S("1 | 2 & 3"),         .output = S("(| 1 (& 2 3))") },
      { .input = S("1 ~ 2 & 3"),         .output = S("(~ 1 (& 2 3))") },
      { .input = S("1 << 2 + 1"),        .output = S("(<< 1 (+ 2 1))") },
      { .input = S("1 + 2 << 3"),        .output = S("(<< (+ 1 2) 3)") },
      { .input = S("1 < 2 | 3"),         .output = S("(< 1 (| 2 3))") },
      { .input = S("1 & 2 == 0"),        .output = S("(== (& 1 2) 0)") },
      { .input = S("1 && 2 | 0"),        .output = S("(&& 1 (| 2 0))") },
      { .input = S("1 || 0 && 0"),       .output = S("(|| 1 (&& 0 0))") },
      { .input = S("a = b = c"),         .output = S("(= a (= b c))") },
      { .input = S("a += b * c"),        .output = S("(+= a (* b c))") },
      { .input = S("a <<= 1 + 2"),       .output = S("(<<= a (+ 1 2))") },
      { .input = S("a |= b & c"),        .output = S("(|= a (& b c))") },
      { .input = S("a = b ? c : d"),     .output = S("(= a (?: b c d))") },
      { .input = S("a += b += c"),       .output = S("(+= a (+= b c))") },

      { .input = S("flags & FLAG_MASK == SOME_FLAG"), .output = S("(== (& flags FLAG_MASK) SOME_FLAG)") },
      { .input = S("---x"),                           .output = S("(- (- (- x)))") },

      // error cases
      // expect => clean error, no crash, no infinite loop
      // S(""),
      // S("+ 1"),
      // S("1 +"),
      // S("(1 + 2"),
      // S("1 * * 2"),
    };

    u64 padding = 0;
    for (u32 i = 0; i < array_count(expr_tests); i++)
    {
      padding = MAX(padding, expr_tests[i].input.count);
    }

    for (u32 i = 0; i < array_count(expr_tests); i++)
    {
      Expr_Test_Case test = expr_tests[i];

      char buf[64];

      Lexer l = lexer_init(test.input);
      Parser p = parser_init(&l);

      AST *e = parse_expression(&p);
      usize n = print_expr(buf, sizeof(buf), &e->expr);

      printf("%.*s  " CLR_GRN "%-*s=>" CLR_YEL "  ", strf(l.source), (int)(padding - l.source.count), "");
      printf("%.*s", (int)n, buf);
      printf("\n" CLR_RESET);

      if (!mem_equal(buf, test.output.data, n))
      {
        printf("Expected " CLR_GRN "%.*s" CLR_RESET ", got " CLR_RED "%.*s\n" CLR_RESET, strf(test.output), (int)n, buf);
        assert(!"Expression test failed");
      }
    }

    int a = 5;
  }

  printf("\n");

  {
    parser_test();
  }

  arena_delete(g_arena);

  return 0;
}
