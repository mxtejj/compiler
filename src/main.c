#include <stdio.h>

// HEADER INCLUDES
#include "base.h"
#include "arena.h"
#include "lexer.h"
#include "string.h"
#include "os.h"
#include "ast.h"
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
    String expr_tests[] =
    {
      // literals
      S("1"),
      S("42"),
      S("true"),
      S("false"),
      S("nil"),
      S("\"hello\""),

      // unary
      S("-1"),      // (- 1)
      S("!true"),   // (! true)
      S("!!false"), // (! (! false))
      S("--1"),     // (- (- 1))
      S("-!false"), // (- (! false))

      // binary precedence
      S("1 + 2 * 3"),
      S("1 * 2 + 3"),
      S("4 / 2 + 8"),

      // paren override precedence
      S("(1 + 2) * 3"), // (* (+ 1 2) 3)
      S("1 * (2 + 3)"), // (* 1 (+ 2 3))
      S("((1))"),       // 1

      // associativity
      S("1 - 2 - 3"), // (- (- 1 2) 3)
      S("8 / 4 / 2"), // (/ (/ 8 4) 2)

      // comparisons
      S("1 < 2"),         // (< 1 2)
      S("1 < 2 + 3"),     // (< 1 (+ 2 3))
      S("1 + 2 < 3 * 4"), // (< (+ 1 2) (* 3 4))

      // equality chains
      // allowed syntactically - semantic phase can reject later
      S("1 == 2"),         // (== 1 2)
      S("1 + 2 == 3 * 4"), // (== (+ 1 2) (* 3 4))
      S("1 == 2 == 3"),    // (== (== 1 2) 3)

      // mixed everything
      S("1 + 2 * 3 == 7"),    // (== (+ 1 (* 2 3)) 7)
      S("(1 + 2) * (3 + 4)"), // (* (+ 1 2) (+ 3 4))
      S("!(1 + 2 * 3 < 10)"), // (! (< (+ 1 (* 2 3)) 10))

      // identifiers
      S("a"),           // a
      S("a + b * c"),   // (+ a (* b c))
      S("(x + y) * z"), // (* (+ x y) z)

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
      padding = MAX(padding, expr_tests[i].count);
    }

    for (u32 i = 0; i < array_count(expr_tests); i++)
    {
      Lexer l = lexer_init(expr_tests[i]);
      Parser p = parser_init(&l);

      AST *e = parse_expression(&p);

      printf("%.*s  " CLR_GRN "%-*s=>" CLR_YEL "  ", strf(l.source), (int)(padding - l.source.count), "");
      ast_print(&e->expr);
      printf("\n" CLR_RESET);
    }

    int a = 5;
  }

  arena_delete(g_arena);

  return 0;
}
