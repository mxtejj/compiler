#include <stdio.h>
#include <stdarg.h>

// HEADER INCLUDES
#include "base.h"
#include "arena.h"
#include "lexer.h"
#include "strings.h"
#include "os.h"
#include "parser.h"

#if !_WIN32
# define RADDBG_MARKUP_STUBS
#endif
#define RADDBG_MARKUP_IMPLEMENTATION
#include "raddbg_markup.h"

#define STB_SPRINTF_IMPLEMENTATION
#define STB_SPRINTF_STATIC
#include "modified_stb_sprintf.h"

// SOURCE INCLUDES
#include "arena.c"
#include "lexer.c"
#include "strings.c"
#include "ast.c"
#include "parser.c"
#include "resolve.c"
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
  Arena_Temp scratch = arena_scratch_get(0, 0);

  String8 source = S(
    "struct Person\n"
    "{\n"
    "  name: string,\n"
    "  age:  int,\n"
    "}\n"
    "\n"
    "\n"
    "proc main()\n"
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
      printf("%.*s %.*s", str8_varg(str_from_token_kind(scratch.arena, t.kind)), str8_varg(t.lexeme));
      switch (t.kind)
      {
      case TOKEN_INTEGER_LITERAL:
        printf(" (value=%llu)", t.value.integer);
        break;
      case TOKEN_FLOAT_LITERAL:
        printf(" (value=%f)", t.value.floating);
        break;
      case TOKEN_STRING_LITERAL:
        printf(" (value=%.*s)", str8_varg(t.value.string));
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
        printf("%3d: expecting %s, got %.*s (%.*s)\n", __LINE__, #x, str8_varg(str_from_token_kind(scratch.arena, t.kind)), str8_varg(t.lexeme)); \
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

    // proc main()
    expect_token(TOKEN_PROC);
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
      String8 input;
      String8 output;
    };

    Expr_Test_Case expr_tests[] =
    {
      #if 1
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

      { .input = S("foo()"),          .output = S("(call foo)") },
      { .input = S("bar(1 + 2, 3)"),  .output = S("(call bar (+ 1 2) 3)") },
      { .input = S("baz(1, 2, 3)"),   .output = S("(call baz 1 2 3)") },
      { .input = S("foo(a ? b : c)"), .output = S("(call foo (?: a b c))") },

      { .input = S("a.b(c)[i].d"), .output = S("(field (index (call (field a b) c) i) d)") },

      // Compound literals
      { .input = S("Person.{}"),                        .output = S("(compound Person)") },
      { .input = S("Person.{\"Bob\"}"),                 .output = S("(compound Person Bob)") },
      { .input = S("Person.{name = \"Bob\"}"),          .output = S("(compound Person name=Bob)") },
      { .input = S("Person.{name = \"Bob\", age = 5}"), .output = S("(compound Person name=Bob age=5)") },
      // { .input = S("Person{name = \"Bob\", 5}"), .output = S("(compound Person name=Bob age=5)") },
      { .input = S("[2]int.{69, 420}"),               .output = S("(compound [2]int 69 420)") },
      { .input = S("[2]*int.{}"),                     .output = S("(compound [2]*int)") },
      #endif
      { .input = S("[8]int.{1,2,3,4,5,6,7,8}"),  .output = S("(compound [8]int 1 2 3 4 5 6 7 8)") }, 
      { .input = S("[2][2]int.{.{1,2},.{3,4}}"), .output = S("(compound [2][2]int (compound nil 1 2) (compound nil 3 4))") },

      #if 1
      // Slice compound literals
      { .input = S("[]int.{1, 2, 3}"),                .output = S("(compound []int 1 2 3)") },
      { .input = S("[]string.{\"a\", \"b\", \"c\"}"), .output = S("(compound []string a b c)") },

      { .input = S("Person.{name=\"Bob\", scores=[]int.{10,20,30}}"), .output = S("(compound Person name=Bob scores=(compound []int 10 20 30))") },

      // TODO(mxtej): This test does not output the right result
      // { .input = S(
      //   "Foo{"
      //   "  a = Bar{"
      //   "    b = Baz{"
      //   "      values = []int{1, 2, 3}"
      //   "    }"
      //   "  }"
      //   "}"
      // ), .output = S("(compound Foo a=(compound Bar b=(compound Baz values=(compound []int 1 2 3))))") },

      // "Foo{a=Bar{b=Baz{values=[]int{1,2,3}}}}"

      { .input = S("[3]int.{1 + 2, foo(), bar * (baz + 4)}"), .output = S("(compound [3]int (+ 1 2) (call foo) (* bar (group (+ baz 4))))") },
      { .input = S("Person.{age = a > b ? 18 : 21}"),         .output = S("(compound Person age=(?: (> a b) 18 21))") },

      // Compound literals in expressions
      // { .input = S("var x = Point{1,2}"), .output = S("") },
      { .input = S("foo(Point.{a + b, c * d})"), .output = S("(call foo (compound Point (+ a b) (* c d)))") },
      { .input = S("arr[Point.{1,2}.x] = 5"),    .output = S("(= (index arr (field (compound Point 1 2) x)) 5)") },

      // Compound literals + postfix
      { .input = S("Person.{name = \"Alice\"}.name"), .output = S("(field (compound Person name=Alice) name)") },
      { .input = S("[2]int.{1, 2}[0]"),               .output = S("(index (compound [2]int 1 2) 0)") },

      // Dereference pointer
      { .input = S("ptr.* = 5;"),     .output = S("(= (.* ptr) 5)") },
      { .input = S("ptr.*.age = 5;"), .output = S("(= (field (.* ptr) age) 5)") },
      #endif

      // "a.b(c)[i].d++\n"
      // "foo()\n"

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

      char buf[128];

      Lexer l = lexer_init(test.input);
      Parser p = parser_init(&l);

      Arena_Temp scratch = arena_scratch_get(0, 0);

      String8List list = {0};

      int indent = 0;
      Expr *e = parse_expr(&p);
      print_expr(scratch.arena, &list, &indent, e);

      String8 result = str8_list_join(scratch.arena, &list, NULL);

      arena_scratch_release(scratch);

      printf("%.*s  " CLR_GRN "%-*s=>" CLR_YEL "  ", str8_varg(l.source), (int)(padding - l.source.count), "");
      printf("%.*s", str8_varg(result));
      printf("\n" CLR_RESET);

      if (!str8_equal(result, test.output))
      {
        printf("Expected " CLR_GRN "%.*s" CLR_RESET ", got " CLR_RED "%.*s\n" CLR_RESET, str8_varg(test.output), str8_varg(result));
        assert(!"Expression test failed");
      }
    }

    int a = 5;
  }

  printf("\n");
  parser_test();
  resolve_test();
  // order_test();

  arena_scratch_release(scratch);
  arena_delete(g_arena);

  return 0;
}
