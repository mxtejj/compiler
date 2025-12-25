#include <stdio.h>

// HEADER INCLUDES
#include "base.h"
#include "arena.h"
#include "lexer.h"
#include "string.h"
#include "os.h"

// SOURCE INCLUDES
#include "arena.c"
#include "lexer.c"
#include "string.c"
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

  String source = S(
    "fn main()\n"
    "{\n"
    "  print(\"Hello KK!\")\n"
    "}\n"
  );

  // READ SOURCE FILE
  Lexer l = lexer_init(source);

  Token t;
  while ((t = lexer_next(&l)).kind != TOKEN_EOF)
  {
    printf("%.*s = %.*s\n", strf(str_from_token_kind(t.kind)), strf(t.lexeme));
  }

  printf("\n");

  {
    // LEXER TEST
    String source = S(
      "struct Person\n"
      "{\n"
      "  name: string;\n"
      "  age:  int;\n"
      "}\n"
      "\n"
      "var a = 5;\n"
      "\n"
      "fn main()\n"
      "{\n"
      "  print(\"Hello KK!\");\n"
      "}\n"
    );

    // READ SOURCE FILE
    Lexer l = lexer_init(source);
    Token t;

    #define expect_token(x) \
      do { \
        t = lexer_next(&l); \
        printf("%3d: expecting %s, got %.*s\n", __LINE__, #x, strf(str_from_token_kind(t.kind))); \
        assert(t.kind == (x)); \
      } while (0); \

    expect_token(TOKEN_IDENT);
    expect_token(TOKEN_IDENT);
    expect_token('{');

    // name: string
    expect_token(TOKEN_IDENT);
    expect_token(':');
    expect_token(TOKEN_IDENT);
    expect_token(';');

    // age:  int;
    expect_token(TOKEN_IDENT);
    expect_token(':');
    expect_token(TOKEN_IDENT);
    expect_token(';');

    expect_token('}');

    // var a = 5;
    expect_token(TOKEN_IDENT);
    expect_token(TOKEN_IDENT);
    expect_token('=');
    expect_token('5'); // TODO TOKEN_INT
    expect_token(';');

    // fn main()
    expect_token(TOKEN_IDENT);
    expect_token(TOKEN_IDENT);
    expect_token('(');
    expect_token(')');

    // expect_token((...);
    expect_token('{');
    expect_token(TOKEN_IDENT);
    expect_token('(');
    expect_token(TOKEN_STRING);
    expect_token(')');
    expect_token(';');
    expect_token('}');

    assert(lexer_next(&l).kind == TOKEN_EOF);

    #undef expect_token
    printf("[Lexer Test] OK\n");
  }

  return 0;
}
