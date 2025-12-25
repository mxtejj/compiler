#pragma once

#include "string.h"
#include "base.h"

/*

name

unary:
+
-

binary:
/
*
[TODO: BIT OPS...]

ternary:
? :

*/

#define X_ENUM(name) name,
#define X_STRING(name) #name,

#define TOKEN_LIST(X) \
  X(IDENT)            \
  X(STRING)           \
  X(REAL)             \
  X(INTEGER)          \
                      \
  X(KEYWORD_BEGIN)    \
  X(VAR)              \
  X(FN)               \
  X(STRUCT)           \
  X(UNION)            \
  X(IF)               \
  X(ELSE)             \
  X(RETURN)           \
  X(TRUE)             \
  X(FALSE)            \
  X(WHILE)            \
  X(FOR)              \
  X(KEYWORD_END)      \

typedef enum Token_Kind Token_Kind;
enum Token_Kind
{
  TOKEN_EOF        = 0,
  TOKEN_LAST_ASCII = 127,

#define X(name) TOKEN_##name,
  TOKEN_LIST(X)
#undef X
};

String str_from_token_kind(Token_Kind kind);

typedef struct Token Token;
struct Token
{
  Token_Kind kind;
  String     lexeme;

  union
  {
    String str_value;
    u64    int_value;
    f64    real_value;
  };
};

typedef struct Lexer Lexer;
struct Lexer
{
  String source;
  u64 start;
  u64 cursor;
  u64 line;
  u64 bol; // beginning of line
};
