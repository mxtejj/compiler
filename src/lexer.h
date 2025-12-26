#pragma once

#include "base.h"
#include "string.h"
#include "arena.h"

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
  X(EQ)               \
  X(NEQ)              \
  X(GTEQ)             \
  X(LTEQ)             \
  X(STRING_LITERAL)   \
  X(INTEGER_LITERAL)  \
  X(FLOAT_LITERAL)    \
  X(KEYWORD_BEGIN)    \
  X(NIL)              \
  X(S8)               \
  X(S16)              \
  X(S32)              \
  X(S64)              \
  X(U8)               \
  X(U16)              \
  X(U32)              \
  X(U64)              \
  X(UINTPTR)          \
  X(INT)              \
  X(UINT)             \
  X(F32)              \
  X(F64)              \
  X(BOOL)             \
  X(TRUE)             \
  X(FALSE)            \
  X(IF)               \
  X(ELSE)             \
  X(FOR)              \
  X(WHILE)            \
  X(SWITCH)           \
  X(CASE)             \
  X(BREAK)            \
  X(CONTINUE)         \
  X(RETURN)           \
  X(STRUCT)           \
  X(UNION)            \
  X(ENUM)             \
  X(STRING)           \
  X(FN)               \
  X(VAR)              \
  X(CONST)            \
  X(SIZE_OF)          \
  X(CAST)             \
  X(TRANSMUTE)        \
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

typedef union Literal_Value Literal_Value;
union Literal_Value
{
  String string;
  u64    integer;
  f64    floating;
  bool   boolean;
};

typedef struct Token Token;
struct Token
{
  Token_Kind kind;
  String     lexeme;
  Literal_Value value;
};

typedef struct Lexer Lexer;
struct Lexer
{
  Arena *arena;
  String source;
  u64 start;
  u64 cursor;
  u64 line;
  u64 bol; // beginning of line
};

Lexer lexer_init(String source);
void  lexer_fini(Lexer *l);

Token lexer_next(Lexer *l);
