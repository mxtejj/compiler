#pragma once

#include "base.h"
#include "strings.h"
#include "arena.h"

#define X_ENUM(name) name,
#define X_STRING(name) #name,

#define TOKEN_LIST(X) \
  X(IDENT)            \
  X(ARROW)            \
  X(DEREF)            \
  X(EQ)               \
  X(NEQ)              \
  X(GTEQ)             \
  X(LTEQ)             \
  X(LOGICAL_OR)       \
  X(LOGICAL_AND)      \
  X(LSHIFT)           \
  X(RSHIFT)           \
  X(LSHIFT_ASSIGN)    \
  X(RSHIFT_ASSIGN)    \
  X(ADD_ASSIGN)       \
  X(SUB_ASSIGN)       \
  X(DIV_ASSIGN)       \
  X(MUL_ASSIGN)       \
  X(AND_ASSIGN)       \
  X(OR_ASSIGN)        \
  X(XOR_ASSIGN)       \
  X(INCREMENT)        \
  X(DECREMENT)        \
  X(RANGE_INCL)       \
  X(RANGE_EXCL)       \
  X(STRING_LITERAL)   \
  X(INTEGER_LITERAL)  \
  X(FLOAT_LITERAL)    \
  X(CHAR_LITERAL)     \
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
  X(STRING)           \
  X(TRUE)             \
  X(FALSE)            \
  X(IF)               \
  X(ELSE)             \
  X(FOR)              \
  X(DO)               \
  X(WHILE)            \
  X(SWITCH)           \
  X(CASE)             \
  X(DEFER)            \
  X(BREAK)            \
  X(FALLTHROUGH)      \
  X(CONTINUE)         \
  X(RETURN)           \
  X(STRUCT)           \
  X(UNION)            \
  X(ENUM)             \
  X(PROC)             \
  X(VAR)              \
  X(CONST)            \
  X(TYPEDEF)          \
  X(SIZE_OF)          \
  X(CAST)             \
  X(TRANSMUTE)        \
  X(IN)               \
  X(THEN)             \
  X(FOREIGN)          \
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

internal String8 str_from_token_kind(Arena *arena, Token_Kind kind);

typedef union Literal_Value Literal_Value;
union Literal_Value
{
  String8 string;
  u64     integer;
  f64     floating;
  bool    boolean;
  char    character;
};

typedef struct Source_Pos Source_Pos;
struct Source_Pos
{
  u64 row; // x
  u64 col; // y
  u64 length; // token length for error highlighting
};

typedef struct Token Token;
struct Token
{
  Token_Kind    kind;
  Source_Pos    pos;
  String8       lexeme;
  Literal_Value value;
};

typedef struct Lexer Lexer;
struct Lexer
{
  Arena *arena;
  String8 source;
  u64 start;
  u64 cursor;
  u64 line;
  u64 bol; // beginning of line
  b32 insert_semicolon;
};

internal Lexer lexer_init(String8 source);
internal void  lexer_fini(Lexer *l);
internal Token lexer_next(Lexer *l);
