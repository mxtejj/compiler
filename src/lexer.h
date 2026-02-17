#pragma once

#include "base.h"
#include "strings.h"
#include "arena.h"

typedef union  LiteralValue LiteralValue;
typedef struct SourcePos    SourcePos;
typedef struct Token        Token;
typedef struct Lexer        Lexer;

typedef enum TokenKind {
  TokenKind_EOF = 0,
  TokenKind_LParen,      // (
  TokenKind_SymbolBegin = TokenKind_LParen,
  TokenKind_RParen,      // )
  TokenKind_LBracket,    // [
  TokenKind_RBracket,    // ]
  TokenKind_LBrace,      // {
  TokenKind_RBrace,      // }
  TokenKind_Plus,        // +
  TokenKind_Minus,       // -
  TokenKind_Caret,       // ^
  TokenKind_Ampersand,   // &
  TokenKind_Pipe,        // |
  TokenKind_Star,        // *
  TokenKind_Slash,       // /
  TokenKind_Percent,     // %
  TokenKind_Exclamation, // !
  TokenKind_Question,    // ?
  TokenKind_Equal,       // =
  TokenKind_Dot,         // .
  TokenKind_Comma,       // ,
  TokenKind_Semicolon,   // ;
  TokenKind_Colon,       // :
  TokenKind_Tilde,       // ~
  TokenKind_SymbolEnd,

  TokenKind_Ident,
  TokenKind_Arrow,
  TokenKind_Deref,
  TokenKind_CmpGt,
  TokenKind_CmpLt,
  TokenKind_CmpEq,
  TokenKind_CmpNeq,
  TokenKind_CmpGtEq,
  TokenKind_CmpLtEq,
  TokenKind_LogicalOr,
  TokenKind_LogicalAnd,
  TokenKind_LShift,
  TokenKind_RShift,
  TokenKind_LShiftAssign,
  TokenKind_RShiftAssign,
  TokenKind_AddAssign,
  TokenKind_SubAssign,
  TokenKind_DivAssign,
  TokenKind_MulAssign,
  TokenKind_AndAssign,
  TokenKind_OrAssign,
  TokenKind_XorAssign,
  TokenKind_Increment,
  TokenKind_Decrement,
  TokenKind_RangeIncl,
  TokenKind_RangeExcl,
  TokenKind_StringLiteral,
  TokenKind_IntegerLiteral,
  TokenKind_FloatLiteral,
  TokenKind_CharLiteral,

  TokenKind_Nil,
  TokenKind_KeywordBegin = TokenKind_Nil,
  TokenKind_I8,
  TokenKind_I16,
  TokenKind_I32,
  TokenKind_I64,
  TokenKind_U8,
  TokenKind_U16,
  TokenKind_U32,
  TokenKind_U64,
  TokenKind_Uintptr,
  TokenKind_Int,
  TokenKind_Uint,
  TokenKind_F32,
  TokenKind_F64,
  TokenKind_Bool,
  TokenKind_String,
  TokenKind_True,
  TokenKind_False,
  TokenKind_If,
  TokenKind_Else,
  TokenKind_For,
  TokenKind_Do,
  TokenKind_While,
  TokenKind_Switch,
  TokenKind_Case,
  TokenKind_Defer,
  TokenKind_Break,
  TokenKind_Fallthrough,
  TokenKind_Continue,
  TokenKind_Return,
  TokenKind_Struct,
  TokenKind_Union,
  TokenKind_Enum,
  TokenKind_Proc,
  TokenKind_SizeOf,
  TokenKind_Cast,
  TokenKind_Transmute,
  TokenKind_In,
  TokenKind_Then,
  TokenKind_Foreign,
  TokenKind_Keyword_End,

  TokenKind_COUNT,
} TokenKind;

union LiteralValue {
  String8 string;
  u64     integer;
  f64     floating;
  bool    boolean;
  char    character;
};

struct SourcePos {
  u64 row; // x
  u64 col; // y
  u64 length; // token length for error highlighting
};

struct Token {
  TokenKind    kind;
  SourcePos    pos;
  String8      lexeme;
  LiteralValue value;
};

struct Lexer {
  Arena *arena;
  String8 source;
  u64 start;
  u64 cursor;
  u64 line;
  u64 bol; // beginning of line
  b32 insert_semicolon;
};

function Lexer lexer_init(String8 source);
function void  lexer_fini(Lexer *l);
function Token lexer_next(Lexer *l);

function String8 str_from_TokenKind(Arena *arena, TokenKind kind);
