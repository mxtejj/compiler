#pragma once

#include "arena.h"
#include "lexer.h"
#include "base.h"
#include "string.h"

//////////////////////////////////////////////////
// Parser
typedef struct Parser Parser;
struct Parser
{
  Arena *arena;
  Lexer *lexer;
  Token curr;
  Token prev;
};

Parser parser_init(Lexer *l);

//////////////////////////////////////////////////
// AST

typedef enum AST_Kind AST_Kind;
enum AST_Kind
{
  AST_NULL,
  AST_DECL,
  AST_STMT,
  AST_EXPR,
  AST_STRUCTURED_TYPE, // array, pointer, struct, union, fn
};

typedef enum AST_Stmt_Kind AST_Stmt_Kind;
enum AST_Stmt_Kind
{
  AST_STMT_NULL,
  AST_STMT_BLOCK,
  AST_STMT_IF,
  AST_STMT_WHILE,
  AST_STMT_FOR,
  AST_STMT_SWITCH,
  AST_STMT_RETURN,
  AST_STMT_BREAK,
  AST_STMT_CONTINUE,
  AST_STMT_EXPR,
  AST_STMT_DECL,
};

typedef struct AST_Expr AST_Expr;

typedef struct AST_Stmt AST_Stmt;
struct AST_Stmt
{
  AST_Stmt_Kind kind;
  union
  {
    // struct
    // {
    //   // list of N statements
    // }
    // block;

    /*
    if (something)
    {}
    else
    {}
    */

    struct
    {
      AST_Expr *cond;
      AST_Stmt *then_stmt;
      AST_Stmt *else_stmt;
      AST_Stmt *body; // BLOCK
    }
    _if;
  };
};

typedef enum Decl_Kind Decl_Kind;
enum Decl_Kind
{
  DECL_NULL,
  DECL_FN,
  DECL_AGGR,
  DECL_ENUM,
  DECL_VAR,
  DECL_CONST,
};

typedef enum Expr_Kind Expr_Kind;
enum Expr_Kind
{
  EXPR_NULL,
  EXPR_IDENT,
  EXPR_UNARY,
  EXPR_BINARY,
  EXPR_TERNARY,
  EXPR_NIL_LITERAL,
  EXPR_STRING_LITERAL,
  EXPR_INTEGER_LITERAL,
  EXPR_FLOAT_LITERAL,
  EXPR_BOOL_LITERAL,
  EXPR_GROUP,
};

typedef struct Expr Expr;
struct Expr
{
  Expr_Kind kind;

  union
  {
    String ident;

    struct
    {
      Token op;
      Expr *right;
    }
    unary;

    struct
    {
      Expr *left;
      Token op;
      Expr *right;
    }
    binary;

    struct
    {
      Expr *cond;
      Expr *then;
      Expr *else_;
    }
    ternary;

    Literal_Value literal;

    struct
    {
      Expr *expr;
    }
    group;
  };
};

// AST NODE
typedef struct AST AST;
struct AST
{
  AST_Kind kind;

  AST *next;
  AST *prev;

  u64 line;
  u64 column;

  union
  {
    struct
    {
      String name;
      AST *type;
      AST *value;
      AST *params;
    }
    decl;

    struct
    {
      AST *cond;
      AST *then_stmt;
      AST *else_stmt;
      AST *body;
    }
    stmt;

    Expr expr;

    struct
    {
      AST *base;
      u32 flags; // pointer, array, etc.
      usize array_count;
    }
    type;
  };
};

AST *ast_alloc(Parser *p);

AST *expr_ident(Parser *p, Token ident);
AST *expr_unary(Parser *p, Token op, Expr *right);
AST *expr_binary(Parser *p, Expr *left, Token op, Expr *right);
AST *expr_ternary(Parser *p, Expr *cond, Expr *then, Expr *else_);
AST *expr_nil_lit(Parser *p);
AST *expr_string_lit(Parser *p, String s);
AST *expr_integer_lit(Parser *p, u64 n);
AST *expr_float_lit(Parser *p, f64 f);
AST *expr_bool_lit(Parser *p, bool b);
AST *expr_group(Parser *p, Expr *e);
