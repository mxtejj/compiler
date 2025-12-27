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

typedef enum Stmt_Kind Stmt_Kind;
enum Stmt_Kind
{
  STMT_KIND_NULL,
  STMT_KIND_BLOCK,
  STMT_KIND_IF,
  STMT_KIND_WHILE,
  STMT_KIND_FOR,
  STMT_KIND_SWITCH,
  STMT_KIND_RETURN,
  STMT_KIND_BREAK,
  STMT_KIND_CONTINUE,
  STMT_KIND_EXPR,
  STMT_KIND_DECL,
};

typedef struct Stmt Stmt;
typedef struct Expr Expr;
// typedef struct Stmt_Node Stmt_Node;
// struct Stmt_Node
// {
//   Stmt stmt;
//   Stmt_Node *next;
//   Stmt_Node *prev;
// };

struct Stmt
{
  Stmt_Kind kind;
  union
  {
    // struct
    // {
    //   // List of N statements
    //   // Stmt_Node node;
    // }
    // block;

    struct
    {
      Expr *cond;
      Stmt *then_stmt;
      Stmt *else_stmt;
      Stmt *body; // BLOCK
    }
    if0;

    struct
    {
      Expr *cond;
      Stmt *body;
    }
    while0;

    struct
    {
      Stmt *init;
      Expr *cond;
      Expr *loop;
      Stmt *body;
    }
    for0;

    struct
    {
      Expr *value;
    }
    return0;

    Expr *expr;
    // TODO: decl
  };
};
raddbg_type_view(Stmt,
                 kind == Stmt_Kind.IF     ? if0 :
                 kind == Stmt_Kind.WHILE  ? while0 :
                 kind == Stmt_Kind.FOR    ? for0 :
                 kind == Stmt_Kind.RETURN ? return0 :
                 kind == Stmt_Kind.EXPR   ? expr :
                 $);

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

    Stmt stmt;
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

typedef struct AST_List AST_List;
struct AST_List
{
  AST *first;
  AST *last;
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

AST *stmt_alloc(Parser *p, Stmt_Kind kind);
AST *stmt_expr(Parser *p, Expr *e);
