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

// typedef enum AST_Kind AST_Kind;
// enum AST_Kind
// {
//   AST_NULL,
//   AST_DECL,
//   AST_STMT,
//   AST_EXPR,
//   AST_STRUCTURED_TYPE, // array, pointer, struct, union, fn
// };

typedef enum Stmt_Kind Stmt_Kind;
enum Stmt_Kind
{
  STMT_NULL,
  STMT_BLOCK,
  STMT_IF,
  STMT_WHILE,
  STMT_FOR,
  STMT_SWITCH,
  STMT_RETURN,
  STMT_BREAK,
  STMT_CONTINUE,
  STMT_EXPR,
  STMT_DECL,
  // call, assign
};

typedef struct Stmt Stmt;
typedef struct Expr Expr;

struct Stmt
{
  Stmt_Kind kind;

  Stmt *next;
  Stmt *prev;

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
// raddbg_type_view(Stmt,
//                  kind == STMT_IF     ? if0 :
//                  kind == STMT_WHILE  ? while0 :
//                  kind == STMT_FOR    ? for0 :
//                  kind == STMT_RETURN ? return0 :
//                  kind == STMT_EXPR   ? expr :
//                  $);

typedef struct Stmt_List Stmt_List;
struct Stmt_List
{
  Stmt *first;
  Stmt *last;
};

Stmt *stmt_alloc(Parser *p, Stmt_Kind kind);
Stmt *stmt_expr(Parser *p, Expr *e);

/////////////////////////////////////////////////////
// TYPE SPECIFIERS
typedef enum Type_Spec_Kind Type_Spec_Kind;
enum Type_Spec_Kind
{
  TYPE_SPEC_NULL,
  TYPE_SPEC_NAME,
  TYPE_SPEC_FUNC,
  TYPE_SPEC_ARRAY,
  TYPE_SPEC_PTR,
};

typedef struct Type_Spec Type_Spec;
struct Type_Spec
{
  Type_Spec_Kind kind;
};

/////////////////////////////////////////////////////
// DECLARATIONS

typedef enum Decl_Kind Decl_Kind;
enum Decl_Kind
{
  DECL_NULL,
  DECL_PROC,
  DECL_AGGR, // union or struct
  DECL_ENUM,
  DECL_VAR,
  DECL_CONST,
};

typedef struct Decl_Proc Decl_Proc;
struct Decl_Proc
{
  String name;
  // Proc_Params
  Type_Spec *ret;
};

typedef struct Decl_Aggr Decl_Aggr;
struct Decl_Aggr
{
  String name;
};

typedef struct Decl_Enum Decl_Enum;
struct Decl_Enum
{
  String name;
};

typedef struct Decl_Var Decl_Var;
struct Decl_Var
{
  String name;
};

typedef struct Decl_Const Decl_Const;
struct Decl_Const
{
  String name;
};

typedef struct Decl Decl;
struct Decl
{
  Decl_Kind kind;

  Decl *next;
  Decl *prev;

  union
  {
    Decl_Proc  proc;
    Decl_Aggr  aggr;
    Decl_Enum  enum0;
    Decl_Var   var;
    Decl_Const const0;
  };
};

typedef struct Decl_List Decl_List;
struct Decl_List
{
  Decl *first;
  Decl *last;
};

Decl *decl_alloc(Parser *p, Decl_Kind kind);

/////////////////////////////////////////////////////
// EXPRESSIONS

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
// raddbg_type_view(Expr,
//                  kind == EXPR_BINARY          ? binary :
//                  $);
// raddbg_type_view(Expr,
//                  kind == Expr_Kind.EXPR_IDENT           ? ident :
//                  kind == Expr_Kind.EXPR_UNARY           ? unary :
//                  kind == Expr_Kind.EXPR_BINARY          ? binary :
//                  kind == Expr_Kind.EXPR_TERNARY         ? ternary :
//                  kind == Expr_Kind.EXPR_NIL_LITERAL     ? "nil" :
//                  kind == Expr_Kind.EXPR_STRING_LITERAL  ? literal.string :
//                  kind == Expr_Kind.EXPR_INTEGER_LITERAL ? literal.integer :
//                  kind == Expr_Kind.EXPR_FLOAT_LITERAL   ? literal.floating :
//                  kind == Expr_Kind.EXPR_BOOL_LITERAL    ? literal.boolean :
//                  kind == Expr_Kind.EXPR_GROUP           ? group :
//                  $);

typedef struct Expr_List Expr_List;
struct Expr_List
{
  Expr *first;
  Expr *last;
};

Expr *expr_alloc(Parser *p, Expr_Kind kind);

Expr *expr_ident(Parser *p, Token ident);
Expr *expr_unary(Parser *p, Token op, Expr *right);
Expr *expr_binary(Parser *p, Expr *left, Token op, Expr *right);
Expr *expr_ternary(Parser *p, Expr *cond, Expr *then, Expr *else_);
Expr *expr_nil_lit(Parser *p);
Expr *expr_string_lit(Parser *p, String s);
Expr *expr_integer_lit(Parser *p, u64 n);
Expr *expr_float_lit(Parser *p, f64 f);
Expr *expr_bool_lit(Parser *p, bool b);
Expr *expr_group(Parser *p, Expr *e);
