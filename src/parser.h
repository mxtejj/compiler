#pragma once

#include "arena.h"
#include "lexer.h"
#include "base.h"
#include "string.h"

// TODO:
// [ ] use intrusive singly-linked list for AST nodes (stmt, expr, decl)

////////////////////////////////
// Parser
typedef struct Parser Parser;
struct Parser
{
  Arena *arena;
  Lexer *lexer;
  Token prev;
  Token curr;
  Token next;
};

internal Parser parser_init(Lexer *l);

////////////////////////////////
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
  STMT_DO_WHILE,
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
typedef struct Stmt_List Stmt_List;
typedef struct Decl Decl;

struct Stmt_List
{
  Stmt *first;
  Stmt *last;
  u64 count;
};

struct Stmt
{
  Stmt_Kind kind;

  Stmt *next;

  union
  {
    struct
    {
      Stmt_List stmts;
    }
    block;

    struct
    {
      Expr *cond;
      Stmt *then_block;
      Stmt *else_stmt; // NULL, STMT_IF or STMT_BLOCK
    }
    if0;

    struct
    {
      Expr *cond;
      Stmt *body;
    }
    do_while;

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
      Expr *expr;
    }
    return0;

    Expr *expr;
    Decl *decl;
  };
};
// raddbg_type_view(Stmt,
//                  kind == STMT_BLOCK  ? block :
//                  kind == STMT_IF     ? if0 :
//                  kind == STMT_DO_WHILE ? do_while :
//                  kind == STMT_WHILE  ? while0 :
//                  kind == STMT_FOR    ? for0 :
//                  kind == STMT_RETURN ? return0 :
//                  kind == STMT_EXPR   ? expr :
//                  kind == STMT_DECL   ? decl :
//                  $);

internal Stmt *stmt_alloc(Parser *p, Stmt_Kind kind);
internal Stmt *stmt_expr(Parser *p, Expr *e);

///////////////////////////////////
// TYPE SPECIFIERS
typedef enum Type_Spec_Kind Type_Spec_Kind;
enum Type_Spec_Kind
{
  TYPE_SPEC_NULL,
  TYPE_SPEC_NAME,  // en: Entity
  TYPE_SPEC_PROC,  // update: proc(Entity)
  TYPE_SPEC_ARRAY, // entity_pair: [2]Entity
  TYPE_SPEC_SLICE, // entities: []Entity
  TYPE_SPEC_PTR,   // en: *Entity
};

typedef struct Type_Spec Type_Spec;

typedef struct Type_Spec_List Type_Spec_List;
struct Type_Spec_List
{
  Type_Spec *first;
  Type_Spec *last;
};

struct Type_Spec
{
  Type_Spec_Kind kind;

  Type_Spec *next;
  Type_Spec *prev; // TODO

  union
  {
    String8 name;

    struct
    {
      Type_Spec_List params; // TODO: replace with array?
      u32 param_count;
      Type_Spec *ret;
    }
    proc;

    struct
    {
      Expr      *count;
      Type_Spec *elem; // TODO: rename to base
    }
    array;

    struct
    {
      Type_Spec *elem;
    }
    slice;

    struct
    {
      Type_Spec *pointee;
    }
    ptr;
  };
};

internal Type_Spec *type_spec_alloc(Parser *p, Type_Spec_Kind kind);

///////////////////////////////////
// DECLARATIONS

typedef enum Decl_Kind Decl_Kind;
enum Decl_Kind
{
  DECL_NULL,
  DECL_PROC,
  DECL_STRUCT,
  DECL_UNION,
  DECL_ENUM,
  DECL_VAR,
  DECL_CONST,
};

typedef struct Proc_Param Proc_Param;
struct Proc_Param
{
  Proc_Param *next;
  Proc_Param *prev;

  String8    name;
  Type_Spec *type;
};

typedef struct Param_List Param_List;
struct Param_List
{
  Proc_Param *first;
  Proc_Param *last;
};

typedef struct Decl_Proc Decl_Proc;
struct Decl_Proc
{
  Param_List  params;
  Type_Spec  *ret;
  Stmt       *body;
};

typedef struct Decl_Aggr Decl_Aggr;
struct Decl_Aggr
{
  int todo;
};

typedef struct Enum_Member Enum_Member;
struct Enum_Member
{
  Enum_Member *next;
  String8      name;
  Expr        *value;
};

typedef struct Enum_Member_List Enum_Member_List;
struct Enum_Member_List
{
  Enum_Member *first;
  Enum_Member *last;
};

typedef struct Decl_Enum Decl_Enum;
struct Decl_Enum
{
  Enum_Member_List members;
};

typedef struct Decl_Var Decl_Var;
struct Decl_Var
{
  Type_Spec *type; // TODO rename to typespec
  Expr      *expr;
};

typedef struct Decl_Const Decl_Const;
struct Decl_Const
{
  Expr *expr;
};

struct Decl
{
  Decl_Kind kind;
  String8   name;

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
// raddbg_type_view(Decl,
//                  kind == DECL_PROC   ? proc :
//                  kind == DECL_STRUCT ? struct0 :
//                  kind == DECL_UNION  ? union0 :
//                  kind == DECL_ENUM   ? enum0 :
//                  kind == DECL_VAR    ? var :
//                  kind == DECL_CONST  ? const0 :
//                  $);

typedef struct Decl_List Decl_List;
struct Decl_List
{
  Decl *first;
  Decl *last;
};

Decl *decl_alloc(Parser *p, Decl_Kind kind);

///////////////////////////////////
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
  EXPR_CAST,         // unary
  EXPR_CALL,         // postfix
  EXPR_INDEX,        // postfix
  EXPR_FIELD,        // postfix
  EXPR_COMPOUND,     // primary/postfix hybrid | Type{}, [5]Type{...}, proc(a: int) -> float {}, &something
  EXPR_SIZE_OF_EXPR, // unary
  EXPR_SIZE_OF_TYPE, // unary
};

STRUCT(Compound_Arg)
{
  Compound_Arg *next;

  String8 optional_name;
  Expr *expr;
};

STRUCT(Compound_Arg_List)
{
  Compound_Arg *first;
  Compound_Arg *last;
  u64 count;
};

internal void push_compound_arg(Compound_Arg_List *list, Compound_Arg *arg);

STRUCT(Expr_List)
{
  Expr *first;
  Expr *last;
};

struct Expr
{
  Expr_Kind kind;

  Expr *next;

  union
  {
    String8 ident;

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

    struct
    {
      Type_Spec *type;
      Expr *expr;
    }
    cast;

    struct
    {
      Expr *expr;
      Expr_List args;
    }
    call;

    struct
    {
      Expr *expr;
      Expr *index;
    }
    index;

    struct
    {
      Expr *expr;
      String8 name;
    }
    field;

    struct
    {
      Type_Spec *type;
      Compound_Arg_List args;
    }
    compound;

    Expr      *size_of_expr;
    Type_Spec *size_of_type;
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

internal Expr *expr_alloc(Parser *p, Expr_Kind kind);

internal Expr *expr_ident(Parser *p, Token ident);
internal Expr *expr_unary(Parser *p, Token op, Expr *right);
internal Expr *expr_binary(Parser *p, Expr *left, Token op, Expr *right);
internal Expr *expr_ternary(Parser *p, Expr *cond, Expr *then, Expr *else_);
internal Expr *expr_nil_lit(Parser *p);
internal Expr *expr_string_lit(Parser *p, String8 s);
internal Expr *expr_integer_lit(Parser *p, u64 n);
internal Expr *expr_float_lit(Parser *p, f64 f);
internal Expr *expr_bool_lit(Parser *p, bool b);
internal Expr *expr_group(Parser *p, Expr *e);
internal Expr *expr_cast(Parser *p, Type_Spec *type, Expr *e);
internal Expr *expr_call(Parser *p, Expr *e, Expr_List args);
internal Expr *expr_index(Parser *p, Expr *e, Expr *index);
internal Expr *expr_field(Parser *p, Expr *e, String8 field);
internal Expr *expr_compound(Parser *p, Type_Spec *type, Compound_Arg_List args);
internal Expr *expr_size_of_expr(Parser *p, Expr *e);
internal Expr *expr_size_of_type(Parser *p, Type_Spec *type);
