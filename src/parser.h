#pragma once

#include "arena.h"
#include "lexer.h"
#include "base.h"
#include "string.h"

// TODO:
// [ ] use intrusive singly-linked list for AST nodes (stmt, expr, decl)
// [ ] in some cases where we use a linked-list at first and then copy to an array
// we could introduce a node wrapper to not pollude the struct with link pointers

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
  STMT_FOR_IN,
  STMT_SWITCH,
  STMT_RETURN,
  STMT_BREAK,
  STMT_CONTINUE,
  STMT_EXPR,
  STMT_DECL,
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

STRUCT(Stmt_Array)
{
  Stmt **v;
  u64 count;
};

STRUCT(Expr_Array)
{
  Expr **v;
  u64 count;
};

STRUCT(Switch_Case)
{
  Expr_Array labels;
  Stmt *block; // NOTE: because we have print_stmt which takes a Stmt * we "cant" make this Stmt_Array
  b8 is_fallthrough;
  b8 is_default; // => (labels.count == 0)
};

STRUCT(Switch_Case_Node)
{
  Switch_Case_Node *next;
  Switch_Case v;
};

STRUCT(Switch_Case_List)
{
  Switch_Case_Node *first;
  Switch_Case_Node *last;
  u64 count;
};

STRUCT(Switch_Case_Array)
{
  Switch_Case *v;
  u64 count;
};

struct Stmt
{
  Stmt_Kind kind;

  Stmt *next;

  union
  {
    Stmt_Array block;

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
    while0;

    struct
    {
      Stmt *init;
      Expr *cond;
      Stmt *loop;
      Stmt *body;
    }
    for0;

    struct
    {
      Expr *item;
      Expr *iter;
      Stmt *body;
    }
    for_in;

    struct
    {
      Expr *expr;
      Switch_Case_Array cases;
    }
    switch0;

    Expr *return_expr;
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

internal Stmt *stmt_block(Parser *p, Stmt_Array stmts);
internal Stmt *stmt_if(Parser *p, Expr *cond, Stmt *then_block, Stmt *else_stmt);
internal Stmt *stmt_while(Parser *p, Expr *cond, Stmt *body);
internal Stmt *stmt_for(Parser *p, Stmt *init, Expr *cond, Stmt *loop, Stmt *body);
internal Stmt *stmt_for_in(Parser *p, Expr *item, Expr *iter, Stmt *body);
internal Stmt *stmt_return(Parser *p, Expr *expr);
internal Stmt *stmt_expr(Parser *p, Expr *expr);
internal Stmt *stmt_decl(Parser *p, Decl *decl);
internal Stmt *stmt_switch(Parser *p, Expr *expr, Switch_Case_Array cases);

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

STRUCT(Type_Spec_Array)
{
  Type_Spec **v;
  u64 count;
};

struct Type_Spec
{
  Type_Spec_Kind kind;

  Type_Spec *next;

  union
  {
    String8 name;

    struct
    {
      Type_Spec_Array params;
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
  DECL_TYPEDEF,
};

STRUCT(Proc_Param)
{
  String8    name;
  Type_Spec *type;
};

STRUCT(Proc_Param_Node)
{
  Proc_Param_Node *next;
  Proc_Param v;
};

typedef struct Param_List Param_List;
struct Param_List
{
  Proc_Param_Node *first;
  Proc_Param_Node *last;
};

typedef struct Param_Array Param_Array;
struct Param_Array
{
  Proc_Param *v;
  u64 count;
};

typedef struct Decl_Proc Decl_Proc;
struct Decl_Proc
{
  Param_Array params;
  Type_Spec  *ret;
  Stmt       *body;
};

STRUCT(Aggr_Field)
{
  String8List names;
  Type_Spec  *type;
};

STRUCT(Aggr_Field_Node)
{
  Aggr_Field_Node *next;
  Aggr_Field v;
};

STRUCT(Aggr_Field_List)
{
  Aggr_Field_Node *first;
  Aggr_Field_Node *last;
  u64 count;
};

STRUCT(Aggr_Field_Array)
{
  Aggr_Field *v;
  u64 count;
};

typedef struct Decl_Aggr Decl_Aggr;
struct Decl_Aggr
{
  Aggr_Field_Array fields;
};

typedef struct Enum_Member Enum_Member;
struct Enum_Member
{
  String8  name;
  Expr    *value;
};

STRUCT(Enum_Member_Node)
{
  Enum_Member_Node *next;
  Enum_Member v;
};

typedef struct Enum_Member_List Enum_Member_List;
struct Enum_Member_List
{
  Enum_Member_Node *first;
  Enum_Member_Node *last;
};

STRUCT(Enum_Member_Array)
{
  Enum_Member *v;
  u64 count;
};

typedef struct Decl_Enum Decl_Enum;
struct Decl_Enum
{
  Enum_Member_Array members;
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

STRUCT(Decl_Typedef)
{
  Type_Spec *type;
};

struct Decl
{
  Decl_Kind kind;
  String8   name;

  Decl *next;

  union
  {
    Decl_Proc  proc;
    Decl_Aggr  aggr;
    Decl_Enum  enum0;
    Decl_Var   var;
    Decl_Const const0;
    Decl_Typedef typedef0;
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

internal Decl *decl_alloc(Parser *p, String8 name, Decl_Kind kind);

internal Decl *decl_proc(Parser *p, String8 name, Param_Array params, Type_Spec *ret, Stmt *body);
internal Decl *decl_aggregate(Parser *p, String8 name, Decl_Kind kind, Aggr_Field_Array fields); // TODO: Field_Array
internal Decl *decl_enum(Parser *p, String8 name, Enum_Member_Array members);
internal Decl *decl_var(Parser *p, String8 name, Type_Spec *type, Expr *expr);
internal Decl *decl_const(Parser *p, String8 name, Expr *expr);
internal Decl *decl_typedef(Parser *p, String8 name, Type_Spec *type);

// TODO order this properly
internal Decl *parse_decl(Parser *p);
internal Decl_List parse_declarations(Parser *p);

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
  EXPR_CHAR_LITERAL,
  EXPR_GROUP,
  EXPR_CAST,         // unary
  EXPR_CALL,         // postfix
  EXPR_INDEX,        // postfix
  EXPR_FIELD,        // postfix
  EXPR_COMPOUND,     // primary/postfix hybrid | Type{}, [5]Type{...}, proc(a: int) -> float {}, &something
  EXPR_SIZE_OF_EXPR, // unary
  EXPR_SIZE_OF_TYPE, // unary
};

// TODO: rename to compound_field, add compound_field_node for linked list
ENUM(Compound_Field_Kind)
{
  COMPOUND_FIELD_NONE,
  COMPOUND_FIELD_NAME,
  COMPOUND_FIELD_INDEX,
};

STRUCT(Compound_Field)
{
  Compound_Field *next;

  Compound_Field_Kind kind;
  Expr *init;

  union
  {
    String8 name;
    Expr *index;
  };
};

STRUCT(Compound_Field_List)
{
  Compound_Field *first;
  Compound_Field *last;
  u64 count;
};

STRUCT(Compound_Field_Array)
{
  Compound_Field **v;
  u64 count;
};

internal void push_compound_field(Compound_Field_List *list, Compound_Field *field);

STRUCT(Expr_List)
{
  Expr *first;
  Expr *last;
  u64 count;
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
      Expr_Array args;
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
      // Compound_Field_List args;
      Compound_Field_Array args;
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
internal Expr *expr_char_lit(Parser *p, char c);
internal Expr *expr_group(Parser *p, Expr *e);
internal Expr *expr_cast(Parser *p, Type_Spec *type, Expr *e);
internal Expr *expr_call(Parser *p, Expr *e, Expr_Array args);
internal Expr *expr_index(Parser *p, Expr *e, Expr *index);
internal Expr *expr_field(Parser *p, Expr *e, String8 field);
internal Expr *expr_compound(Parser *p, Type_Spec *type, Compound_Field_Array args);
internal Expr *expr_size_of_expr(Parser *p, Expr *e);
internal Expr *expr_size_of_type(Parser *p, Type_Spec *type);
