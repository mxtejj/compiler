#pragma once

#include "arena.h"
#include "base.h"
#include "lexer.h"
#include "string.h"

typedef struct Parser Parser;
typedef struct Stmt Stmt;
typedef struct Expr Expr;
typedef struct StmtList StmtList;
typedef struct Decl Decl;
typedef struct StmtArray StmtArray;
typedef struct ExprArray ExprArray;
typedef struct SwitchCase SwitchCase;
typedef struct SwitchCaseNode SwitchCaseNode;
typedef struct SwitchCaseList SwitchCaseList;
typedef struct SwitchCaseArray SwitchCaseArray;
typedef struct TypeSpec TypeSpec;
typedef struct TypeSpecList TypeSpecList;
typedef struct TypeSpecArray TypeSpecArray;
typedef struct EnumMember EnumMember;
typedef struct EnumMemberArray EnumMemberArray;
typedef struct AggrField AggrField;
typedef struct AggrFieldArray AggrFieldArray;
typedef struct DeclArray DeclArray;
typedef struct ProcParam ProcParam;
typedef struct ProcParamNode ProcParamNode;
typedef struct ParamList ParamList;
typedef struct ParamArray ParamArray;
typedef struct DeclProc DeclProc;
typedef struct AggrField AggrField;
typedef struct AggrFieldNode AggrFieldNode;
typedef struct AggrFieldList AggrFieldList;
typedef struct DeclAggr DeclAggr;
typedef struct EnumMemberNode EnumMemberNode;
typedef struct EnumMemberList EnumMemberList;
typedef struct DeclEnum DeclEnum;
typedef struct DeclVar DeclVar;
typedef struct DeclConst DeclConst;
typedef struct DeclTypedef DeclTypedef;
typedef struct DeclList DeclList;
typedef struct CompoundField CompoundField;
typedef struct CompoundFieldList CompoundFieldList;
typedef struct CompoundFieldArray CompoundFieldArray;
typedef struct ExprList ExprList;

////////////////////////////////
// Parser
struct Parser {
  Arena *arena;
  Lexer *lexer;
  Token prev;
  Token curr;
  Token next;
};

function Parser parser_init(Lexer *l);

////////////////////////////////
// AST

// from resolver
struct Type;

typedef enum StmtKind {
  StmtKind_Null,
  StmtKind_Block,
  StmtKind_If,
  StmtKind_DoWhile,
  StmtKind_While,
  StmtKind_For,
  StmtKind_ForIn,
  StmtKind_Switch,
  StmtKind_Return,
  StmtKind_Defer,
  StmtKind_Break,
  StmtKind_Continue,
  StmtKind_Expr,
  StmtKind_Decl,
} StmtKind;

struct StmtList {
  Stmt *first;
  Stmt *last;
  u64 count;
};

struct StmtArray {
  Stmt **v;
  u64 count;
};

struct ExprArray {
  Expr **v;
  u64 count;
};

struct SwitchCase {
  ExprArray labels;
  Stmt *block; // NOTE: because we have print_stmt which takes a Stmt * we
               // "cant" make this StmtArray
  b8 is_fallthrough;
  b8 is_default; // => (labels.count == 0)
};

struct SwitchCaseNode {
  SwitchCaseNode *next;
  SwitchCase v;
};

struct SwitchCaseList {
  SwitchCaseNode *first;
  SwitchCaseNode *last;
  u64 count;
};

struct SwitchCaseArray {
  SwitchCase *v;
  u64 count;
};

struct Stmt {
  StmtKind kind;
  SourcePos pos;

  Stmt *next;

  union {
    StmtArray block;

    struct {
      Expr *cond;
      Stmt *then_block;
      Stmt *else_stmt; // NULL, STMT_IF or STMT_BLOCK
    } if0;

    struct {
      Expr *cond;
      Stmt *body;
    } while0;

    struct {
      Stmt *init;
      Expr *cond;
      Stmt *loop;
      Stmt *body;
    } for0;

    struct {
      Expr *item;
      Expr *iter;
      Stmt *body;
    } for_in;

    struct {
      Expr *expr;
      SwitchCaseArray cases;
    } switch0;

    Expr *return_expr;
    Stmt *defer_stmt;
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

function Stmt *stmt_alloc(Parser *p, StmtKind kind);

function Stmt *stmt_block(Parser *p, StmtArray stmts);
function Stmt *stmt_if(Parser *p, Expr *cond, Stmt *then_block,
                       Stmt *else_stmt);
function Stmt *stmt_while(Parser *p, Expr *cond, Stmt *body);
function Stmt *stmt_do_while(Parser *p, Expr *cond, Stmt *body);
function Stmt *stmt_for(Parser *p, Stmt *init, Expr *cond, Stmt *loop,
                        Stmt *body);
function Stmt *stmt_for_in(Parser *p, Expr *item, Expr *iter, Stmt *body);
function Stmt *stmt_return(Parser *p, Expr *expr);
function Stmt *stmt_defer(Parser *p, Stmt *stmt);
function Stmt *stmt_expr(Parser *p, Expr *expr);
function Stmt *stmt_decl(Parser *p, Decl *decl);
function Stmt *stmt_switch(Parser *p, Expr *expr, SwitchCaseArray cases);

///////////////////////////////////
// TYPE SPECIFIERS
typedef enum TypeSpecKind {
  TypeSpecKind_Null,
  TypeSpecKind_Name,   // Entity
  TypeSpecKind_Proc,   // proc(...) -> ret
  TypeSpecKind_Enum,   // enum { ... }
  TypeSpecKind_Struct, // struct { ... }
  TypeSpecKind_Union,  // union { ... }
  TypeSpecKind_Array,  // [2]Entity
  TypeSpecKind_Slice,  // []Entity
  TypeSpecKind_Ptr,    // *Entity
} TypeSpecKind;

struct TypeSpecList {
  TypeSpec *first;
  TypeSpec *last;
};

struct TypeSpecArray {
  TypeSpec **v;
  u64 count;
};

struct EnumMember {
  String8 name;
  Expr *value;
};

struct EnumMemberArray {
  EnumMember *v;
  u64 count;
};

struct AggrFieldArray {
  AggrField *v;
  u64 count;
};

struct DeclArray {
  Decl **v;
  u64 count;
};

struct TypeSpec {
  TypeSpecKind kind;
  SourcePos pos;
  TypeSpec *next;

  union {
    String8 name;

    struct {
      DeclArray params;
      TypeSpec *ret;
      Stmt *body;
    } proc;

    EnumMemberArray enum_members;
    AggrFieldArray aggr_fields;

    struct {
      Expr *count;
      TypeSpec *elem; // TODO: rename to base
    } array;

    struct {
      TypeSpec *elem;
    } slice;

    struct {
      TypeSpec *pointee;
    } ptr;
  };
};

function TypeSpec *type_spec_alloc(Parser *p, TypeSpecKind kind);
function TypeSpec *type_spec_name(Parser *p, String8 name);
function TypeSpec *type_spec_proc(Parser *p, DeclArray params, TypeSpec *ret, Stmt *body);
function TypeSpec *type_spec_enum(Parser *p, EnumMemberArray members);
function TypeSpec *type_spec_aggr(Parser *p, TypeSpecKind kind, AggrFieldArray fields);
function TypeSpec *type_spec_array(Parser *p, Expr *count, TypeSpec *elem);
function TypeSpec *type_spec_slice(Parser *p, TypeSpec *elem);
function TypeSpec *type_spec_ptr(Parser *p, TypeSpec *pointee);

///////////////////////////////////
// DECLARATIONS

typedef enum DeclKind {
  DeclKind_Null,
  DeclKind_Var,
  DeclKind_Const,
} DeclKind;

struct ProcParam {
  String8 name;
  TypeSpec *type;
};

struct ProcParamNode {
  ProcParamNode *next;
  ProcParam v;
};

struct ParamList {
  ProcParamNode *first;
  ProcParamNode *last;
};

struct ParamArray {
  ProcParam *v;
  u64 count;
};

struct DeclProc {
  ParamArray params;
  TypeSpec *ret;
  Stmt *body;
};

struct AggrField {
  String8List names;
  TypeSpec *type;
};

struct AggrFieldNode {
  AggrFieldNode *next;
  AggrField v;
};

struct AggrFieldList {
  AggrFieldNode *first;
  AggrFieldNode *last;
  u64 count;
};

struct DeclAggr {
  AggrFieldArray fields;
};

struct EnumMemberNode {
  EnumMemberNode *next;
  EnumMember v;
};

struct EnumMemberList {
  EnumMemberNode *first;
  EnumMemberNode *last;
};

struct DeclEnum {
  EnumMemberArray members;
};

struct DeclVar {
  TypeSpec *type; // TODO rename to typespec
  Expr *expr;
};

struct DeclConst {
  TypeSpec *type;
  Expr *expr;
};

struct DeclTypedef {
  TypeSpec *type;
};

struct Decl {
  DeclKind kind;
  SourcePos pos;
  struct Sym *sym;
  String8 name;
  TypeSpec *type_hint; // The 'T' in 'x: T = ...'
  b32 is_foreign;

  // payload
  Expr *init_expr;      // 'N :: 5'
  TypeSpec *init_type; // 'Color :: enum { ... }'

  Decl *next;

  // union
  // {
  //   DeclProc  proc;
  //   DeclAggr  aggr;
  //   DeclEnum  enum0;
  //   DeclVar   var;
  //   DeclConst const0;
  //   DeclTypedef typedef0;
  // };
};
// raddbg_type_view(Decl,
//                  kind == DeclProc   ? proc :
//                  kind == DECL_STRUCT ? struct0 :
//                  kind == DECL_UNION  ? union0 :
//                  kind == DeclEnum   ? enum0 :
//                  kind == DeclVar    ? var :
//                  kind == DeclConst  ? const0 :
//                  $);

struct Decl;

struct DeclList {
  Decl *first;
  Decl *last;
};

function Decl *decl_alloc(Parser *p, String8 name, DeclKind kind);
function Decl *decl_var(Parser *p, String8 name, TypeSpec *type_hint, Expr *init_expr);
function Decl *decl_const(Parser *p, String8 name, TypeSpec *type_hint, Expr *init_expr, TypeSpec *init_type);

// TODO order this properly
function Decl *parse_decl(Parser *p);
function DeclList parse_declarations(Parser *p);

///////////////////////////////////
// EXPRESSIONS

typedef enum ExprKind {
  ExprKind_Null,
  ExprKind_Ident,
  ExprKind_Unary,
  ExprKind_Binary,
  ExprKind_Ternary,
  ExprKind_NilLiteral,
  ExprKind_StringLiteral,
  ExprKind_IntegerLiteral,
  ExprKind_FloatLiteral,
  ExprKind_BoolLiteral,
  ExprKind_CharLiteral,
  ExprKind_Group,
  ExprKind_Cast,     // unary
  ExprKind_Call,     // postfix
  ExprKind_Index,    // postfix
  ExprKind_Field,    // postfix
  ExprKind_Compound, // primary/postfix hybrid | Type{}, [5]Type{...}, proc(a:
                     // int) -> float {}, &something
  ExprKind_SizeOf,   // unary
} ExprKind;

// TODO: rename to CompoundField, add compound_field_node for linked list
typedef enum CompoundFieldKind {
  CompoundFieldKind_None,
  CompoundFieldKind_Name,
  CompoundFieldKind_Index,
} CompoundFieldKind;

struct CompoundField {
  CompoundField *next;
  SourcePos pos;

  CompoundFieldKind kind;
  Expr *init;

  union {
    String8 name;
    Expr *index;
  };
};

struct CompoundFieldList {
  CompoundField *first;
  CompoundField *last;
  u64 count;
};

struct CompoundFieldArray {
  CompoundField **v;
  u64 count;
};

function void push_compound_field(Parser *p, CompoundFieldList *list,
                                  CompoundField *field);

struct ExprList {
  Expr *first;
  Expr *last;
  u64 count;
};

struct Expr {
  ExprKind kind;
  SourcePos pos;
  struct Type *type;

  Expr *next;

  union {
    String8 ident;

    struct {
      Token op;
      Expr *right;
    } unary;

    struct {
      Expr *left;
      Token op;
      Expr *right;
    } binary;

    struct {
      Expr *cond;
      Expr *then;
      Expr *else_;
    } ternary;

    LiteralValue literal;

    struct {
      Expr *expr;
    } group;

    struct {
      TypeSpec *type;
      Expr *expr;
    } cast;

    struct {
      Expr *expr; // TODO callee
      ExprArray args;
    } call;

    struct {
      Expr *expr; // TODO: operand
      Expr *index;
    } index;

    struct {
      Expr *expr; // TODO: operand
      String8 name;
    } field;

    struct {
      TypeSpec *type;
      // CompoundFieldList args;
      CompoundFieldArray args;
    } compound;

    struct {
      b32 is_expr;
      union {
        Expr *expr;
        TypeSpec *type;
      };
    } size_of;
  };
};
// raddbg_type_view(Expr,
//                  kind == EXPR_BINARY          ? binary :
//                  $);
// raddbg_type_view(Expr,
//                  kind == ExprKind.EXPR_IDENT           ? ident :
//                  kind == ExprKind.EXPR_UNARY           ? unary :
//                  kind == ExprKind.EXPR_BINARY          ? binary :
//                  kind == ExprKind.EXPR_TERNARY         ? ternary :
//                  kind == ExprKind.EXPR_NIL_LITERAL     ? "nil" :
//                  kind == ExprKind.EXPR_STRING_LITERAL  ? literal.string :
//                  kind == ExprKind.EXPR_INTEGER_LITERAL ? literal.integer :
//                  kind == ExprKind.EXPR_FLOAT_LITERAL   ? literal.floating :
//                  kind == ExprKind.EXPR_BOOL_LITERAL    ? literal.boolean :
//                  kind == ExprKind.EXPR_GROUP           ? group :
//                  $);

function Expr *expr_alloc(Parser *p, ExprKind kind);
function Expr *expr_ident(Parser *p, Token ident);
function Expr *expr_unary(Parser *p, Token op, Expr *right);
function Expr *expr_binary(Parser *p, Expr *left, Token op, Expr *right);
function Expr *expr_ternary(Parser *p, Expr *cond, Expr *then, Expr *else_);
function Expr *expr_nil_lit(Parser *p);
function Expr *expr_string_lit(Parser *p, String8 s);
function Expr *expr_integer_lit(Parser *p, u64 n);
function Expr *expr_float_lit(Parser *p, f64 f);
function Expr *expr_bool_lit(Parser *p, bool b);
function Expr *expr_char_lit(Parser *p, char c);
function Expr *expr_group(Parser *p, Expr *e);
function Expr *expr_cast(Parser *p, TypeSpec *type, Expr *e);
function Expr *expr_call(Parser *p, Expr *e, ExprArray args);
function Expr *expr_index(Parser *p, Expr *e, Expr *index);
function Expr *expr_field(Parser *p, Expr *e, String8 field);
function Expr *expr_compound(Parser *p, TypeSpec *type, CompoundFieldArray args);
function Expr *expr_size_of_expr(Parser *p, Expr *e);
function Expr *expr_size_of_type(Parser *p, TypeSpec *type);
