#include "parser.h"

#include <stdio.h>
#include <stdarg.h>

internal Type_Spec *
type_spec_alloc(Parser *p, Type_Spec_Kind kind)
{
  Type_Spec *t = push_struct(p->arena, Type_Spec);
  t->kind = kind;
  return t;
}

internal Decl *
decl_alloc(Parser *p, Decl_Kind kind)
{
  Decl *decl = push_struct(p->arena, Decl);
  decl->kind = kind;
  return decl;
}

internal Expr *
expr_alloc(Parser *p, Expr_Kind kind)
{
  Expr *expr = push_struct(p->arena, Expr);
  expr->kind = kind;
  return expr;
}

internal Expr *
expr_ident(Parser *p, Token ident)
{
  Expr *expr = expr_alloc(p, EXPR_IDENT);
  expr->ident = ident.lexeme;
  return expr;
}

internal Expr *
expr_unary(Parser *p, Token op, Expr *right)
{
  Expr *expr = expr_alloc(p, EXPR_UNARY);
  expr->unary.op    = op;
  expr->unary.right = right;
  return expr;
}

internal Expr *
expr_binary(Parser *p, Expr *left, Token op, Expr *right)
{
  Expr *expr = expr_alloc(p, EXPR_BINARY);
  expr->binary.left  = left;
  expr->binary.op    = op;
  expr->binary.right = right;
  return expr;
}

internal Expr *
expr_ternary(Parser *p, Expr *cond, Expr *then, Expr *else_)
{
  Expr *expr = expr_alloc(p, EXPR_TERNARY);
  expr->ternary.cond = cond;
  expr->ternary.then = then;
  expr->ternary.else_ = else_;
  return expr;
}

internal Expr *
expr_nil_lit(Parser *p)
{
  Expr *expr = expr_alloc(p, EXPR_NIL_LITERAL);
  return expr;
}

internal Expr *
expr_string_lit(Parser *p, String8 s)
{
  Expr *expr = expr_alloc(p, EXPR_STRING_LITERAL);
  expr->literal.string = s;
  return expr;
}

internal Expr *
expr_integer_lit(Parser *p, u64 n)
{
  Expr *expr = expr_alloc(p, EXPR_INTEGER_LITERAL);
  expr->literal.integer = n;
  return expr;
}

internal Expr *
expr_float_lit(Parser *p, f64 f)
{
  Expr *expr = expr_alloc(p, EXPR_FLOAT_LITERAL);
  expr->literal.floating = f;
  return expr;
}

internal Expr *
expr_bool_lit(Parser *p, bool b)
{
  Expr *expr = expr_alloc(p, EXPR_BOOL_LITERAL);
  expr->literal.boolean = b;
  return expr;
}

internal Expr *
expr_group(Parser *p, Expr *e)
{
  Expr *expr = expr_alloc(p, EXPR_GROUP);
  expr->group.expr = e;
  return expr;
}

internal Expr *
expr_cast(Parser *p, Type_Spec *type, Expr *e)
{
  Expr *expr = expr_alloc(p, EXPR_CAST);
  expr->cast.type = type;
  expr->cast.expr = e;
  return expr;
}

internal Expr *
expr_call(Parser *p, Expr *e, Expr_List args)
{
  Expr *expr = expr_alloc(p, EXPR_CALL);
  expr->call.expr = e;
  expr->call.args = args;
  return expr;
}

internal Expr *
expr_index(Parser *p, Expr *e, Expr *index)
{
  Expr *expr = expr_alloc(p, EXPR_INDEX);
  expr->index.expr = e;
  expr->index.index = index;
  return expr;
}

internal Expr *
expr_field(Parser *p, Expr *e, String8 field)
{
  Expr *expr = expr_alloc(p, EXPR_FIELD);
  expr->field.expr = e;
  expr->field.name = field;
  return expr;
}

internal Expr *
expr_compound(Parser *p, Type_Spec *type, Compound_Arg_List args)
{
  Expr *expr = expr_alloc(p, EXPR_COMPOUND);
  expr->compound.type = type;
  expr->compound.args = args;
  return expr;
}

internal Expr *
expr_size_of_expr(Parser *p, Expr *e)
{
  Expr *expr = expr_alloc(p, EXPR_SIZE_OF_EXPR);
  expr->size_of_expr = e;
  return expr;
}

internal Expr *
expr_size_of_type(Parser *p, Type_Spec *type)
{
  Expr *expr = expr_alloc(p, EXPR_SIZE_OF_TYPE);
  expr->size_of_type = type;
  return expr;
}

/////////////////////////////////////////////////////
// Statements
internal Stmt *
stmt_alloc(Parser *p, Stmt_Kind kind)
{
  Stmt *s = push_struct(p->arena, Stmt);
  s->kind = kind;
  return s;
}

internal Stmt *
stmt_expr(Parser *p, Expr *e)
{
  Stmt *s = stmt_alloc(p, STMT_EXPR);
  s->expr = e;
  return s;
}

// TODO: for testing
internal Token
make_token(char c)
{
  return (Token){ .kind = c };
}

internal usize
print_expr(char *buf, usize buf_size, Expr *e);

internal usize
print_type(char *buf, usize buf_size, Type_Spec *t)
{
  usize n = 0;

  switch (t->kind)
  {
  case TYPE_SPEC_NULL:
    n += snprintf(buf + n, buf_size - n, "<NULL>");
    break;
  case TYPE_SPEC_NAME:
    n += snprintf(buf + n, buf_size - n, "%.*s", str8_varg(t->name));
    break;
  // case TYPE_SPEC_PROC:
  //   n += snprintf(buf + n, buf_size - n, "proc");
  //   break;
  case TYPE_SPEC_ARRAY:
    n += snprintf(buf + n, buf_size - n, "[");
    n += print_expr(buf + n, buf_size - n, t->array.count);
    n += snprintf(buf + n, buf_size - n, "]");
    n += print_type(buf + n, buf_size - n, t->array.elem);
    break;
  case TYPE_SPEC_SLICE:
    n += snprintf(buf + n, buf_size - n, "[]");
    n += print_type(buf + n, buf_size - n, t->slice.elem);
    break;
  case TYPE_SPEC_PTR:
    n += snprintf(buf + n, buf_size - n, "*");
    n += print_type(buf + n, buf_size - n, t->ptr.pointee);
    break;
  default:
    assert(0);
    break;
  }

  return n;
}

internal void
print_ln(Arena *arena, String8List *list, int *indent)
{
  str8_list_pushf(arena, list, "\n%.*s", (*indent), "                                     ");
}

internal void
print_decl(Arena *arena, String8List *list, int *indent, Decl *d)
{
  switch (d->kind)
  {
  case DECL_NULL:
    str8_list_pushf(arena, list, "<NULL>");
    break;
  case DECL_PROC:
    // d->proc.
    str8_list_pushf(arena, list, "(proc %.*s ", str8_varg(d->name));
    str8_list_pushf(arena, list, "(");
    for (Proc_Param *it = d->proc.params.first;
         it != 0;
         it = it->next)
    {
      
    }
    str8_list_pushf(arena, list, ")");

    str8_list_pushf(arena, list, ")");
    break;
  case DECL_STRUCT:
    break;
  case DECL_UNION:
    break;
  case DECL_ENUM:
    str8_list_pushf(arena, list, "(enum %.*s ", str8_varg(d->name));
    *indent++;
    for (Enum_Member *it = d->enum0.members.first;
         it != 0;
         it = it->next)
    {
      print_ln(arena, list, indent);
      str8_list_pushf(arena, list, "(%.*s ", str8_varg(it->name));
      if (it->value)
      {
        //str8_list_pushf(arena, list, "(%.*s ", str8_varg(it->name));
        // print_expr(arena, list, indent, it->value);
        str8_list_pushf(arena, list, "TODO");
      }
      else
      {
        str8_list_pushf(arena, list, "nil");
      }
      str8_list_pushf(arena, list, ")");
    }
    *indent--;
    str8_list_pushf(arena, list, ")");
    break;
  case DECL_VAR:
    break;
  case DECL_CONST:
    break;
  default:
    assert(0);
    break;
  }
}

internal usize
print_expr(char *buf, usize buf_size, Expr *e)
{
  usize n = 0;

  switch (e->kind)
  {
  case EXPR_NULL: n += snprintf(buf + n, buf_size - n, "<NULL>\n"); break;
  case EXPR_IDENT:
    n += snprintf(buf + n, buf_size - n, "%.*s", str8_varg(e->ident));
    break;
  case EXPR_UNARY:
    if (e->unary.op.kind < 128)
    {
      n += snprintf(buf + n, buf_size - n, "(%c ", e->unary.op.kind);
    }
    else
    {
      n += snprintf(buf + n, buf_size - n, "(%.*s ", str8_varg(e->unary.op.lexeme));
    }
    n += print_expr(buf + n, buf_size - n, e->unary.right);
    n += snprintf(buf + n, buf_size - n, ")");
    break;
  case EXPR_BINARY:
    if (e->binary.op.kind < 128)
    {
      n += snprintf(buf + n, buf_size - n, "(%c ", e->binary.op.kind);
    }
    else
    {
      n += snprintf(buf + n, buf_size - n, "(%.*s ", str8_varg(e->binary.op.lexeme));
    }
    n += print_expr(buf + n, buf_size - n, e->binary.left);
    n += snprintf(buf + n, buf_size - n, " ");
    n += print_expr(buf + n, buf_size - n, e->binary.right);
    n += snprintf(buf + n, buf_size - n, ")");
    break;
  case EXPR_TERNARY:
    n += snprintf(buf + n, buf_size - n, "(?: ");
    n += print_expr(buf + n, buf_size - n, e->ternary.cond);
    n += snprintf(buf + n, buf_size - n, " ");
    n += print_expr(buf + n, buf_size - n, e->ternary.then);
    n += snprintf(buf + n, buf_size - n, " ");
    n += print_expr(buf + n, buf_size - n, e->ternary.else_);
    n += snprintf(buf + n, buf_size - n, ")");
    break;
  case EXPR_NIL_LITERAL:
    n += snprintf(buf + n, buf_size - n, "nil");
    break;
  case EXPR_STRING_LITERAL:
    n += snprintf(buf + n, buf_size - n, "%.*s", str8_varg(e->literal.string));
    break;
  case EXPR_INTEGER_LITERAL:
    n += snprintf(buf + n, buf_size - n, "%llu", e->literal.integer);
    break;
  case EXPR_FLOAT_LITERAL:
    n += snprintf(buf + n, buf_size - n, "%.2f", e->literal.floating);
    break;
  case EXPR_BOOL_LITERAL:
    n += snprintf(buf + n, buf_size - n, e->literal.boolean ? "true" : "false");
    break;
  case EXPR_GROUP:
    n += snprintf(buf + n, buf_size - n, "(group ");
    n += print_expr(buf + n, buf_size - n, e->group.expr);
    n += snprintf(buf + n, buf_size - n, ")");
    break;
  case EXPR_CAST:
    n += snprintf(buf + n, buf_size - n, "(cast ");
    break;
  case EXPR_CALL:
    n += snprintf(buf + n, buf_size - n, "(call ");
    n += print_expr(buf + n, buf_size - n, e->call.expr);
    for (Expr *arg = e->call.args.first;
         arg != 0;
         arg = arg->next)
    {
      n += snprintf(buf + n, buf_size - n, " ");
      n += print_expr(buf + n, buf_size - n, arg);
    }
    n += snprintf(buf + n, buf_size - n, ")");
    break;
  case EXPR_INDEX:
    n += snprintf(buf + n, buf_size - n, "(index ");
    n += print_expr(buf + n, buf_size - n, e->index.expr);
    n += snprintf(buf + n, buf_size - n, " ");
    n += print_expr(buf + n, buf_size - n, e->index.index);
    n += snprintf(buf + n, buf_size - n, ")");
    break;
  case EXPR_FIELD:
    n += snprintf(buf + n, buf_size - n, "(field ");
    n += print_expr(buf + n, buf_size - n, e->field.expr);
    n += snprintf(buf + n, buf_size - n, " ");
    n += snprintf(buf + n, buf_size - n, "%.*s", str8_varg(e->field.name));
    n += snprintf(buf + n, buf_size - n, ")");
    break;
  case EXPR_COMPOUND:
    n += snprintf(buf + n, buf_size - n, "(compound");
    if (e->compound.type != TYPE_SPEC_NULL)
    {
      n += snprintf(buf + n, buf_size - n, " ");
    }
    n += print_type(buf + n, buf_size - n, e->compound.type);
    for (Compound_Arg *arg = e->compound.args.first;
         arg != 0;
         arg = arg->next)
    {
      n += snprintf(buf + n, buf_size - n, " ");
      if (arg->optional_name.count > 0)
      {
        n += snprintf(buf + n, buf_size - n, "%.*s=", str8_varg(arg->optional_name));
      }
      n += print_expr(buf + n, buf_size - n, arg->expr);
    }
    n += snprintf(buf + n, buf_size - n, ")");
    break;
  case EXPR_SIZE_OF_EXPR:
    n += snprintf(buf + n, buf_size - n, "(sizeof_expr ");
    n += snprintf(buf + n, buf_size - n, ")");
    break;
  case EXPR_SIZE_OF_TYPE:
    n += snprintf(buf + n, buf_size - n, "(sizeof_type ");
    n += snprintf(buf + n, buf_size - n, ")");
    break;
  default:
    assert(!"TODO");
    break;
  }

  return n;
}

internal usize
print_stmt(char *buf, usize buf_size, Stmt *s)
{
  usize n = 0;

  switch (s->kind)
  {
  case STMT_NULL: assert(!"NULL Statement"); break;
  case STMT_EXPR:
    n += print_expr(buf, buf_size, s->expr);
    break;
  default:
    assert(!"TODO");
    break;
  }

  return n;
}

internal void
ast_test(Parser *p)
{
  // Expr *int_literal = expr_integer_lit(p, 123);
  // Expr *float_literal = expr_float_lit(p, 45.67);

  // Expr *unary = expr_unary(p, make_token('-'), &int_literal->expr);
  // Expr *group = expr_group(p, &float_literal->expr);

  // Expr *e = expr_binary(
  //   p,
  //   &unary->expr,
  //   make_token('*'),
  //   &group->expr
  // );

  // print_expr(&e->expr);
}
