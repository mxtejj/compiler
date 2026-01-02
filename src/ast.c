#include "parser.h"

#include <stdio.h>
#include <stdarg.h>

Type_Spec *
type_alloc(Parser *p, Type_Spec_Kind kind)
{
  Type_Spec *t = push_struct(p->arena, Type_Spec);
  t->kind = kind;
  return t;
}

Decl *
decl_alloc(Parser *p, Decl_Kind kind)
{
  Decl *decl = push_struct(p->arena, Decl);
  decl->kind = kind;
  return decl;
}

Expr *
expr_alloc(Parser *p, Expr_Kind kind)
{
  Expr *expr = push_struct(p->arena, Expr);
  expr->kind = kind;
  return expr;
}

Expr *
expr_ident(Parser *p, Token ident)
{
  Expr *expr = expr_alloc(p, EXPR_IDENT);
  expr->ident = ident.lexeme;
  return expr;
}

Expr *
expr_unary(Parser *p, Token op, Expr *right)
{
  Expr *expr = expr_alloc(p, EXPR_UNARY);
  expr->unary.op    = op;
  expr->unary.right = right;
  return expr;
}

Expr *
expr_binary(Parser *p, Expr *left, Token op, Expr *right)
{
  Expr *expr = expr_alloc(p, EXPR_BINARY);
  expr->binary.left  = left;
  expr->binary.op    = op;
  expr->binary.right = right;
  return expr;
}

Expr *
expr_ternary(Parser *p, Expr *cond, Expr *then, Expr *else_)
{
  Expr *expr = expr_alloc(p, EXPR_TERNARY);
  expr->ternary.cond = cond;
  expr->ternary.then = then;
  expr->ternary.else_ = else_;
  return expr;
}

Expr *
expr_nil_lit(Parser *p)
{
  Expr *expr = expr_alloc(p, EXPR_NIL_LITERAL);
  return expr;
}

Expr *
expr_string_lit(Parser *p, String8 s)
{
  Expr *expr = expr_alloc(p, EXPR_STRING_LITERAL);
  expr->literal.string = s;
  return expr;
}

Expr *
expr_integer_lit(Parser *p, u64 n)
{
  Expr *expr = expr_alloc(p, EXPR_INTEGER_LITERAL);
  expr->literal.integer = n;
  return expr;
}

Expr *
expr_float_lit(Parser *p, f64 f)
{
  Expr *expr = expr_alloc(p, EXPR_FLOAT_LITERAL);
  expr->literal.floating = f;
  return expr;
}

Expr *
expr_bool_lit(Parser *p, bool b)
{
  Expr *expr = expr_alloc(p, EXPR_BOOL_LITERAL);
  expr->literal.boolean = b;
  return expr;
}

Expr *
expr_group(Parser *p, Expr *e)
{
  Expr *expr = expr_alloc(p, EXPR_GROUP);
  expr->group.expr = e;
  return expr;
}

/////////////////////////////////////////////////////
// Statements
Stmt *
stmt_alloc(Parser *p, Stmt_Kind kind)
{
  Stmt *s = push_struct(p->arena, Stmt);
  s->kind = kind;
  return s;
}

Stmt *
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
