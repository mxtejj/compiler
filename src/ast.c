#include "ast.h"

#include <stdio.h>
#include <stdarg.h>

internal AST *
expr_alloc(Parser *p, Expr_Kind kind)
{
  AST *node = ast_alloc(p);
  node->kind = AST_EXPR;
  node->expr.kind = kind;
  return node;
}

AST *
expr_ident(Parser *p, Token ident)
{
  AST *node = expr_alloc(p, EXPR_IDENT);
  node->expr.ident = ident.lexeme;
  return node;
}

AST *
expr_unary(Parser *p, Token op, Expr *right)
{
  AST *node = expr_alloc(p, EXPR_UNARY);
  node->expr.unary.op    = op;
  node->expr.unary.right = right;
  return node;
}

AST *
expr_binary(Parser *p, Expr *left, Token op, Expr *right)
{
  AST *node = expr_alloc(p, EXPR_BINARY);
  node->expr.binary.left  = left;
  node->expr.binary.op    = op;
  node->expr.binary.right = right;
  return node;
}

AST *
expr_ternary(Parser *p)
{
  AST *node = expr_alloc(p, EXPR_TERNARY);
  assert(!"TODO");
  return node;
}

AST *
expr_nil_lit(Parser *p)
{
  AST *node = expr_alloc(p, EXPR_NIL_LITERAL);
  return node;
}

AST *
expr_string_lit(Parser *p, String s)
{
  AST *node = expr_alloc(p, EXPR_STRING_LITERAL);
  node->expr.literal.string = s;
  return node;
}

AST *
expr_integer_lit(Parser *p, u64 n)
{
  AST *node = expr_alloc(p, EXPR_INTEGER_LITERAL);
  node->expr.literal.integer = n;
  return node;
}

AST *
expr_float_lit(Parser *p, f64 f)
{
  AST *node = expr_alloc(p, EXPR_FLOAT_LITERAL);
  node->expr.literal.floating = f;
  return node;
}

AST *
expr_bool_lit(Parser *p, bool b)
{
  AST *node = expr_alloc(p, EXPR_BOOL_LITERAL);
  node->expr.literal.boolean = b;
  return node;
}

AST *
expr_group(Parser *p, Expr *expr)
{
  AST *node = expr_alloc(p, EXPR_GROUP);
  node->expr.group.expr = expr;
  return node;
}

// TODO: for testing
internal Token
make_token(char c)
{
  return (Token){ .kind = c };
}

internal void
ast_print(Expr *e)
{
  switch (e->kind)
  {
  case EXPR_NULL: printf("<NULL>\n"); break;
  case EXPR_IDENT:
    printf("%.*s", strf(e->ident));
    break;
  case EXPR_UNARY:
    if (e->unary.op.kind < 128)
    {
      printf("(%c ", e->unary.op.kind);
    }
    else
    {
      printf("(%.*s ", strf(e->unary.op.lexeme));
    }
    ast_print(e->unary.right);
    printf(")");
    break;
  case EXPR_BINARY:
    if (e->binary.op.kind < 128)
    {
      printf("(%c ", e->binary.op.kind);
    }
    else
    {
      printf("(%.*s ", strf(e->binary.op.lexeme));
    }
    ast_print(e->binary.left);
    printf(" ");
    ast_print(e->binary.right);
    printf(")");
    break;
  case EXPR_TERNARY:
    assert(!"TODO EXPR_TERNARY");
    break;
  case EXPR_NIL_LITERAL:
    printf("nil");
    break;
  case EXPR_STRING_LITERAL:
    printf("%.*s", strf(e->literal.string));
    break;
  case EXPR_INTEGER_LITERAL:
    printf("%llu", e->literal.integer);
    break;
  case EXPR_FLOAT_LITERAL:
    printf("%.2f", e->literal.floating);
    break;
  case EXPR_BOOL_LITERAL:
    printf(e->literal.boolean ? "true" : "false");
    break;
  case EXPR_GROUP:
    printf("(group ");
    ast_print(e->group.expr);
    printf(")");
    break;
  default:
    assert(!"TODO");
    break;
  }
}

internal void
ast_test(Parser *p)
{
  AST *int_literal = expr_integer_lit(p, 123);
  AST *float_literal = expr_float_lit(p, 45.67);

  AST *unary = expr_unary(p, make_token('-'), &int_literal->expr);
  AST *group = expr_group(p, &float_literal->expr);

  AST *e = expr_binary(
    p,
    &unary->expr,
    make_token('*'),
    &group->expr
  );

  ast_print(&e->expr);
}
