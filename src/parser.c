#include "parser.h"
#include "base.h"

internal Token
advance(Parser *p);

Parser
parser_init(Lexer *l)
{
  Parser p = {0};
  p.arena = arena_alloc(GB(1), MB(1), 0);
  p.lexer = l;
  advance(&p);
  return p;
}

// TODO: `parser_fini` destroy arena

AST *
ast_alloc(Parser *p)
{
  AST *node = push_struct(p->arena, AST);
  return node;
}

internal AST *
parse_expression(Parser *p);

internal void
report(Parser *p)
{

}

internal Token
advance(Parser *p)
{
  Token curr = p->curr;
  p->curr = lexer_next(p->lexer);
  p->prev = curr;
  return p->prev;
}

internal bool
parser_check(Parser *p, Token_Kind kind)
{
  // TODO: lexer_can_peek
  return p->curr.kind == kind;
}

internal bool
expect(Parser *p, Token_Kind kind)
{
  if (p->curr.kind == kind)
  {
    advance(p);
    return true;
  }
  printf("Expected '%.*s', got '%.*s'", strf(str_from_token_kind(kind)), strf(str_from_token_kind(p->curr.kind)));
  return false;
}

internal bool
match(Parser *p, Token_Kind kind)
{
  if (parser_check(p, kind))
  {
    advance(p);
    return true;
  }
  return false;
}

internal AST *
parse_expr_primary(Parser *p)
{
  if (match(p, TOKEN_TRUE))            return expr_bool_lit(p, true);
  if (match(p, TOKEN_FALSE))           return expr_bool_lit(p, false);
  if (match(p, TOKEN_NIL))             return expr_nil_lit(p);
  if (match(p, TOKEN_STRING_LITERAL))  return expr_string_lit(p, p->prev.value.string);
  if (match(p, TOKEN_INTEGER_LITERAL)) return expr_integer_lit(p, p->prev.value.integer);
  if (match(p, TOKEN_FLOAT_LITERAL))   return expr_float_lit(p, p->prev.value.floating);
  if (match(p, TOKEN_IDENT))           return expr_ident(p, p->prev);
  if (match(p, '('))
  {
    AST *expr = parse_expression(p);
    expect(p, ')');
    return expr_group(p, &expr->expr);
  }

  assert(!"Expected expression");
  return NULL;
}

internal AST *
parse_expr_unary(Parser *p)
{
  while (match(p, '!') || match(p, '-'))
  {
    Token op = p->prev;
    AST *right = parse_expr_unary(p);
    return expr_unary(p, op, &right->expr);
  }
  return parse_expr_primary(p);
}

internal AST *
parse_expr_factor(Parser *p)
{
  AST *expr = parse_expr_unary(p);

  while (match(p, '/') || match(p, '*'))
  {
    Token op = p->prev;
    AST *right = parse_expr_unary(p);
    expr = expr_binary(p, &expr->expr, op, &right->expr);
  }

  return expr;
}

internal AST *
parse_expr_term(Parser *p)
{
  AST *expr = parse_expr_factor(p);

  while (match(p, '-') || match(p, '+'))
  {
    Token op = p->prev;
    AST *right = parse_expr_factor(p);
    expr = expr_binary(p, &expr->expr, op, &right->expr);
  }

  return expr;
}

internal AST *
parse_expr_comparison(Parser *p)
{
  AST *expr = parse_expr_term(p);

  while (match(p, '>') || match(p, TOKEN_GTEQ) || match(p, '<') || match(p, TOKEN_LTEQ))
  {
    Token op = p->prev;
    AST *right = parse_expr_term(p);
    expr = expr_binary(p, &expr->expr, op, &right->expr);
  }

  return expr;
}

internal AST *
parse_expr_equality(Parser *p)
{
  AST *expr = parse_expr_comparison(p);

  while (match(p, TOKEN_NEQ) || match(p, TOKEN_EQ))
  {
    Token op = p->prev;
    AST *right = parse_expr_comparison(p);
    expr = expr_binary(p, &expr->expr, op, &right->expr);
  }

  return expr;
}

internal AST *
parse_expression(Parser *p)
{
  return parse_expr_equality(p);
}

internal void
parser_test(Parser *p)
{
  // ...
}
