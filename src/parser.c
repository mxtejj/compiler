#include "parser.h"
#include "base.h"

/*

primary            literals, identifiers, (), calls
unary              ! - ~ -- ++
postfix            ++ --
factor             * / %
term               + -
shift              << >>
bitwise AND        &
bitwise XOR        ^
bitwise OR         |
comparison         < <= > >=
equality           == !=
logical AND        &&
logical OR         ||
ternary            ?:
assignment         = += -= *= ...

*/

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

  printf("%.*s\n", strf(p->lexer->source));
  assert(!"Expected expression");
  return NULL;
}

internal AST *
parse_expr_unary(Parser *p)
{
  while (match(p, '!') || match(p, '-') || match(p, '~'))
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

  // TODO: %  - modulo (truncated)  - integers
  // TODO: %% - remainder (floored) - integers
  while (match(p, '/') || match(p, '*') || match(p, '%'))
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
parse_expr_shift(Parser *p)
{
  AST *expr = parse_expr_term(p);

  while (match(p, TOKEN_LSHIFT) || match(p, TOKEN_LSHIFT))
  {
    Token op = p->prev;
    AST *right = parse_expr_term(p);
    expr = expr_binary(p, &expr->expr, op, &right->expr);
  }

  return expr;
}

internal AST *
parse_expr_bitwise_and(Parser *p)
{
  AST *expr = parse_expr_shift(p);

  while (match(p, '&'))
  {
    Token op = p->prev;
    AST *right = parse_expr_shift(p);
    expr = expr_binary(p, &expr->expr, op, &right->expr);
  }

  return expr;
}

internal AST *
parse_expr_bitwise_xor(Parser *p)
{
  AST *expr = parse_expr_bitwise_and(p);

  while (match(p, '~'))
  {
    Token op = p->prev;
    AST *right = parse_expr_bitwise_and(p);
    expr = expr_binary(p, &expr->expr, op, &right->expr);
  }

  return expr;
}

internal AST *
parse_expr_bitwise_or(Parser *p)
{
  AST *expr = parse_expr_bitwise_xor(p);

  while (match(p, '|'))
  {
    Token op = p->prev;
    AST *right = parse_expr_bitwise_xor(p);
    expr = expr_binary(p, &expr->expr, op, &right->expr);
  }

  return expr;
}

internal AST *
parse_expr_comparison(Parser *p)
{
  AST *expr = parse_expr_bitwise_or(p);

  while (match(p, '>') || match(p, TOKEN_GTEQ) || match(p, '<') || match(p, TOKEN_LTEQ))
  {
    Token op = p->prev;
    AST *right = parse_expr_bitwise_or(p);
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
parse_expr_logical_and(Parser *p)
{
  AST *expr = parse_expr_equality(p);

  while (match(p, TOKEN_LOGICAL_AND))
  {
    Token op = p->prev;
    AST *right = parse_expr_equality(p);
    expr = expr_binary(p, &expr->expr, op, &right->expr);
  }

  return expr;
}

internal AST *
parse_expr_logical_or(Parser *p)
{
  AST *expr = parse_expr_logical_and(p);

  while (match(p, TOKEN_LOGICAL_OR))
  {
    Token op = p->prev;
    AST *right = parse_expr_logical_and(p);
    expr = expr_binary(p, &expr->expr, op, &right->expr);
  }

  return expr;
}

internal AST *
parse_expr_ternary(Parser *p)
{
  AST *expr = parse_expr_logical_or(p);

  while (match(p, '?'))
  {
    AST *then = parse_expression(p);
    expect(p, ':');
    AST *else_ = parse_expr_ternary(p); // RIGHT ASSOCIATIVE
    expr = expr_ternary(p, &expr->expr, &then->expr, &else_->expr);
  }

  return expr;
}

internal AST *
parse_expr_assignment(Parser *p)
{
  AST *expr = parse_expr_ternary(p);

  // use `if` if we want a = b = c (recursive)
  // use while if not
  if (match(p, '=')
   || match(p, TOKEN_ADD_ASSIGN)
   || match(p, TOKEN_SUB_ASSIGN)
   || match(p, TOKEN_DIV_ASSIGN)
   || match(p, TOKEN_MUL_ASSIGN)
   || match(p, TOKEN_AND_ASSIGN)
   // TODO: TOKEN_MOD_ASSIGN (%=)
   || match(p, TOKEN_LSHIFT_ASSIGN)
   || match(p, TOKEN_RSHIFT_ASSIGN)
   || match(p, TOKEN_OR_ASSIGN)
   || match(p, TOKEN_XOR_ASSIGN))
  {
    Token op = p->prev;
    AST *right = parse_expr_assignment(p); // RIGHT ASSOCIATIVE

    // semantic check later: left must be assignable
    expr = expr_binary(p, &expr->expr, op, &right->expr);
  }

  return expr;
}

internal AST *
parse_expression(Parser *p)
{
  //
  // TODO:
  // Add error productions to handle each binary operator appearing without a left-hand operand.
  // In other words, detect a binary operator appearing at the beginning of an expression.
  // Report that as an error, but also parse and discard a right-hand operand with the appropriate precedence.
  //
  return parse_expr_assignment(p);
}

internal void
parser_test(Parser *p)
{
  // ...
}
