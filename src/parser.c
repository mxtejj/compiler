#include "parser.h"
#include "base.h"
#include <stdio.h>

// TODO parser_synchronize https://craftinginterpreters.com/parsing-expressions.html#synchronizing-a-recursive-descent-parser

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
range              ..< ..=
logical AND        &&
logical OR         ||
ternary            ?:
assignment         = += -= *= ...

*/

internal Token
advance(Parser *p);

internal Type_Spec *
parse_type(Parser *p);

internal Parser
parser_init(Lexer *l)
{
  Parser p = {0};
  p.arena = arena_alloc(GB(1), MB(1), 0);
  p.lexer = l;
  // advance(&p);

  p.curr = lexer_next(l);
  p.next = lexer_next(l);

  return p;
}

internal void
push_compound_field(Compound_Field_List *list, Compound_Field *field)
{
  sll_queue_push(list->first, list->last, field);
  list->count += 1;
}

// TODO: `parser_fini` destroy arena

internal Expr *
parse_expr(Parser *p);

internal void
report_error(Parser *p, const char *fmt, ...)
{
  // TODO: duplicate of lexer_syntax_error
  Lexer *l = p->lexer;

  va_list args;
  va_start(args, fmt);

  // printf(WHT "%.*s(%llu:%llu) ", strf(l->file_path), lexer_row(l), lexer_col(l));
  printf(CLR_WHT "(%llu:%llu) ", lexer_row(l), lexer_col(l));
  printf(CLR_RED "Syntax Error: " CLR_RESET);
  vprintf(fmt, args);
  printf("\n");

  u64 line_start = l->bol;
  u64 line_end = line_start;
  while (line_end < l->source.count && l->source.data[line_end] != '\n' && l->source.data[line_end] != '\r')
    line_end++;

  printf(CLR_CYN "%.*s\n", (int)(line_end - line_start), &l->source.data[line_start]);
  u64 col = lexer_col(l) - 1;
  printf("%*s" CLR_GRN "^ here\n" CLR_RESET, (int)col, "");

  va_end(args);

  trap();

  // TODO: Make some errors non fatal
  // getchar();
  // os_exit(1);
}

internal Token
advance(Parser *p)
{
  p->prev = p->curr;
  p->curr = p->next;
  p->next = lexer_next(p->lexer);

  // Token curr = p->curr;
  // p->curr = lexer_next(p->lexer);
  // p->prev = curr;
  // p->next = lexer_next(p->lexer);
  return p->prev;
}

internal bool
check(Parser *p, Token_Kind kind)
{
  // lexer_can_peek(p->lexer) && 
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

  Arena_Temp scratch = arena_scratch_get(0, 0);

  String8 expected = str_from_token_kind(scratch.arena, kind);
  String8 got      = str_from_token_kind(scratch.arena, p->curr.kind);

  report_error(p, "Expected '%.*s', got '%.*s'", str8_varg(expected), str8_varg(got));

  arena_scratch_release(scratch);
  trap();
  return false;
}

internal bool
match(Parser *p, Token_Kind kind)
{
  if (check(p, kind))
  {
    advance(p);
    return true;
  }
  return false;
}

internal String8
parse_ident(Parser *p)
{
  if (expect(p, TOKEN_IDENT))
  {
    return str8_copy(p->arena, p->prev.lexeme);
  }
  assert(0);
  return (String8){0};
}

internal b32
peek(Parser *p, Token_Kind kind)
{
  return p->next.kind == kind;
}

internal bool
is_type_start(Parser *p)
{
  // TODO: Support inline types thingy
  // like a := [2]struct{ name: string, age: int }{}
  //      a := [2]Person{}
  switch (p->curr.kind)
  {
  case TOKEN_IDENT:
  case TOKEN_STRING:
  case TOKEN_S8:
  case TOKEN_S16:
  case TOKEN_S32:
  case TOKEN_S64:
  case TOKEN_U8:
  case TOKEN_U16:
  case TOKEN_U32:
  case TOKEN_U64:
  case TOKEN_UINTPTR:
  case TOKEN_INT:
  case TOKEN_UINT:
  case TOKEN_F32:
  case TOKEN_F64:
  case TOKEN_BOOL:
  case TOKEN_PROC:
  case '*':
  case '[':
    return true;
  }
  return false;
}

// NOTE(mxtej): doesn't contain identifiers
internal bool
is_explicit_type_start(Parser *p)
{
  switch (p->curr.kind)
  {
  // built-in type keywords
  case TOKEN_STRING:
  case TOKEN_S8:      case TOKEN_S16: case TOKEN_S32: case TOKEN_S64:
  case TOKEN_U8:      case TOKEN_U16: case TOKEN_U32: case TOKEN_U64:
  case TOKEN_UINTPTR: case TOKEN_INT: case TOKEN_UINT:
  case TOKEN_F32:     case TOKEN_F64: case TOKEN_BOOL:

  // type literal keywords
  case TOKEN_PROC: case TOKEN_STRUCT: case TOKEN_UNION: case TOKEN_ENUM:

  // type operators
  case '*':
  case '[':
    return true;
  }
  return false;
}

internal Type_Spec *parse_type_base(Parser *p);
internal Expr *parse_compound_or_expr(Parser *p);

internal Compound_Field *
parse_compound_field(Parser *p)
{
  Compound_Field *field = push_struct(p->arena, Compound_Field);

  // [index] = value
  if (match(p, '['))
  {
    field->kind = COMPOUND_FIELD_INDEX;
    field->index = parse_expr(p);
    expect(p, ']');
    expect(p, '=');
    field->init = parse_expr(p); // parse_compound_or_expr
  }
  // name = value
  else if (check(p, TOKEN_IDENT) && peek(p, '='))
  {
    field->kind = COMPOUND_FIELD_NAME;
    field->name = parse_ident(p);
    advance(p); // Consume '='
    field->init = parse_expr(p); // parse_compound_or_expr
  }
  // value
  else
  {
    field->kind = COMPOUND_FIELD_NONE;
    field->init = parse_expr(p); // parse_compound_or_expr
  }

  return field;
}

internal Expr *
parse_compound_literal(Parser *p, Type_Spec *explicit_type)
{
  expect(p, '{');

  // TODO: use Compound_Field_Node and arena scratch for list; also in other places
  Compound_Field_List list = {0};

  if (!match(p, '}'))
  {
    do
    {
      Compound_Field *field = parse_compound_field(p);
      push_compound_field(&list, field);
    } while (match(p, ','));

    expect(p, '}');
  }

  Compound_Field_Array fields = {0};
  if (list.count > 0)
  {
    fields.count = list.count;
    fields.v = push_array_nz(p->arena, Compound_Field *, fields.count);

    u32 i = 0;
    for each_node(it, Compound_Field, list.first)
    {
      fields.v[i++] = it;
    }
  }

  return expr_compound(p, explicit_type, fields);
}

internal b32
looks_like_dotted_compound(Parser *p)
{
  Arena_Temp scratch = arena_scratch_get(0, 0);

  // Temporary clone of parser and lexer
  Parser tmp = *p;
  Lexer temp_lexer = *p->lexer;
  tmp.arena = scratch.arena;
  tmp.lexer = &temp_lexer;

  Type_Spec *type = parse_type(&tmp);
  b32 result = (type != NULL && check(&tmp, '.') && peek(&tmp, '{'));

  arena_scratch_release(scratch);
  return result;
}

internal Expr *
parse_expr_primary(Parser *p)
{
  // implicit compound literal
  if (check(p, '.') && peek(p, '{'))
  {
    advance(p); // eat dot
    return parse_compound_literal(p, NULL);
  }
  // explicit compound literal
  if (is_type_start(p) && looks_like_dotted_compound(p))
  {
    Type_Spec *type = parse_type(p);
    expect(p, '.');
    return parse_compound_literal(p, type);
  }

  if (match(p, TOKEN_TRUE))
  {
    return expr_bool_lit(p, true);
  }
  if (match(p, TOKEN_FALSE))
  {
    return expr_bool_lit(p, false);
  }
  if (match(p, TOKEN_NIL))
  {
    return expr_nil_lit(p);
  }
  if (match(p, TOKEN_STRING_LITERAL))
  {
    return expr_string_lit(p, p->prev.value.string);
  }
  if (match(p, TOKEN_INTEGER_LITERAL))
  {
    return expr_integer_lit(p, p->prev.value.integer);
  }
  if (match(p, TOKEN_FLOAT_LITERAL))
  {
    return expr_float_lit(p, p->prev.value.floating);
  }
  if (match(p, TOKEN_CHAR_LITERAL))
  {
    return expr_char_lit(p, p->prev.value.character);
  }
  if (match(p, TOKEN_IDENT))
  {
    return expr_ident(p, p->prev);
  }
  if (match(p, '('))
  {
    Expr *expr = parse_expr(p);
    expect(p, ')');
    return expr_group(p, expr);
  }

error:
  // printf("%.*s\n", str8_varg(p->lexer->source));
  // assert(!"Expected expression");
  report_error(p, "Expected expression");
  return NULL;
}

// primary
internal Expr *
parse_expr_postfix(Parser *p)
{
  Expr *expr = parse_expr_primary(p);

  while (true)
  {
    // CALL: foo(...)
    if (match(p, '('))
    {
      Expr_List args = {0};
      u64 count = 0;

      if (!match(p, ')'))
      {
        do
        {
          Expr *arg = parse_expr(p);
          sll_queue_push(args.first, args.last, arg);
          count += 1;
        } while (match(p, ','));
        expect(p, ')');
      }

      Expr_Array arg_array = {0};
      if (count > 0)
      {
        arg_array.count = count;
        arg_array.v = push_array(p->arena, Expr*, count);

        u32 i = 0;
        for each_node(it, Expr, args.first)
        {
          arg_array.v[i++] = it;
        }
      }

      expr = expr_call(p, expr, arg_array);
      continue;
    }

    // INDEX: a[b]
    if (match(p, '['))
    {
      Expr *index = parse_expr(p);
      expect(p, ']');
      expr = expr_index(p, expr, index);
      continue;
    }

    // DEREF: ptr.*
    if (match(p, TOKEN_DEREF))
    {
      Token op = p->prev;
      expr = expr_unary(p, op, expr);
      continue;
    }

    // FIELD: person.name
    if (match(p, '.'))
    {
      String8 name = parse_ident(p);
      expr = expr_field(p, expr, name);
      continue;
    }

    // POSTFIX ++ --
    // if (match(p, TOKEN_INCREMENT) || match(p, TOKEN_DECREMENT))
    // {
    //   Token op = p->prev;
    //   expr = expr_postfix(p, expr, op);
    //   continue;
    // }

    break;
  }

  return expr;
}

internal Expr *
parse_expr_unary(Parser *p)
{
  if (match(p, TOKEN_CAST))
  {
    // cast(f32)a
    expect(p, '(');
    Type_Spec *type = parse_type(p);
    expect(p, ')');
    Expr *expr = parse_expr_unary(p);
    return expr_cast(p, type, expr);
  }

  if (match(p, TOKEN_SIZE_OF))
  {
    Expr *expr = NULL;

    expect(p, '(');
    if (match(p, ':'))
    {
      // size_of(:Entity) TYPE
      Type_Spec *type = parse_type(p);
      expect(p, ')');
      expr = expr_size_of_type(p, type);
    }
    else
    {
      // size_of(f32)     EXPR
      Expr *e = parse_expr(p);
      expect(p, ')');
      expr = expr_size_of_expr(p, e);
    }
    return expr;
  }

  // prefix operators
  // TODO(mxtej) add?: increment, decrement
  while (match(p, '!') || match(p, '-') || match(p, '+') || match(p, '~') || match(p, '&'))
  {
    Token op = p->prev;
    Expr *right = parse_expr_unary(p);
    return expr_unary(p, op, right);
  }
  return parse_expr_postfix(p);
  // return parse_expr_primary(p);
}

internal Expr *
parse_expr_factor(Parser *p)
{
  Expr *expr = parse_expr_unary(p);

  // TODO: %  - modulo (truncated)  - integers
  // TODO: %% - remainder (floored) - integers
  while (match(p, '/') || match(p, '*') || match(p, '%'))
  {
    Token op = p->prev;
    Expr *right = parse_expr_unary(p);
    expr = expr_binary(p, expr, op, right);
  }

  return expr;
}

internal Expr *
parse_expr_term(Parser *p)
{
  Expr *expr = parse_expr_factor(p);

  while (match(p, '-') || match(p, '+'))
  {
    Token op = p->prev;
    Expr *right = parse_expr_factor(p);
    expr = expr_binary(p, expr, op, right);
  }

  return expr;
}

internal Expr *
parse_expr_shift(Parser *p)
{
  Expr *expr = parse_expr_term(p);

  while (match(p, TOKEN_LSHIFT) || match(p, TOKEN_LSHIFT))
  {
    Token op = p->prev;
    Expr *right = parse_expr_term(p);
    expr = expr_binary(p, expr, op, right);
  }

  return expr;
}

internal Expr *
parse_expr_bitwise_and(Parser *p)
{
  Expr *expr = parse_expr_shift(p);

  while (match(p, '&'))
  {
    Token op = p->prev;
    Expr *right = parse_expr_shift(p);
    expr = expr_binary(p, expr, op, right);
  }

  return expr;
}

internal Expr *
parse_expr_bitwise_xor(Parser *p)
{
  Expr *expr = parse_expr_bitwise_and(p);

  while (match(p, '~'))
  {
    Token op = p->prev;
    Expr *right = parse_expr_bitwise_and(p);
    expr = expr_binary(p, expr, op, right);
  }

  return expr;
}

internal Expr *
parse_expr_bitwise_or(Parser *p)
{
  Expr *expr = parse_expr_bitwise_xor(p);

  while (match(p, '|'))
  {
    Token op = p->prev;
    Expr *right = parse_expr_bitwise_xor(p);
    expr = expr_binary(p, expr, op, right);
  }

  return expr;
}

internal Expr *
parse_expr_comparison(Parser *p)
{
  Expr *expr = parse_expr_bitwise_or(p);

  while (match(p, '>') || match(p, TOKEN_GTEQ) || match(p, '<') || match(p, TOKEN_LTEQ))
  {
    Token op = p->prev;
    Expr *right = parse_expr_bitwise_or(p);
    expr = expr_binary(p, expr, op, right);
  }

  return expr;
}

internal Expr *
parse_expr_equality(Parser *p)
{
  Expr *expr = parse_expr_comparison(p);

  while (match(p, TOKEN_NEQ) || match(p, TOKEN_EQ))
  {
    Token op = p->prev;
    Expr *right = parse_expr_comparison(p);
    expr = expr_binary(p, expr, op, right);
  }

  return expr;
}

internal Expr *
parse_expr_range(Parser *p)
{
  Expr *expr = parse_expr_equality(p);

  while (match(p, TOKEN_RANGE_EXCL) || match(p, TOKEN_RANGE_INCL))
  {
    Token op = p->prev;
    Expr *right = parse_expr_equality(p);
    expr = expr_binary(p, expr, op, right);
  }

  return expr;
}

internal Expr *
parse_expr_logical_and(Parser *p)
{
  Expr *expr = parse_expr_range(p);

  while (match(p, TOKEN_LOGICAL_AND))
  {
    Token op = p->prev;
    Expr *right = parse_expr_range(p);
    expr = expr_binary(p, expr, op, right);
  }

  return expr;
}

internal Expr *
parse_expr_logical_or(Parser *p)
{
  Expr *expr = parse_expr_logical_and(p);

  while (match(p, TOKEN_LOGICAL_OR))
  {
    Token op = p->prev;
    Expr *right = parse_expr_logical_and(p);
    expr = expr_binary(p, expr, op, right);
  }

  return expr;
}

internal Expr *
parse_expr_ternary(Parser *p)
{
  Expr *expr = parse_expr_logical_or(p);

  while (match(p, '?'))
  {
    Expr *then = parse_expr(p);
    expect(p, ':');
    Expr *else0 = parse_expr_ternary(p); // RIGHT ASSOCIATIVE
    expr = expr_ternary(p, expr, then, else0);
  }

  return expr;
}

internal b32
is_assign_op(Token_Kind kind)
{
  switch (kind)
  {
  case '=':
  case TOKEN_ADD_ASSIGN:
  case TOKEN_SUB_ASSIGN:
  case TOKEN_DIV_ASSIGN:
  case TOKEN_MUL_ASSIGN:
  case TOKEN_AND_ASSIGN:
  // case TOKEN_MOD_ASSIGN:
  case TOKEN_LSHIFT_ASSIGN:
  case TOKEN_RSHIFT_ASSIGN:
  case TOKEN_OR_ASSIGN:
  case TOKEN_XOR_ASSIGN:
    return true;
  default:
    break;
  }
  return false;
}

internal Expr *
parse_expr_assignment(Parser *p)
{
  Expr *expr = parse_expr_ternary(p);

  // use `if` if we want a = b = c (recursive)
  // use while if not
  if (is_assign_op(p->curr.kind))
  {
    advance(p);

    Token op = p->prev;
    Expr *right = parse_expr_assignment(p); // RIGHT ASSOCIATIVE

    // semantic check later: left must be assignable
    expr = expr_binary(p, expr, op, right);
  }

  return expr;
}

internal Expr *
parse_expr(Parser *p)
{
  //
  // TODO:
  // Add error productions to handle each binary operator appearing without a left-hand operand.
  // In other words, detect a binary operator appearing at the beginning of an expression.
  // Report that as an error, but also parse and discard a right-hand operand with the appropriate precedence.
  //
  return parse_expr_assignment(p);
}

/////////////////////////////////////////////////////
//- Statements
internal Stmt *
parse_stmt(Parser *p);

internal Stmt *
parse_stmt_expr_nosemi(Parser *p)
{
  Expr *expr = parse_expr(p);
  return stmt_expr(p, expr);
}

internal Stmt *
parse_stmt_expr(Parser *p)
{
  Expr *expr = parse_expr(p);
  expect(p, ';');
  return stmt_expr(p, expr);
}

internal Stmt *
parse_stmt_block(Parser *p)
{
  expect(p, '{');
  Stmt_List list = {0};
  while (!check(p, '}'))
  {
    Stmt *stmt = parse_stmt(p);
    if (stmt->kind != STMT_NULL)
    {
      sll_queue_push(list.first, list.last, stmt);
      list.count += 1;
    }
  }
  expect(p, '}');

  Stmt_Array stmts = {0};
  if (list.count > 0)
  {
    stmts.count = list.count;
    stmts.v = push_array_nz(p->arena, Stmt *, stmts.count);

    u32 i = 0;
    for each_node(it, Stmt, list.first)
    {
      stmts.v[i++] = it;
    }
  }

  return stmt_block(p, stmts);
}

internal Stmt *
parse_stmt_if(Parser *p)
{
  Expr *cond       = parse_expr(p);
  // match(p, ';'); // allman indent hacky fix

  Stmt *then_block = parse_stmt_block(p);
  Stmt *else_stmt  = NULL;

  if (match(p, TOKEN_ELSE))
  {
    if (match(p, TOKEN_IF))
    {
      // else if -> recurse
      else_stmt = parse_stmt_if(p);
    }
    else
    {
      // else -> block
      else_stmt = parse_stmt_block(p);
    }
  }

  return stmt_if(p, cond, then_block, else_stmt);
}

internal Stmt *
parse_stmt_while(Parser *p)
{
  Expr *cond = parse_expr(p);
  Stmt *body = parse_stmt_block(p);
  return stmt_while(p, cond, body);
}

internal Stmt *
parse_stmt_do_while(Parser *p)
{
  Stmt *body = parse_stmt_block(p);
  expect(p, TOKEN_WHILE);
  Expr *cond = parse_expr(p);
  expect(p, ';');
  return stmt_while(p, cond, body);
}

internal Stmt *
parse_stmt_return(Parser *p)
{
  Expr *expr = NULL;

  // Check if this is an empty return (return; or return at end of block)
  if (!check(p, ';') && !check(p, '}'))
  {
    expr = parse_expr(p);
  }

  // Consume semicolon if present (allows both "return;" and "return" before "}")
  match(p, ';');

  return stmt_return(p, expr);
}

internal Stmt *
parse_stmt_continue(Parser *p)
{
  Stmt *s = stmt_alloc(p, STMT_CONTINUE);
  expect(p, ';');
  return s;
}

internal Stmt *
parse_stmt_break(Parser *p)
{
  Stmt *s = stmt_alloc(p, STMT_BREAK);
  expect(p, ';');
  return s;
}

internal Stmt *
parse_stmt_defer(Parser *p)
{
  Stmt *stmt = parse_stmt(p);
  switch (stmt->kind)
  {
  case STMT_RETURN:
    report_error(p, "Cannot defer a return statement");
    break;
  case STMT_DEFER:
    report_error(p, "Cannot defer a defer statement");
    break;
  default:
    break;
  }
  // expect(p, ';');
  return stmt_defer(p, stmt);
}

internal Stmt *
parse_stmt_for(Parser *p)
{
  Stmt *stmt = NULL;

  if (check(p, TOKEN_IDENT) && peek(p, TOKEN_IN))
  {
    //- parse for-in loop
    // for p in particles {}
    // for i in 0 ..< 10 {}
    // for i in 0 ..= 5*2 {}
    Expr *item = parse_expr(p);
    expect(p, TOKEN_IN);
    Expr *iter = parse_expr(p);
    Stmt *body = parse_stmt_block(p);
    stmt = stmt_for_in(p, item, iter, body);
  }
  else
  {
    //- parse c-style for loop
    // for i := 0; i < 10; i += 1 {}
    // for ; i < 10; i += 1 {}
    Stmt *init = NULL;
    Expr *cond = NULL;
    Stmt *loop = NULL;

    if (!check(p, ';'))
    {
      init = parse_stmt(p);
    }
    // @CLEANUP
    // TODO: because parse_stmt expects & consumes ; we cant expect it here also
    // expect(p, ';');
    if (match(p, ';')) {}

    if (!check(p, ';'))
    {
      cond = parse_expr(p);
    }
    expect(p, ';');

    loop = parse_stmt_expr_nosemi(p);
    Stmt *body = parse_stmt_block(p);

    // TODO: for now we need an additional semicolon after loop statement @CLEANUP

    stmt = stmt_for(p, init, cond, loop, body);
  }
  return stmt;
}

internal Switch_Case
parse_switch_case(Parser *p)
{
  Switch_Case result = {0};
  result.is_default = true;

  expect(p, TOKEN_CASE);

  // parse optional labels
  if (!check(p, ':'))
  {
    result.is_default = false;

    Expr_List list = {0};
    do
    {
      Expr *label = parse_expr(p);
      sll_queue_push(list.first, list.last, label);
      list.count += 1;
    } while (match(p, ','));

    assert(list.count > 0); // this should never assert
    result.labels.count = list.count;
    result.labels.v = push_array_nz(p->arena, Expr *, list.count);

    u32 i = 0;
    for each_node(it, Expr, list.first)
    {
      result.labels.v[i++] = it;
    }
  }

  expect(p, ':');

  Stmt_List list = {0};

  //- parse statements until next case or '}'
  while (!check(p, TOKEN_CASE) && !check(p, '}'))
  {
    if (match(p, TOKEN_FALLTHROUGH))
    {
      expect(p, ';');
      result.is_fallthrough = true;
      break; // must be last statement
    }

    Stmt *stmt = parse_stmt(p);
    sll_queue_push(list.first, list.last, stmt);
    list.count += 1;
  }

  Stmt_Array stmts = {0};
  if (list.count > 0)
  {
    stmts.count = list.count;
    stmts.v = push_array_nz(p->arena, Stmt *, list.count);

    u32 i = 0;
    for each_node(it, Stmt, list.first)
    {
      stmts.v[i++] = it;
    }
  }

  result.block = stmt_block(p, stmts);
  return result;
}

internal Stmt *
parse_stmt_switch(Parser *p)
{
  Arena_Temp scratch = arena_scratch_get(0, 0);

  Expr *expr = parse_expr(p);
  Switch_Case_List list = {0};

  expect(p, '{');

  while (!check(p, '}') && !check(p, TOKEN_EOF))
  {
    Switch_Case_Node *node = push_struct(scratch.arena, Switch_Case_Node);
    node->v = parse_switch_case(p);
    sll_queue_push(list.first, list.last, node);
    list.count += 1;
  }

  Switch_Case_Array cases = {0};
  if (list.count > 0)
  {
    cases.count = list.count;
    cases.v = push_array_nz(p->arena, Switch_Case, cases.count);

    u32 i = 0;
    for each_node(it, Switch_Case_Node, list.first)
    {
      cases.v[i++] = it->v;
    }
  }

  expect(p, '}');

  arena_scratch_release(scratch);
  return stmt_switch(p, expr, cases);
}

internal Stmt *
parse_stmt(Parser *p)
{
  if (match(p, ';'))
  {
    // "eats" leading semicolons
    return stmt_alloc(p, STMT_NULL);
  }
  if (check(p, '{'))            return parse_stmt_block(p);
  if (match(p, TOKEN_IF))       return parse_stmt_if(p);
  if (match(p, TOKEN_DO))       return parse_stmt_do_while(p);
  if (match(p, TOKEN_WHILE))    return parse_stmt_while(p);
  if (match(p, TOKEN_DEFER))    return parse_stmt_defer(p);
  if (match(p, TOKEN_RETURN))   return parse_stmt_return(p);
  if (match(p, TOKEN_CONTINUE)) return parse_stmt_continue(p);
  if (match(p, TOKEN_BREAK))    return parse_stmt_break(p);
  if (check(p, TOKEN_IDENT) && peek(p, ':')) return stmt_decl(p, parse_decl(p));
  if (match(p, TOKEN_FOR))      return parse_stmt_for(p);
  if (match(p, TOKEN_SWITCH))   return parse_stmt_switch(p);

  return parse_stmt_expr(p);
}

internal Stmt_List
parse_statements(Parser *p)
{
  Stmt_List list = {0};

  while (lexer_can_peek(p->lexer))
  {
    Stmt *stmt = parse_stmt(p);
    if (stmt->kind != STMT_NULL)
    {
      sll_queue_push(list.first, list.last, stmt);
    }
  }

  return list;
}

/////////////////////////////////////////////////////
// DECLARATIONS

internal String8
parse_type_name(Parser *p)
{
  switch (p->curr.kind)
  {
  // builtin types
  case TOKEN_S8:
  case TOKEN_S16:
  case TOKEN_S32:
  case TOKEN_S64:
  case TOKEN_U8:
  case TOKEN_U16:
  case TOKEN_U32:
  case TOKEN_U64:
  case TOKEN_UINTPTR:
  case TOKEN_INT:
  case TOKEN_UINT:
  case TOKEN_F32:
  case TOKEN_F64:
  case TOKEN_BOOL:
  case TOKEN_STRING:
  // user-defined types
  case TOKEN_IDENT:
    String8 name = p->curr.lexeme;
    advance(p);
    return str8_copy(p->arena, name);
  }

  // assert(!"Expected type name");
  report_error(p, "Expected type name");
  return (String8){0};
}

internal Type_Spec *
parse_type_prefix(Parser *p)
{
  if (match(p, '*'))
  {
    Type_Spec *t = type_spec_alloc(p, TYPE_SPEC_PTR);
    t->ptr.pointee = parse_type_prefix(p);
    return t;
  }
  if (match(p, '['))
  {
    if (!match(p, ']'))
    {
      Type_Spec *t = type_spec_alloc(p, TYPE_SPEC_ARRAY);
      t->array.count = parse_expr(p);
      expect(p, ']');
      t->array.elem  = parse_type_prefix(p);
      return t;
    }
    else
    {
      Type_Spec *t = type_spec_alloc(p, TYPE_SPEC_SLICE);
      t->slice.elem = parse_type_prefix(p);
      return t;
    }
  }

  // named type
  Type_Spec *t = type_spec_alloc(p, TYPE_SPEC_NAME);
  t->name = parse_type_name(p);
  return t;
}

internal Decl *parse_decl_nosemi(Parser *p);

internal Type_Spec *
parse_type_proc(Parser *p)
{
  Decl_List list = {0};
  u64 param_count = 0;

  expect(p, '(');

  while (!match(p, ')'))
  {
    // need to call parse_decl
    // a := 5
    // a: int
    // Type_Spec *param = parse_type(p);
    Decl *param = parse_decl_nosemi(p);
    sll_queue_push(list.first, list.last, param);
    param_count += 1;
    match(p, ',');
  }

  Type_Spec *ret = NULL;
  if (match(p, TOKEN_ARROW))
  {
    ret = parse_type(p);
  }

  Stmt *body = NULL;
  if (check(p, '{'))
  {
    body = parse_stmt_block(p);
  }

  Decl_Array params = {0};
  if (param_count > 0)
  {
    params.count = param_count;
    params.v = push_array(p->arena, Decl *, params.count);

    u32 i = 0;
    for each_node(it, Decl, list.first)
    {
      params.v[i++] = it;
    }
  }

  return type_spec_proc(p, params, ret, body);
}

internal Type_Spec *
parse_type_aggr(Parser *p, Type_Spec_Kind kind)
{
  assert(kind == TYPE_SPEC_STRUCT || kind == TYPE_SPEC_UNION);
  expect(p, '{');

  Aggr_Field_List list = {0};
  u64 count = 0;
  while (!check(p, '}'))
  {
    Aggr_Field_Node *node = push_struct(p->arena, Aggr_Field_Node);
    while (true)
    {
      String8 field_name = parse_ident(p);
      str8_list_push(p->arena, &node->v.names, field_name);

      if (match(p, ',')) continue;
      if (match(p, ':')) break;

      // if (!match(p, ':'))
      // {
      //   break;
      // }

      report_error(p, "Expected ',' or ':' after field name");
    }

    node->v.type = parse_type(p);
    sll_queue_push(list.first, list.last, node);
    list.count += 1;
    count += 1;

    if (match(p, ','))
    {
      continue;
    }
  }

  expect(p, '}');

  Aggr_Field_Array fields = {0};
  if (count > 0)
  {
    fields.count = count;
    fields.v = push_array_nz(p->arena, Aggr_Field, list.count);

    u32 i = 0;
    for each_node(it, Aggr_Field_Node, list.first)
    {
      fields.v[i++] = it->v;
    }
  }

  return type_spec_aggr(p, kind, fields);
}

internal Type_Spec *
parse_type_enum(Parser *p)
{
  expect(p, '{');

  Enum_Member_List list = {0};
  u64 count = 0;

  while (!check(p, '}'))
  {
    Enum_Member_Node *node = push_struct(p->arena, Enum_Member_Node);

    node->v.name = parse_ident(p);
    if (match(p, '='))
    {
      node->v.value = parse_expr(p);
    }

    sll_queue_push(list.first, list.last, node);
    count += 1;

    if (!match(p, ','))
    {
      break;
    }
  }

  expect(p, '}');

  Enum_Member_Array members = {0};
  if (count > 0)
  {
    members.count = count;
    members.v = push_array_nz(p->arena, Enum_Member, count);

    u32 i = 0;
    for each_node(it, Enum_Member_Node, list.first)
    {
      members.v[i++] = it->v;
    }
  }

  return type_spec_enum(p, members);
}

internal Type_Spec *
parse_type_base(Parser *p)
{
  // 1. identifiers and built-in types
  if (check(p, TOKEN_IDENT) || (p->curr.kind >= TOKEN_S8 && p->curr.kind <= TOKEN_STRING))
  {
    return type_spec_name(p, parse_type_name(p));
  }
  // 2. proc(args) -> ret
  else if (match(p, TOKEN_PROC))
  {
    return parse_type_proc(p);
  }
  // 3. struct { ... }
  else if (match(p, TOKEN_STRUCT))
  {
    return parse_type_aggr(p, TYPE_SPEC_STRUCT);
  }
  // 4. union { ... }
  else if (match(p, TOKEN_UNION))
  {
    return parse_type_aggr(p, TYPE_SPEC_UNION);
  }
  // 5. enum { ... }
  else if (match(p, TOKEN_ENUM))
  {
    return parse_type_enum(p);
  }
  // 6. grouping: (int)
  else if (match(p, '('))
  {
    Type_Spec *t = parse_type(p);
    expect(p, ')');
    return t;
  }
  Arena_Temp scratch = arena_scratch_get(0, 0);
  report_error(p, "Unexpected token %.*s in type", str8_varg(str_from_token_kind(scratch.arena, p->curr.kind)));
  arena_scratch_release(scratch);
  return NULL;
}

internal Type_Spec *
parse_type(Parser *p)
{
  if (match(p, '*'))
  {
    Type_Spec *t = type_spec_alloc(p, TYPE_SPEC_PTR);
    t->ptr.pointee = parse_type(p); // Recursive call to handle **int
    return t;
  }
  
  if (match(p, '['))
  {
    // Check if it's a slice []int or array [N]int
    if (match(p, ']'))
    {
      Type_Spec *t = type_spec_alloc(p, TYPE_SPEC_SLICE);
      t->slice.elem = parse_type(p);
      return t;
    }
    else
    {
      Type_Spec *t = type_spec_alloc(p, TYPE_SPEC_ARRAY);
      t->array.count = parse_expr(p); // Parse the '2' in [2]
      expect(p, ']');
      t->array.elem = parse_type(p);
      return t;
    }
  }

  // Base case: Builtins or Identifiers
  return parse_type_base(p);
}

internal Decl *
parse_decl_nosemi(Parser *p)
{
  String8 name = parse_ident(p);
  expect(p, ':');

  Type_Spec *type_hint = NULL;
  Expr      *init_expr = NULL;
  Type_Spec *init_type = NULL;
  b32        is_const  = false;

  // CASE A: Inferred Type (name :: value OR name := value)
  if (check(p, ':') || check(p, '='))
  {
    if (match(p, ':')) is_const = true;
    else               match(p, '=');

    // Is the value a Type Literal (struct/enum) or a Value Expression (5+2)?
    if (is_explicit_type_start(p))
    {
      init_type = parse_type(p); // Color :: enum { RED, BLUE }
    }
    else
    {
      init_expr = parse_expr(p); // N :: 500
    }
  }
  // CASE B: Explicit Type (name : int ...)
  else
  {
    type_hint = parse_type(p);

    if (match(p, ':')) // name : type : value;
    {
      is_const = true;
      init_expr = parse_expr(p);
    }
    else if (match(p, '=')) // name : type = value;
    {
      init_expr = parse_expr(p);
    }
  }

  // Return the appropriate node
  if (is_const)
  {
    // Your decl_const should be updated to accept both init_expr and init_type
    return decl_const(p, name, type_hint, init_expr, init_type);
  }

  return decl_var(p, name, type_hint, init_expr);
}

internal Decl *
parse_decl(Parser *p)
{
  while (match(p, ';')) {};
  Decl *decl = parse_decl_nosemi(p);
  expect(p, ';');
  return decl;
}

internal Decl_List
parse_declarations(Parser *p)
{
  Decl_List list = {0};

  while (lexer_can_peek(p->lexer))
  {
    Decl *decl = parse_decl(p);
    sll_queue_push(list.first, list.last, decl);

    // // eat extra semicolons
    // while (match(p, ';')) {}
  }

  return list;
}

internal void
parser_test()
{
  printf("\n");
  printf("--- STATEMENTS\n");
  printf("\n");

  {
    // String8 source = S(
    //   "x = 5 + 3\n"
    //   "do\n"
    //   "{\n"
    //   "  a -= a\n"
    //   "} while a > b\n"
    // );

    String8 source = S(
      "x = 5 + 3\n"
      "x = -a\n"
      "x = (2 + 3) * 4\n"
      "x = a > b ? 1 : 0\n"
      "x = a && b\n"
      "x = y = z = 42\n"
      // "{\n"
      // "  a = 5\n"
      // // "  defer a = 40\n"
      // // "  defer if a < b { print(a); }\n"
      // // "  defer { b = c; }\n"
      // "  if a > b {\n"
      // "    x = a\n"
      // "  } else if a > c {\n"
      // "    x = c\n"
      // "  } else if c > a {\n"
      // "    x = a\n"
      // "  } else {\n"
      // "    x = b\n"
      // "  }\n"
      // "}\n"

      // "defer { a = 41; }\n"

      "do {\n"
      "  a -= 1\n"
      "} while (a > b)\n"

      "while !request_shutdown() {\n"
      "  dt: f32 = 1.0 / 60.0\n"
      "  tick_game(dt)\n"
      "}\n"

      // TODO: This takes the array and gives me a slice of all elements
      // or i can also do array[<lo>:<hi>]
      // "return array[:]\n"
      // "return Person.{\"Joe\", 53}\n"

      "return [10]Person.{}\n"

      "update_proc: proc(en: Entity)\n"

      "break\n"
      "continue\n"

      "x = 5\n"
      "x = y = z\n"

      "for i := 0; i < 10; i += 1 {}\n"
      "for i = 0; i < 10; i += 1 {}\n"
      "for ; i < 10; i += 1 {}\n"

      "for i in 0 ..< 10 {}\n"
      "for i in 0 ..= 9 {}\n"
      "for i in 0 ..< 2*5 {}\n"

      "people: []Person\n"
      "for person in people {}\n"

      "switch c {\n"
      // "{\n"
      "case 'a' ..= 'z', 'A' ..= 'Z':\n"
      "  print(\"letter\")\n"
      "case '_':\n"
      "  print(\"underscore\")\n"
      "case 0..=9:\n"
      "  print(\"number\")\n"
      "case:\n"
      "  print(\"default case\")\n"
      "}\n"

      "if a > b {\n"
      "  return a\n"
      "}\n"
    );

    Lexer l = lexer_init(source);
    Parser p = parser_init(&l);

    Stmt_List list = parse_statements(&p);

    for (Stmt *it = list.first; it != NULL; it = it->next)
    {
      Arena_Temp scratch = arena_scratch_get(0, 0);

      String8List list = {0};

      int indent = 0;
      print_stmt(scratch.arena, &list, &indent, it);

      String8 result = str8_list_join(scratch.arena, &list, NULL);
      printf("%.*s\n", str8_varg(result));

      arena_scratch_release(scratch);
    }
  }

  printf("\n");
  printf("--- DECLARATIONS\n");
  printf("\n");

  {
    String8 source = S(
      // "var foo = a ? a&b + c<<d + e*f == +u-v-w + *g/h(x,y) + -i%k[x] && m <= n*(p+q)/r : 0\n"
      // "var foo = a ? 1 : 0\n"
      // "var foo = a ? b + c : 0\n"
      // "var foo = a ? b + c << d : 0\n"
      // "var foo = a ? b == c : 0\n"
      // "var foo = a ? +u : 0\n"
      "foo := a ? h(x) : 0\n" // proc call
      "foo := a ? k[x] : 0\n" // indexing
      "Color :: enum {\n"
      // "{\n"
      "  Red = 3,\n"
      "  Green,\n"
      "  Blue = 0,\n"
      "}\n"
      // "\n"
      "Person :: struct\n"
      "{\n"
      "  name:  string,\n"
      "  x,y,z: f32,\n"
      "  age:   int,\n"
      "};;;;;;;;;;;;;;;;;\n" // extra semicolons should not cause errors
      "\n"
      "x := Point.{1,2}\n"
      "\n"
      "make_person :: proc(name: string, age: int) -> Person {\n"
      "  person: Person\n"
      "  person.name = name\n"
      "  person.age  = age\n"
      "  return person\n"
      // "  return\n"
      "}\n"

      // "N :: size_of(:[16]*int)\n"
      // "N :: sizeof(1+2)\n"
      "x := b == 1 ? 1+2 : 3-4\n"
      "fact :: proc(n: int) -> int { trace(\"fact\"); if n == 0 { return 1; } else { return n * fact(n-1); } }\n"
      "fact :: proc(n: int) -> int { p := 1; for i := 1; i < n; i += 1 { p *= i; } return p; }\n"
      // "foo := a ? a&b + c<<d + e*f == +u-v-w + *g/h(x,y) + -i%k[x] && m <= n*(p+q)/r : 0\n"
      // "f :: proc(x: int) -> bool { switch x { case 0: case 1: return true; case 2: default: return false; } }\n"
      "Color :: enum { RED = 3, GREEN, BLUE = 0 }\n"
      // "PI :: 3.14\n"
      "Vector :: struct { x, y: f32, }\n"
      "v := Vector.{1.0, -1.0}\n"
      // "v: Vector = {1.0. -1.0}\n"
      "Int_Or_Float :: union { i: int, f: f32, }\n"
      "Vectors :: [1+2]Vector\n"
      "f :: proc() { do { print(42); } while (1); }\n"
      // "T :: [16]proc(int, f32, string, Vector, string, f32, int, uint) -> int\n"
      "f :: proc() { E :: enum { A, B, C, }; return; }\n"
      "f :: proc() { if (1) { return 1; } else if (2) { return 2; } else { return 3; } }\n"

      "Int_Or_Ptr :: union { i: int, p: *int, }\n"

      "Vector :: struct { x, y: int, }\n"
      "v: Vector = 0 ? .{1,2} : .{3,4}\n"
    );
    /*

    (proc (make_person (name string, age int) Person)
      (var (person Person) value)
      (return person)
    )
    
    */

    Lexer l = lexer_init(source);
    Parser p = parser_init(&l);

    Decl_List list = parse_declarations(&p);

    for each_node(it, Decl, list.first)
    {
      Arena_Temp scratch = arena_scratch_get(0, 0);

      String8List list = {0};

      int a = 5;
      // char buf[128];
      // usize buf_size = sizeof(buf);
      // usize n = print_stmt(buf, buf_size, it);
      int indent = 0;
      print_decl(scratch.arena, &list, &indent, it);

      String8 result = str8_list_join(scratch.arena, &list, NULL);
      printf("%.*s\n", str8_varg(result));

      // printf("%.*s\n", (int)n, buf);
      arena_scratch_release(scratch);
    }
  }

  int a = 5;
}
