#include "parser.h"
#include "base.h"
#include "lexer.h"
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

//
// TODO:
// [ ] Instead of having a discriminated union for Expr, Stmt, Decl; do this?:
// - ref: https://github.com/graphitemaster/codin/blob/main/src/tree.h#L237
//
// struct Expr {
//   Expr_Kind kind;
// };
//
// struct Unary_Expr {
//   Expr base;
//   Operator_Kind operator;
//   Expr *operand;
// }
//

internal Token
advance(Parser *p);

internal TypeSpec *
parse_type(Parser *p);

function Parser
parser_init(Lexer *l) {
  Parser p = {0};
  p.arena = arena_alloc(GB(1), MB(1), 0);
  p.lexer = l;
  // advance(&p);

  p.curr = lexer_next(l);
  p.next = lexer_next(l);

  return p;
}

internal void
push_compound_field(Parser *p, CompoundFieldList *list, CompoundField *field) {
  queue_push(list->first, list->last, field);
  list->count += 1;
}

// TODO: `parser_fini` destroy arena

internal Expr *
parse_expr(Parser *p);

internal void
report_error(Parser *p, const char *fmt, ...) {
  // TODO(#8): duplicate of lexer_syntax_error
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
  u64 highlight_len = (p->curr.pos.length > 0) ? p->curr.pos.length : 1;
  printf("%*s" CLR_GRN, (int)col, "");
  for (u64 i = 0; i < highlight_len; i++) printf("^");
  printf(" here\n" CLR_RESET);

  va_end(args);

  trap();

  // TODO: Make some errors non fatal
  // getchar();
  // os_exit(1);
}

internal Token
advance(Parser *p) {
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
check(Parser *p, TokenKind kind) {
  // lexer_can_peek(p->lexer) && 
  return p->curr.kind == kind;
}

internal bool
expect(Parser *p, TokenKind kind) {
  if (p->curr.kind == kind) {
    advance(p);
    return true;
  }

  Temp scratch = arena_scratch_get(0, 0);

  String8 expected = str_from_token_kind(scratch.arena, kind);
  String8 got      = str_from_token_kind(scratch.arena, p->curr.kind);

  report_error(p, "Expected '%.*s', got '%.*s'", str8_varg(expected), str8_varg(got));

  arena_scratch_release(scratch);
  trap();
  return false;
}

internal bool
match(Parser *p, TokenKind kind) {
  if (check(p, kind)) {
    advance(p);
    return true;
  }
  return false;
}

internal String8
parse_ident(Parser *p) {
  if (expect(p, TokenKind_Ident)) {
    return str8_copy(p->arena, p->prev.lexeme);
  }
  assert(0);
  return (String8){0};
}

internal b32
peek(Parser *p, TokenKind kind) {
  return p->next.kind == kind;
}

internal bool
is_type_start(Parser *p) {
  switch (p->curr.kind) {
  case TokenKind_Ident:
  case TokenKind_String:
  case TokenKind_I8:
  case TokenKind_I16:
  case TokenKind_I32:
  case TokenKind_I64:
  case TokenKind_U8:
  case TokenKind_U16:
  case TokenKind_U32:
  case TokenKind_U64:
  case TokenKind_Uintptr:
  case TokenKind_Int:
  case TokenKind_Uint:
  case TokenKind_F32:
  case TokenKind_F64:
  case TokenKind_Bool:
  case TokenKind_Proc:
  case TokenKind_Star:
  case TokenKind_LBracket:
    return true;
  default: break;
  }
  return false;
}

// NOTE(mxtej): doesn't contain identifiers
internal bool
is_explicit_type_start(Parser *p) {
  switch (p->curr.kind) {
  // built-in type keywords
  case TokenKind_String:
  case TokenKind_I8:      case TokenKind_I16: case TokenKind_I32: case TokenKind_I64:
  case TokenKind_U8:      case TokenKind_U16: case TokenKind_U32: case TokenKind_U64:
  case TokenKind_Uintptr: case TokenKind_Int: case TokenKind_Uint:
  case TokenKind_F32:     case TokenKind_F64: case TokenKind_Bool:

  // type literal keywords
  case TokenKind_Proc: case TokenKind_Struct: case TokenKind_Union: case TokenKind_Enum:

  // type operators
  case TokenKind_Star:
  case TokenKind_LBracket:
    return true;
  default:
    break;
  }
  return false;
}

internal TypeSpec *parse_type_base(Parser *p);
internal Expr *parse_compound_or_expr(Parser *p);

internal CompoundField *
parse_compound_field(Parser *p) {
  CompoundField *field = push_struct(p->arena, CompoundField);

  if (match(p, TokenKind_LBracket)) {
    // [index] = value
    field->kind = CompoundFieldKind_Index;
    field->index = parse_expr(p);
    expect(p, TokenKind_RBracket);
    expect(p, TokenKind_Equal);
    field->init = parse_expr(p); // parse_compound_or_expr
  } else if (check(p, TokenKind_Ident) && peek(p, TokenKind_Equal)) {
    // name = value
    field->kind = CompoundFieldKind_Name;
    field->name = parse_ident(p);
    advance(p); // Consume TokenKind_Equal
    field->init = parse_expr(p); // parse_compound_or_expr
  } else {
    // value
    field->kind = CompoundFieldKind_None;
    field->init = parse_expr(p); // parse_compound_or_expr
  }

  return field;
}

internal Expr *
parse_compound_literal(Parser *p, TypeSpec *explicit_type) {
  expect(p, TokenKind_LBrace);

  // TODO(#9): use Compound_Field_Node and arena scratch for list; also in other places
  CompoundFieldList list = {0};

  if (!match(p, TokenKind_RBrace)) {
    do {
      SourcePos pos = p->curr.pos;
      CompoundField *field = parse_compound_field(p);
      push_compound_field(p, &list, field);
      field->pos = pos;
    } while (match(p, TokenKind_Comma) && !check(p, TokenKind_RBrace));

    expect(p, TokenKind_RBrace);
  }

  CompoundFieldArray fields = {0};
  if (list.count > 0) {
    fields.count = list.count;
    fields.v = push_array_nz(p->arena, CompoundField *, fields.count);

    u32 i = 0;
    for each_node(it, CompoundField, list.first) {
      fields.v[i++] = it;
    }
  }

  return expr_compound(p, explicit_type, fields);
}

internal b32
looks_like_dotted_compound(Parser *p) {
  Temp scratch = arena_scratch_get(0, 0);

  // Temporary clone of parser and lexer
  Parser tmp = *p;
  Lexer temp_lexer = *p->lexer;
  tmp.arena = scratch.arena;
  tmp.lexer = &temp_lexer;

  TypeSpec *type = parse_type(&tmp);
  b32 result = (type != NULL && check(&tmp, TokenKind_Dot) && peek(&tmp, TokenKind_LBrace));

  arena_scratch_release(scratch);
  return result;
}

internal Expr *
parse_expr_primary(Parser *p) {
  // implicit compound literal
  if (check(p, TokenKind_Dot) && peek(p, TokenKind_LBrace)) {
    advance(p); // eat dot
    return parse_compound_literal(p, NULL);
  }
  // explicit compound literal
  if (is_type_start(p) && looks_like_dotted_compound(p)) {
    TypeSpec *type = parse_type(p);
    expect(p, TokenKind_Dot);
    return parse_compound_literal(p, type);
  }

  if (match(p, TokenKind_True)) {
    return expr_bool_lit(p, true);
  }
  if (match(p, TokenKind_False)) {
    return expr_bool_lit(p, false);
  }
  if (match(p, TokenKind_Nil)) {
    return expr_nil_lit(p);
  }
  if (match(p, TokenKind_StringLiteral)) {
    return expr_string_lit(p, p->prev.value.string);
  }
  if (match(p, TokenKind_IntegerLiteral)) {
    return expr_integer_lit(p, p->prev.value.integer);
  }
  if (match(p, TokenKind_FloatLiteral)) {
    return expr_float_lit(p, p->prev.value.floating);
  }
  if (match(p, TokenKind_CharLiteral)) {
    return expr_char_lit(p, p->prev.value.character);
  }
  if (match(p, TokenKind_Ident)) {
    return expr_ident(p, p->prev);
  }
  if (match(p, TokenKind_LParen)) {
    Expr *expr = parse_expr(p);
    expect(p, TokenKind_RParen);
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
parse_expr_postfix(Parser *p) {
  Expr *expr = parse_expr_primary(p);

  while (true) {
    // CALL: foo(...)
    if (match(p, TokenKind_LParen)) {
      ExprList args = {0};
      u64 count = 0;

      if (!match(p, TokenKind_RParen)) {
        do {
          Expr *arg = parse_expr(p);
          queue_push(args.first, args.last, arg);
          count += 1;
        } while (match(p, TokenKind_Comma));

        expect(p, TokenKind_RParen);
      }

      ExprArray arg_array = {0};
      if (count > 0) {
        arg_array.count = count;
        arg_array.v = push_array(p->arena, Expr*, count);

        u32 i = 0;
        for each_node(it, Expr, args.first) {
          arg_array.v[i++] = it;
        }
      }

      expr = expr_call(p, expr, arg_array);
      continue;
    }

    // INDEX: a[b]
    if (match(p, TokenKind_LBracket)) {
      Expr *index = parse_expr(p);
      expect(p, TokenKind_RBracket);
      expr = expr_index(p, expr, index);
      continue;
    }

    // DEREF: ptr.*
    if (match(p, TokenKind_Deref)) {
      Token op = p->prev;
      expr = expr_unary(p, op, expr);
      continue;
    }

    // FIELD: person.name
    // TODO(#10): rename this to selector and also parse
    // .Enumerator
    if (match(p, TokenKind_Dot)) {
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
parse_expr_unary(Parser *p) {
  if (match(p, TokenKind_Cast)) {
    // cast(f32)a
    expect(p, TokenKind_LParen);
    TypeSpec *type = parse_type(p);
    expect(p, TokenKind_RParen);
    Expr *expr = parse_expr_unary(p);
    return expr_cast(p, type, expr);
  }

  if (match(p, TokenKind_SizeOf)) {
    Expr *expr = NULL;

    expect(p, TokenKind_LParen);
    
    // Distinguish between type and expression:
    // - Explicit types: int, *int, [3]int, struct{}, proc(), etc.
    // - Expressions: identifiers (could be vars or type names), literals, operators, etc.
    // We use is_explicit_type_start to avoid ambiguity with identifiers.
    if (is_explicit_type_start(p)) {
      // size_of(int), size_of(*int), size_of([3]int)
      TypeSpec *type = parse_type(p);
      expect(p, TokenKind_RParen);
      expr = expr_size_of_type(p, type);
    } else {
      // size_of(x), size_of(Vector), size_of(1+2)
      // Identifiers are parsed as expressions - resolver handles type names
      Expr *e = parse_expr(p);
      expect(p, TokenKind_RParen);
      expr = expr_size_of_expr(p, e);
    }
    return expr;
  }

  // prefix operators
  // TODO add?: increment, decrement
  while (
    match(p, TokenKind_Exclamation) ||
    match(p, TokenKind_Minus)       ||
    match(p, TokenKind_Plus)        ||
    match(p, TokenKind_Tilde)       ||
    match(p, TokenKind_Ampersand)
  ) {
    Token op = p->prev;
    Expr *right = parse_expr_unary(p);
    return expr_unary(p, op, right);
  }
  return parse_expr_postfix(p);
  // return parse_expr_primary(p);
}

internal Expr *
parse_expr_factor(Parser *p) {
  Expr *expr = parse_expr_unary(p);

  // TODO(#11): %  - modulo (truncated)  - integers
  // TODO(#12): %% - remainder (floored) - integers
  while (
    match(p, TokenKind_Slash) ||
    match(p, TokenKind_Star)  ||
    match(p, TokenKind_Percent)
  ) {
    Token op = p->prev;
    Expr *right = parse_expr_unary(p);
    expr = expr_binary(p, expr, op, right);
  }

  return expr;
}

internal Expr *
parse_expr_term(Parser *p) {
  Expr *expr = parse_expr_factor(p);

  while (
    match(p, TokenKind_Minus) ||
    match(p, TokenKind_Plus)
  ) {
    Token op = p->prev;
    Expr *right = parse_expr_factor(p);
    expr = expr_binary(p, expr, op, right);
  }

  return expr;
}

internal Expr *
parse_expr_shift(Parser *p) {
  Expr *expr = parse_expr_term(p);

  while (
    match(p, TokenKind_LShift) ||
    match(p, TokenKind_RShift)
  ) {
    Token op = p->prev;
    Expr *right = parse_expr_term(p);
    expr = expr_binary(p, expr, op, right);
  }

  return expr;
}

internal Expr *
parse_expr_bitwise_and(Parser *p) {
  Expr *expr = parse_expr_shift(p);

  while (match(p, TokenKind_Ampersand)) {
    Token op = p->prev;
    Expr *right = parse_expr_shift(p);
    expr = expr_binary(p, expr, op, right);
  }

  return expr;
}

internal Expr *
parse_expr_bitwise_xor(Parser *p) {
  Expr *expr = parse_expr_bitwise_and(p);

  while (match(p, TokenKind_Tilde)) {
    Token op = p->prev;
    Expr *right = parse_expr_bitwise_and(p);
    expr = expr_binary(p, expr, op, right);
  }

  return expr;
}

internal Expr *
parse_expr_bitwise_or(Parser *p) {
  Expr *expr = parse_expr_bitwise_xor(p);

  while (match(p, TokenKind_Pipe)) {
    Token op = p->prev;
    Expr *right = parse_expr_bitwise_xor(p);
    expr = expr_binary(p, expr, op, right);
  }

  return expr;
}

internal Expr *
parse_expr_comparison(Parser *p) {
  Expr *expr = parse_expr_bitwise_or(p);

  while (
    match(p, TokenKind_CmpGt)    ||
    match(p, TokenKind_CmpGtEq)  ||
    match(p, TokenKind_CmpLt)    ||
    match(p, TokenKind_CmpLtEq)
  ) {
    Token op = p->prev;
    Expr *right = parse_expr_bitwise_or(p);
    expr = expr_binary(p, expr, op, right);
  }

  return expr;
}

internal Expr *
parse_expr_equality(Parser *p) {
  Expr *expr = parse_expr_comparison(p);

  while (match(p, TokenKind_CmpNeq) || match(p, TokenKind_CmpEq)) {
    Token op = p->prev;
    Expr *right = parse_expr_comparison(p);
    expr = expr_binary(p, expr, op, right);
  }

  return expr;
}

internal Expr *
parse_expr_range(Parser *p) {
  Expr *expr = parse_expr_equality(p);

  while (match(p, TokenKind_RangeExcl) || match(p, TokenKind_RangeIncl)) {
    Token op = p->prev;
    Expr *right = parse_expr_equality(p);
    expr = expr_binary(p, expr, op, right);
  }

  return expr;
}

internal Expr *
parse_expr_logical_and(Parser *p) {
  Expr *expr = parse_expr_range(p);

  while (match(p, TokenKind_LogicalAnd)) {
    Token op = p->prev;
    Expr *right = parse_expr_range(p);
    expr = expr_binary(p, expr, op, right);
  }

  return expr;
}

internal Expr *
parse_expr_logical_or(Parser *p) {
  Expr *expr = parse_expr_logical_and(p);

  while (match(p, TokenKind_LogicalOr)) {
    Token op = p->prev;
    Expr *right = parse_expr_logical_and(p);
    expr = expr_binary(p, expr, op, right);
  }

  return expr;
}

internal Expr *
parse_expr_ternary(Parser *p) {
  Expr *expr = parse_expr_logical_or(p);

  while (match(p, TokenKind_Question)) {
    Expr *then = parse_expr(p);
    expect(p, TokenKind_Colon);
    Expr *else0 = parse_expr_ternary(p); // RIGHT ASSOCIATIVE
    expr = expr_ternary(p, expr, then, else0);
  }

  return expr;
}

internal b32
is_assign_op(TokenKind kind) {
  switch (kind) {
  case TokenKind_Equal:
  case TokenKind_AddAssign:
  case TokenKind_SubAssign:
  case TokenKind_DivAssign:
  case TokenKind_MulAssign:
  case TokenKind_AndAssign:
  // case TokenKind_ModAssign:
  case TokenKind_LShiftAssign:
  case TokenKind_RShiftAssign:
  case TokenKind_OrAssign:
  case TokenKind_XorAssign:
    return true;
  default:
    break;
  }
  return false;
}

internal Expr *
parse_expr_assignment(Parser *p) {
  Expr *expr = parse_expr_ternary(p);

  // use `if` if we want a = b = c (recursive)
  // use while if not
  if (is_assign_op(p->curr.kind)) {
    advance(p);

    Token op = p->prev;
    Expr *right = parse_expr_assignment(p); // RIGHT ASSOCIATIVE

    // semantic check later: left must be assignable
    expr = expr_binary(p, expr, op, right);
  }

  return expr;
}

internal Expr *
parse_expr(Parser *p) {
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
parse_stmt_expr_nosemi(Parser *p) {
  Expr *expr = parse_expr(p);
  return stmt_expr(p, expr);
}

internal Stmt *
parse_stmt_expr(Parser *p) {
  Expr *expr = parse_expr(p);
  expect(p, TokenKind_Semicolon);
  return stmt_expr(p, expr);
}

internal Stmt *
parse_stmt_block(Parser *p) {
  expect(p, TokenKind_LBrace);
  StmtList list = {0};
  while (!check(p, TokenKind_RBrace)) {
    Stmt *stmt = parse_stmt(p);
    if (stmt->kind != StmtKind_Null) {
      queue_push(list.first, list.last, stmt);
      list.count += 1;
    }
  }
  expect(p, TokenKind_RBrace);

  StmtArray stmts = {0};
  if (list.count > 0) {
    stmts.count = list.count;
    stmts.v = push_array_nz(p->arena, Stmt *, stmts.count);

    u32 i = 0;
    for each_node(it, Stmt, list.first) {
      stmts.v[i++] = it;
    }
  }

  return stmt_block(p, stmts);
}

internal Stmt *
parse_stmt_if(Parser *p) {
  Expr *cond       = parse_expr(p);
  // match(p, TokenKind_Colon); // allman indent hacky fix

  Stmt *then_block = parse_stmt_block(p);
  Stmt *else_stmt  = NULL;

  if (match(p, TokenKind_Else)) {
    if (match(p, TokenKind_If)) {
      // else if -> recurse
      else_stmt = parse_stmt_if(p);
    } else {
      // else -> block
      else_stmt = parse_stmt_block(p);
    }
  }

  return stmt_if(p, cond, then_block, else_stmt);
}

internal Stmt *
parse_stmt_while(Parser *p) {
  Expr *cond = parse_expr(p);
  Stmt *body = parse_stmt_block(p);
  return stmt_while(p, cond, body);
}

internal Stmt *
parse_stmt_do_while(Parser *p) {
  Stmt *body = parse_stmt_block(p);
  expect(p, TokenKind_While);
  Expr *cond = parse_expr(p);
  expect(p, TokenKind_Semicolon);
  return stmt_do_while(p, cond, body);
}

internal Stmt *
parse_stmt_return(Parser *p) {
  Expr *expr = NULL;

  // Check if this is an empty return (return; or return at end of block)
  if (!check(p, TokenKind_Semicolon) && !check(p, TokenKind_RBrace)) {
    expr = parse_expr(p);
  }

  // Consume semicolon if present (allows both "return;" and "return" before "}")
  match(p, TokenKind_Semicolon);

  return stmt_return(p, expr);
}

internal Stmt *
parse_stmt_continue(Parser *p) {
  Stmt *s = stmt_alloc(p, StmtKind_Continue);
  expect(p, TokenKind_Semicolon);
  return s;
}

internal Stmt *
parse_stmt_break(Parser *p) {
  Stmt *s = stmt_alloc(p, StmtKind_Break);
  expect(p, TokenKind_Semicolon);
  return s;
}

internal Stmt *
parse_stmt_defer(Parser *p) {
  Stmt *stmt = parse_stmt(p);
  switch (stmt->kind) {
  case StmtKind_Return:
    report_error(p, "Cannot defer a return statement");
    break;
  case StmtKind_Defer:
    report_error(p, "Cannot defer a defer statement");
    break;
  default:
    break;
  }
  // expect(p, TokenKind_Semicolon);
  return stmt_defer(p, stmt);
}

internal Stmt *
parse_stmt_for(Parser *p) {
  Stmt *stmt = NULL;

  if (check(p, TokenKind_Ident) && peek(p, TokenKind_In)) {
    //- parse for-in loop
    // for p in particles {}
    // for i in 0 ..< 10 {}
    // for i in 0 ..= 5*2 {}
    Expr *item = parse_expr(p);
    expect(p, TokenKind_In);
    Expr *iter = parse_expr(p);
    Stmt *body = parse_stmt_block(p);
    stmt = stmt_for_in(p, item, iter, body);
  } else {
    //- parse c-style for loop
    // for i := 0; i < 10; i += 1 {}
    // for ; i < 10; i += 1 {}
    Stmt *init = NULL;
    Expr *cond = NULL;
    Stmt *loop = NULL;

    if (!check(p, TokenKind_Semicolon)) {
      init = parse_stmt(p);
    }
    // @CLEANUP
    // TODO: because parse_stmt expects & consumes ; we cant expect it here also
    // expect(p, TokenKind_Semicolon);
    if (match(p, TokenKind_Semicolon)) {}

    if (!check(p, TokenKind_Semicolon)) {
      cond = parse_expr(p);
    }
    expect(p, TokenKind_Semicolon);

    loop = parse_stmt_expr_nosemi(p);
    Stmt *body = parse_stmt_block(p);

    // TODO: for now we need an additional semicolon after loop statement @CLEANUP

    stmt = stmt_for(p, init, cond, loop, body);
  }
  return stmt;
}

internal SwitchCase
parse_switch_case(Parser *p) {
  SwitchCase result = {0};
  result.is_default = true;

  expect(p, TokenKind_Case);

  // parse optional labels
  if (!check(p, TokenKind_Colon)) {
    result.is_default = false;

    ExprList list = {0};
    do {
      Expr *label = parse_expr(p);
      queue_push(list.first, list.last, label);
      list.count += 1;
    } while (match(p, TokenKind_Comma));

    assert(list.count > 0); // this should never assert
    result.labels.count = list.count;
    result.labels.v = push_array_nz(p->arena, Expr *, list.count);

    u32 i = 0;
    for each_node(it, Expr, list.first) {
      result.labels.v[i++] = it;
    }
  }

  expect(p, TokenKind_Colon);

  StmtList list = {0};

  //- parse statements until next case or TokenKind_RBrace
  while (!check(p, TokenKind_Case) && !check(p, TokenKind_RBrace)) {
    if (match(p, TokenKind_Fallthrough)) {
      expect(p, TokenKind_Semicolon);
      result.is_fallthrough = true;
      break; // must be last statement
    }

    Stmt *stmt = parse_stmt(p);
    queue_push(list.first, list.last, stmt);
    list.count += 1;
  }

  StmtArray stmts = {0};
  if (list.count > 0) {
    stmts.count = list.count;
    stmts.v = push_array_nz(p->arena, Stmt *, list.count);

    u32 i = 0;
    for each_node(it, Stmt, list.first) {
      stmts.v[i++] = it;
    }
  }

  result.block = stmt_block(p, stmts);
  return result;
}

internal Stmt *
parse_stmt_switch(Parser *p) {
  Temp scratch = arena_scratch_get(0, 0);

  Expr *expr = parse_expr(p);
  SwitchCaseList list = {0};

  expect(p, TokenKind_LBrace);

  while (!check(p, TokenKind_RBrace) && !check(p, TokenKind_EOF)) {
    SwitchCaseNode *node = push_struct(scratch.arena, SwitchCaseNode);
    node->v = parse_switch_case(p);
    queue_push(list.first, list.last, node);
    list.count += 1;
  }

  SwitchCaseArray cases = {0};
  if (list.count > 0) {
    cases.count = list.count;
    cases.v = push_array_nz(p->arena, SwitchCase, cases.count);

    u32 i = 0;
    for each_node(it, SwitchCaseNode, list.first) {
      cases.v[i++] = it->v;
    }
  }

  expect(p, TokenKind_RBrace);

  arena_scratch_release(scratch);
  return stmt_switch(p, expr, cases);
}

internal Stmt *
parse_stmt(Parser *p) {
  if (match(p, TokenKind_Semicolon)) {
    // "eats" leading semicolons
    return stmt_alloc(p, StmtKind_Null);
  }

  if (check(p, TokenKind_LBrace))   return parse_stmt_block(p);
  if (match(p, TokenKind_If))       return parse_stmt_if(p);
  if (match(p, TokenKind_Do))       return parse_stmt_do_while(p);
  if (match(p, TokenKind_While))    return parse_stmt_while(p);
  if (match(p, TokenKind_Defer))    return parse_stmt_defer(p);
  if (match(p, TokenKind_Return))   return parse_stmt_return(p);
  if (match(p, TokenKind_Continue)) return parse_stmt_continue(p);
  if (match(p, TokenKind_Break))    return parse_stmt_break(p);
  if (match(p, TokenKind_For))      return parse_stmt_for(p);
  if (match(p, TokenKind_Switch))   return parse_stmt_switch(p);

  if (check(p, TokenKind_Ident) && peek(p, TokenKind_Colon)) {
    return stmt_decl(p, parse_decl(p));
  }

  return parse_stmt_expr(p);
}

internal StmtList
parse_statements(Parser *p) {
  StmtList list = {0};

  while (lexer_can_peek(p->lexer)) {
    Stmt *stmt = parse_stmt(p);
    if (stmt->kind != StmtKind_Null) {
      queue_push(list.first, list.last, stmt);
    }
  }

  return list;
}

/////////////////////////////////////////////////////
// DECLARATIONS

internal String8
parse_type_name(Parser *p) {
  switch (p->curr.kind) {
  // builtin types
  case TokenKind_I8:
  case TokenKind_I16:
  case TokenKind_I32:
  case TokenKind_I64:
  case TokenKind_U8:
  case TokenKind_U16:
  case TokenKind_U32:
  case TokenKind_U64:
  case TokenKind_Uintptr:
  case TokenKind_Int:
  case TokenKind_Uint:
  case TokenKind_F32:
  case TokenKind_F64:
  case TokenKind_Bool:
  case TokenKind_String:
  // user-defined types
  case TokenKind_Ident:
    String8 name = p->curr.lexeme;
    advance(p);
    return str8_copy(p->arena, name);
  default:
    break;
  }

  // assert(!"Expected type name");
  report_error(p, "Expected type name");
  return (String8){0};
}

internal TypeSpec *
parse_type_prefix(Parser *p) {
  if (match(p, TokenKind_Star)) {
    TypeSpec *t = type_spec_alloc(p, TypeSpecKind_Ptr);
    t->ptr.pointee = parse_type_prefix(p);
    return t;
  }
  if (match(p, TokenKind_LBracket)) {
    if (!match(p, TokenKind_RBracket)) {
      TypeSpec *t = type_spec_alloc(p, TypeSpecKind_Array);
      t->array.count = parse_expr(p);
      expect(p, TokenKind_RBracket);
      t->array.elem  = parse_type_prefix(p);
      return t;
    } else {
      TypeSpec *t = type_spec_alloc(p, TypeSpecKind_Slice);
      t->slice.elem = parse_type_prefix(p);
      return t;
    }
  }

  // named type
  TypeSpec *t = type_spec_alloc(p, TypeSpecKind_Name);
  t->name = parse_type_name(p);
  return t;
}

internal Decl *parse_decl_nosemi(Parser *p);

internal TypeSpec *
parse_type_proc(Parser *p) {
  DeclList list = {0};
  u64 param_count = 0;

  expect(p, TokenKind_LParen);

  while (!match(p, TokenKind_RParen)) {
    // need to call parse_decl
    // a := 5
    // a: int
    // TypeSpec *param = parse_type(p);
    Decl *param = parse_decl_nosemi(p);
    queue_push(list.first, list.last, param);
    param_count += 1;
    match(p, TokenKind_Comma);
  }

  TypeSpec *ret = NULL;
  if (match(p, TokenKind_Arrow)) {
    ret = parse_type(p);
  }

  Stmt *body = NULL;
  if (check(p, TokenKind_LBrace)) {
    body = parse_stmt_block(p);
  }

  DeclArray params = {0};
  if (param_count > 0) {
    params.count = param_count;
    params.v = push_array(p->arena, Decl *, params.count);

    u32 i = 0;
    for each_node(it, Decl, list.first) {
      params.v[i++] = it;
    }
  }

  return type_spec_proc(p, params, ret, body);
}

internal TypeSpec *
parse_type_aggr(Parser *p, TypeSpecKind kind) {
  assert(kind == TypeSpecKind_Struct || kind == TypeSpecKind_Union);
  expect(p, TokenKind_LBrace);

  AggrFieldList list = {0};
  u64 count = 0;
  while (!check(p, TokenKind_RBrace)) {
    AggrFieldNode *node = push_struct(p->arena, AggrFieldNode);
    while (true) {
      String8 field_name = parse_ident(p);
      str8_list_push(p->arena, &node->v.names, field_name);

      if (match(p, TokenKind_Comma)) continue;
      if (match(p, TokenKind_Colon)) break;

      // if (!match(p, TokenKind_Colon))
      // {
      //   break;
      // }

      report_error(p, "Expected TokenKind_Comma or TokenKind_Colon after field name");
    }

    node->v.type = parse_type(p);
    queue_push(list.first, list.last, node);
    list.count += 1;
    count += 1;

    if (match(p, TokenKind_Semicolon)) {
      continue;
    }
  }

  expect(p, TokenKind_RBrace);

  AggrFieldArray fields = {0};
  if (count > 0) {
    fields.count = count;
    fields.v = push_array_nz(p->arena, AggrField, list.count);

    u32 i = 0;
    for each_node(it, AggrFieldNode, list.first) {
      fields.v[i++] = it->v;
    }
  }

  return type_spec_aggr(p, kind, fields);
}

internal TypeSpec *
parse_type_enum(Parser *p) {
  expect(p, TokenKind_LBrace);

  EnumMemberList list = {0};
  u64 count = 0;

  while (!check(p, TokenKind_RBrace)) {
    EnumMemberNode *node = push_struct(p->arena, EnumMemberNode);

    node->v.name = parse_ident(p);
    if (match(p, TokenKind_Equal)) {
      node->v.value = parse_expr(p);
    }

    queue_push(list.first, list.last, node);
    count += 1;

    if (!match(p, TokenKind_Comma)) {
      break;
    }
  }

  expect(p, TokenKind_RBrace);

  EnumMemberArray members = {0};
  if (count > 0) {
    members.count = count;
    members.v = push_array_nz(p->arena, EnumMember, count);

    u32 i = 0;
    for each_node(it, EnumMemberNode, list.first) {
      members.v[i++] = it->v;
    }
  }

  return type_spec_enum(p, members);
}

internal TypeSpec *
parse_type_base(Parser *p) {
  if (check(p, TokenKind_Ident) || (p->curr.kind >= TokenKind_I8 && p->curr.kind <= TokenKind_String)) {
    // 1. identifiers and built-in types
    return type_spec_name(p, parse_type_name(p));
  } else if (match(p, TokenKind_Proc)) {
    // 2. proc(args) -> ret
    return parse_type_proc(p);
  } else if (match(p, TokenKind_Struct)) {
    // 3. struct { ... }
    return parse_type_aggr(p, TypeSpecKind_Struct);
  } else if (match(p, TokenKind_Union)) {
    // 4. union { ... }
    return parse_type_aggr(p, TypeSpecKind_Union);
  } else if (match(p, TokenKind_Enum)) {
    // 5. enum { ... }
    return parse_type_enum(p);
  } else if (match(p, TokenKind_LParen)) {
    // 6. grouping: (int)
    TypeSpec *t = parse_type(p);
    expect(p, TokenKind_RParen);
    return t;
  }
  Temp scratch = arena_scratch_get(0, 0);
  report_error(p, "Unexpected token %.*s in type", str8_varg(str_from_token_kind(scratch.arena, p->curr.kind)));
  arena_scratch_release(scratch);
  return NULL;
}

internal TypeSpec *
parse_type(Parser *p) {
  if (match(p, TokenKind_Star)) {
    TypeSpec *t = type_spec_alloc(p, TypeSpecKind_Ptr);
    t->ptr.pointee = parse_type(p); // Recursive call to handle **int
    return t;
  }
  
  if (match(p, TokenKind_LBracket)) {
    // Check if it's a slice []int or array [N]int
    if (match(p, TokenKind_RBracket)) {
      TypeSpec *t = type_spec_alloc(p, TypeSpecKind_Slice);
      t->slice.elem = parse_type(p);
      return t;
    } else {
      TypeSpec *t = type_spec_alloc(p, TypeSpecKind_Array);
      t->array.count = parse_expr(p); // Parse the '2' in [2]
      expect(p, TokenKind_RBracket);
      t->array.elem = parse_type(p);
      return t;
    }
  }

  // Base case: Builtins or Identifiers
  return parse_type_base(p);
}

internal Decl *
parse_decl_nosemi(Parser *p) {
  b32 is_foreign = false;
  if (match(p, TokenKind_Foreign)) {
    is_foreign = true;
  }

  String8 name = parse_ident(p);
  expect(p, TokenKind_Colon);

  TypeSpec *type_hint = NULL;
  Expr      *init_expr = NULL;
  TypeSpec *init_type = NULL;
  b32        is_const  = false;

  // CASE A: Inferred Type (name :: value OR name := value)
  if (check(p, TokenKind_Colon) || check(p, TokenKind_Equal)) {
    if (match(p, TokenKind_Colon)) is_const = true;
    else                           match(p, TokenKind_Equal);

    // Is the value a Type Literal (struct/enum) or a Value Expression (5+2)?
    // But check if it's a compound literal first (e.g., [3]int.{1,2,3})
    if (is_explicit_type_start(p) && !looks_like_dotted_compound(p)) {
      init_type = parse_type(p); // Color :: enum { RED, BLUE }
    } else {
      init_expr = parse_expr(p); // N :: 500 or a := [3]int.{1,2,3}
    }
  } else {
    // CASE B: Explicit Type (name : int ...)
    type_hint = parse_type(p);

    if (match(p, TokenKind_Colon)) {
      // name : type : value;
      is_const = true;
      init_expr = parse_expr(p);
    } else if (match(p, TokenKind_Equal)) {
      // name : type = value;
      init_expr = parse_expr(p);
    }
  }

  Decl *decl = NULL;

  if (is_const) {
    decl = decl_const(p, name, type_hint, init_expr, init_type);
  } else {
    decl = decl_var(p, name, type_hint, init_expr);
  }

  // TODO: foreign stuff
  decl->is_foreign = is_foreign;
  return decl;
}

internal Decl *
parse_decl(Parser *p) {
  while (match(p, TokenKind_Semicolon)) {};
  Decl *decl = parse_decl_nosemi(p);
  expect(p, TokenKind_Semicolon);
  return decl;
}

internal DeclList
parse_declarations(Parser *p) {
  DeclList list = {0};

  while (lexer_can_peek(p->lexer)) {
    Decl *decl = parse_decl(p);
    queue_push(list.first, list.last, decl);

    // // eat extra semicolons
    // while (match(p, TokenKind_Semicolon)) {}
  }

  return list;
}

internal void
parser_test() {
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

    StmtList list = parse_statements(&p);

    for (Stmt *it = list.first; it != NULL; it = it->next) {
      Temp scratch = arena_scratch_get(0, 0);

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
      "  name:  string\n"
      "  x,y,z: f32\n"
      "  age:   int\n"
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
      "Vector :: struct { x, y: f32 }\n"
      "v := Vector.{1.0, -1.0}\n"
      // "v: Vector = {1.0. -1.0}\n"
      "Int_Or_Float :: union { i: int; f: f32 }\n"
      "Vectors :: [1+2]Vector\n"
      "f :: proc() { do { print(42); } while (1); }\n"
      // "T :: [16]proc(int, f32, string, Vector, string, f32, int, uint) -> int\n"
      "f :: proc() { E :: enum { A, B, C, }; return; }\n"
      "f :: proc() { if (1) { return 1; } else if (2) { return 2; } else { return 3; } }\n"

      "Int_Or_Ptr :: union { i: int; p: *int }\n"

      "Vector :: struct { x, y: int }\n"
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

    DeclList list = parse_declarations(&p);

    for each_node(it, Decl, list.first) {
      Temp scratch = arena_scratch_get(0, 0);

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
