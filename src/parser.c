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
push_compound_arg(Compound_Arg_List *list, Compound_Arg *arg)
{
  sll_queue_push(list->first, list->last, arg);
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
  u64 col = lexer_col(l);
  printf("%*s" CLR_GRN "^\n" CLR_RESET, (int)col, "");

  va_end(args);

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
parser_check(Parser *p, Token_Kind kind)
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
  report_error(p, "Expected '%.*s', got '%.*s'", str8_varg(str_from_token_kind(kind)), str8_varg(str_from_token_kind(p->curr.kind)));
  trap();
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

internal b32
is_named_arg(Parser *p)
{
  return parser_check(p, TOKEN_IDENT) && peek(p, '=');
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

internal b32
is_type_followed_by_lbrace(Parser *p)
{
  Arena_Temp scratch = arena_scratch_get(0, 0);

  Parser temp_parser = *p;
  Lexer temp_lexer = *p->lexer;

  temp_parser.arena = scratch.arena;
  temp_parser.lexer = &temp_lexer;

  parse_type(&temp_parser);
  b32 result = temp_parser.curr.kind == '{';

  arena_scratch_release(scratch);
  return result;
}

internal Expr *
parse_compound_element(Parser *p)
{
  if (is_type_start(p) && is_type_followed_by_lbrace(p))
  {
    Type_Spec *type = parse_type(p);
    expect(p, '{');

    Compound_Arg_List args = {0};

    if (!match(p, '}'))
    {
      do
      {
        Expr *elem = parse_compound_element(p);
        Compound_Arg *arg = push_struct(p->arena, Compound_Arg);
        arg->expr = elem;
        push_compound_arg(&args, arg);
      } while (match(p, ','));

      expect(p, '}');
    }

    return expr_compound(p, type, args);
  }
  return parse_expr(p);
}

internal Expr *
parse_expr_primary(Parser *p)
{
  // implicit compound literals
  if ((p->expr_parse_flags & EXPR_ALLOW_COMPOUND) && match(p, '{'))
  {
    Compound_Arg_List args = {0};

    if (!match(p, '}'))
    {
      do
      {
        Expr *value = parse_expr(p);
        Compound_Arg *arg = push_struct(p->arena, Compound_Arg);
        arg->expr = value;
        push_compound_arg(&args, arg);
      } while (match(p, ','));

      expect(p, '}');
    }

    local_persist Type_Spec null_type = { .kind = TYPE_SPEC_NULL };
    return expr_compound(p, &null_type, args);
  }

  // if (parser_check(p, TOKEN_IDENT) && peek(p, '{'))
  if ((p->expr_parse_flags & EXPR_ALLOW_COMPOUND) && is_type_start(p) && is_type_followed_by_lbrace(p))
  {
    Type_Spec *type = parse_type(p);
    expect(p, '{');
    Compound_Arg_List args = {0};

    if (!match(p, '}'))
    {
      b32 is_named, decided = false;

      do
      {
        if (!decided)
        {
          is_named = is_named_arg(p);
          decided  = true;
        }

        if (is_named)
        {
          if (!parser_check(p, TOKEN_IDENT) || !peek(p, '='))
          {
            // TODO:
            // Person{name = "Bob", 5}
            //                        ^
            // arrow should be under 5???
            report_error(p, "Expected field name");
          }
          // named field
          String8 name = parse_ident(p);
          expect(p, '=');
          Expr *value = parse_compound_element(p);

          Compound_Arg *arg = push_struct(p->arena, Compound_Arg);
          arg->expr = value;
          arg->optional_name = name;
          push_compound_arg(&args, arg);
        }
        else
        {
          if (is_named_arg(p))
          {
            report_error(p, "Named fields not allowed after positional fields");
          }
          // positional field
          Expr *value = parse_compound_element(p);

          Compound_Arg *arg = push_struct(p->arena, Compound_Arg);
          arg->expr = value;
          push_compound_arg(&args, arg);
        }
      } while (match(p, ','));

      expect(p, '}');
    }

    return expr_compound(p, type, args);
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

      if (!match(p, ')'))
      {
        do
        {
          Expr *arg = parse_expr(p);
          sll_queue_push(args.first, args.last, arg);
        } while (match(p, ','));
        expect(p, ')');
      }

      expr = expr_call(p, expr, args);
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

    // FIELD person.name
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
      Expr *expr = parse_expr(p);
      expect(p, ')');
      expr = expr_size_of_expr(p, expr);
    }
    return expr;
  }

  // prefix operators
  // TODO(mxtej) add?: increment, decrement
  while (match(p, '!') || match(p, '-') || match(p, '+') || match(p, '~'))
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
parse_expr_logical_and(Parser *p)
{
  Expr *expr = parse_expr_equality(p);

  while (match(p, TOKEN_LOGICAL_AND))
  {
    Token op = p->prev;
    Expr *right = parse_expr_equality(p);
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

internal Expr *
parse_expr_assignment(Parser *p)
{
  Expr *expr = parse_expr_ternary(p);

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
    Expr *right = parse_expr_assignment(p); // RIGHT ASSOCIATIVE

    // semantic check later: left must be assignable
    expr = expr_binary(p, expr, op, right);
  }

  return expr;
}

internal Expr *
parse_expr_with_flags(Parser *p, Expr_Parse_Flags flags)
{
  p->expr_parse_flags = flags;
  // return parse_expr(p);
  return parse_expr_assignment(p);
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
  // return parse_expr_assignment(p);
  return parse_expr_with_flags(p, EXPR_ALLOW_COMPOUND);
}

/////////////////////////////////////////////////////
// STATEMENTS
internal Stmt *
parse_stmt(Parser *p);

internal Stmt *
parse_stmt_expr(Parser *p)
{
  // Don't allow implicit compound literal
  Expr *expr = parse_expr_with_flags(p, 0);
  expect(p, ';');
  return stmt_expr(p, expr);
}

internal Stmt *
parse_stmt_block(Parser *p)
{
  expect(p, '{');
  Stmt *s = stmt_alloc(p, STMT_BLOCK);
  while (!parser_check(p, '}'))
  {
    Stmt *stmt = parse_stmt(p);
    sll_queue_push(s->block.stmts.first, s->block.stmts.last, stmt);
    s->block.stmts.count += 1;
  }
  expect(p, '}');
  return s;
}

internal Stmt *
parse_stmt_if(Parser *p)
{
  Stmt *s = stmt_alloc(p, STMT_IF);

  s->if0.cond       = parse_expr_with_flags(p, 0);
  s->if0.then_block = parse_stmt_block(p);
  s->if0.else_stmt  = NULL;

  if (match(p, TOKEN_ELSE))
  {
    if (match(p, TOKEN_IF))
    {
      // else if -> recurse
      s->if0.else_stmt = parse_stmt_if(p);
    }
    else
    {
      // else -> block
      s->if0.else_stmt = parse_stmt_block(p);
    }
  }

  return s;
}

internal Stmt *
parse_stmt_while(Parser *p)
{
  Stmt *s = stmt_alloc(p, STMT_WHILE);

  s->while0.cond = parse_expr_with_flags(p, 0);
  s->while0.body = parse_stmt_block(p);

  return s;
}

internal Stmt *
parse_stmt_do_while(Parser *p)
{
  Stmt *s = stmt_alloc(p, STMT_DO_WHILE);

  /*
  do
  {
    body;
  } while cond;
  */

  s->do_while.body = parse_stmt_block(p);
  expect(p, TOKEN_WHILE);
  s->do_while.cond = parse_expr_with_flags(p, 0);
  expect(p, ';');

  return s;
}

internal Stmt *
parse_stmt_return(Parser *p)
{
  Stmt *s = stmt_alloc(p, STMT_RETURN);
  s->return0.expr = parse_expr(p);
  expect(p, ';');
  return s;
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
parse_stmt_decl(Parser *p)
{
  // Example: var a = 5;
  //        | const PI = 3.14;
  //
  // TODO:
  // Eventually support:
  // a := 5;
  // PI :: 3.14

  bool is_const = false;
  if (p->prev.kind == TOKEN_VAR)
  {
    is_const = false;
  }
  else if (p->prev.kind == TOKEN_CONST)
  {
    is_const = true;
  }
  else
  {
    assert(!"parse_stmt_decl called without var/const");
  }

  String8 name = parse_ident(p);

  Type_Spec *type = NULL;
  Expr      *init = NULL;

  // optional type annotation
  if (match(p, ':'))
  {
    type = parse_type(p);
  }

  // optional initializer
  if (match(p, '='))
  {
    init = parse_expr(p);
  }

  expect(p, ';');

  // TODO: CLEANUP
  Stmt *s = stmt_alloc(p, STMT_DECL);
  // @HACK
  s->decl = push_struct(p->arena, Decl);

  if (is_const)
  {
    s->decl->kind = DECL_CONST;
    s->decl->name = name;
    s->decl->const0.expr = init;
  }
  else
  {
    s->decl->kind = DECL_VAR;
    s->decl->name = name;
    s->decl->var.type = type;
    s->decl->var.expr = init;
  }

  return s;
}

internal Stmt *
parse_stmt(Parser *p)
{
  if (parser_check(p, '{'))
    return parse_stmt_block(p);

  // TODO switch-case
  if (match(p, TOKEN_IF))    return parse_stmt_if(p);
  if (match(p, TOKEN_DO))    return parse_stmt_do_while(p);
  if (match(p, TOKEN_WHILE)) return parse_stmt_while(p);
  // if (match(p, DEFER)) return parse_stmt_defer(p);
  if (match(p, TOKEN_RETURN))   return parse_stmt_return(p);
  if (match(p, TOKEN_CONTINUE)) return parse_stmt_continue(p);
  if (match(p, TOKEN_BREAK))    return parse_stmt_break(p);
  if (match(p, TOKEN_VAR))      return parse_stmt_decl(p);
  if (match(p, TOKEN_CONST))    return parse_stmt_decl(p);

  /*
  STMT_FOR,
  STMT_SWITCH,
  STMT_DECL,
  */

  return parse_stmt_expr(p);
}

internal Stmt_List
parse_statements(Parser *p)
{
  Stmt_List list = {0};

  while (lexer_can_peek(p->lexer))
  {
    Stmt *stmt = parse_stmt(p);
    sll_queue_push(list.first, list.last, stmt);
  }

  return list;
}

/////////////////////////////////////////////////////
// DECLARATIONS

internal Decl *
parse_decl_enum(Parser *p)
{
  String8 name = parse_ident(p);

  expect(p, '{');

  Decl *decl = decl_alloc(p, DECL_ENUM);
  decl->name = name;

  Enum_Member_List *members = &decl->enum0.members;
  while (!parser_check(p, '}'))
  {
    Enum_Member *member = push_struct(p->arena, Enum_Member);

    member->name = parse_ident(p);
    if (match(p, '='))
    {
      member->value = parse_expr(p);
    }

    sll_queue_push(members->first, members->last, member);

    if (!match(p, ','))
    {
      break;
    }
  }

  expect(p, '}');

  return decl;
}

internal Decl *
parse_decl_var(Parser *p)
{
  // var a = EXPR;
  String8 name = parse_ident(p);
  expect(p, '=');
  Expr *expr = parse_expr(p);
  expect(p, ';');

  Decl *decl = decl_alloc(p, DECL_VAR);
  decl->name = name;
  decl->var.expr = expr;

  return decl;
}

internal Proc_Param *
parse_decl_proc_param(Parser *p)
{
  Proc_Param *param = push_struct(p->arena, Proc_Param);

  param->name = parse_ident(p);
  expect(p, ':');
  param->type = parse_type(p);

  return param;
}

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

  assert(!"Expected type name");
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

internal Type_Spec *
parse_type_proc(Parser *p)
{
  // update: proc(...) -> [...]
  Type_Spec *t = type_spec_alloc(p, TYPE_SPEC_PROC);

  expect(p, '(');

  while (!match(p, ')'))
  {
    Type_Spec *param = parse_type(p);
    sll_queue_push(t->proc.params.first, t->proc.params.last, param);
    t->proc.param_count += 1;
    match(p, ',');
  }

  if (match(p, TOKEN_ARROW))
  {
    t->proc.ret = parse_type(p);
  }

  return t;
}

internal Type_Spec *
parse_type(Parser *p)
{
  if (match(p, TOKEN_PROC)) return parse_type_proc(p);
  return parse_type_prefix(p);
}

internal Decl *
parse_decl_proc(Parser *p)
{
  /*
  proc make_person(name: string, age: int) -> Person
  {
    var person = Person{};
    person.name = name;
    person.age  = age;
    return person;
  }
  */
  // proc main(argc: int, argv: []cstring) -> int { return 0; }
  String8 name = parse_ident(p);
  expect(p, '(');

  Decl *decl = decl_alloc(p, DECL_PROC);
  decl->name = name;

  Param_List *params = &decl->proc.params;

  while (!match(p, ')'))
  {
    Proc_Param *param = parse_decl_proc_param(p);
    dll_push_back(params->first, params->last, param);
    match(p, ',');
  }
  // if (!match(p, ')'))
  // {
  //   do
  //   {
  //     Proc_Param *param = parse_decl_proc_param(p);
  //     dll_push_back(params->first, params->last, param);
  //   } while (match(p, ','));
  // }
  // expect(p, ')');
  if (match(p, TOKEN_ARROW))
  {
    decl->proc.ret = parse_type(p);
  }
  decl->proc.body = parse_stmt_block(p);

  return decl;
}

// internal Decl *
// parse_decl_struct(Parser *p)
// {
//   String8 name = parse_ident(p);

//   expect(p, '{');

//   Decl *decl = decl_alloc(p, DECL_STRUCT);
//   decl->struct0.name = name;

//   Enum_Member_List *members = &decl->struct0.members;
//   while (!parser_check(p, '}'))
//   {
//     Enum_Member *member = push_struct(p->arena, Enum_Member);

//     member->name = parse_ident(p);
//     if (match(p, '='))
//     {
//       member->value = parse_expr(p);
//     }

//     dll_push_back(members->first, members->last, member);

//     if (!match(p, ','))
//     {
//       break;
//     }
//   }

//   expect(p, '}');

//   return decl;
// }

internal Decl *
parse_decl(Parser *p)
{
  if (match(p, TOKEN_PROC)) return parse_decl_proc(p);
  // aggr
  if (match(p, TOKEN_ENUM)) return parse_decl_enum(p);
  if (match(p, TOKEN_VAR))  return parse_decl_var(p);
  // const
  // assert(!"Expected declaration keyword");
  report_error(p, "Expected declaration keyword");
  return NULL;
}

internal Decl_List
parse_declarations(Parser *p)
{
  Decl_List list = {0};

  while (lexer_can_peek(p->lexer))
  {
    Decl *decl = parse_decl(p);
    dll_push_back(list.first, list.last, decl);
  }

  return list;
}

internal void
parser_test()
{
  {
    String8 source = S(
      "x = 5 + 3;\n"
      "x = -a;\n"
      "x = (2 + 3) * 4;\n"
      "x = a > b ? 1 : 0;\n"
      "x = a && b;\n"
      "x = y = z = 42;\n"
      "{\n"
      "  a = 5;\n"
      "  if a > b\n"
      "  {\n"
      "    x = a;\n"
      "  }\n"
      "  else if a > c\n"
      "  {\n"
      "    x = c;\n"
      "  }\n"
      "  else if c > a\n"
      "  {\n"
      "    x = a;\n"
      "  }\n"
      "  else\n"
      "  {\n"
      "    x = b;\n"
      "  }\n"
      "}\n"

      "do\n"
      "{\n"
      "  a -= 1;\n"
      "} while (a > b);\n"

      "while !request_shutdown()\n"
      "{\n"
      "  var dt: f32 = 1.0 / 60.0;\n"
      "  tick_game(dt);\n"
      "}\n"

      // TODO: This takes the array and gives me a slice of all elements
      // or i can also do array[<lo>:<hi>]
      // "return array[:];\n"
      "return Person{\"Joe\", 53};\n"

      "return [10]Person{};\n"

      "var update_proc: proc(Entity);\n"

      "break;\n"
      "continue;\n"
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

  {
    String8 source = S(
      // "var foo = a ? a&b + c<<d + e*f == +u-v-w + *g/h(x,y) + -i%k[x] && m <= n*(p+q)/r : 0;\n"
      // "var foo = a ? 1 : 0;\n"
      // "var foo = a ? b + c : 0;\n"
      // "var foo = a ? b + c << d : 0;\n"
      // "var foo = a ? b == c : 0;\n"
      // "var foo = a ? +u : 0;\n"
      // // "var foo = a ? h(x) : 0;\n" // proc call
      // // "var foo = a ? k[x] : 0;\n" // indexing
      "enum Color\n"
      "{\n"
      "  Red = 3,\n"
      "  Green,\n"
      "  Blue = 0\n"
      "}\n"
      // "\n"
      // "struct Person\n"
      // "{\n"
      // "  name: string,\n"
      // "  age:  int,\n"
      // "}\n"
      "\n"
      "var x = Point{1,2};\n"
      "\n"
      "proc make_person(name: string, age: int) -> Person\n"
      "{\n"
      "  var person: Person;\n"
      "  person.name = name;\n" // TODO
      "  person.age  = age;\n"
      "  return person;\n"
      "}\n"
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

    for (Decl *it = list.first; it != NULL; it = it->next)
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
