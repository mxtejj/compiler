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

internal void
print_expr(Arena *arena, String8List *list, int *indent, Expr *e);

internal void
print_type(Arena *arena, String8List *list, int *indent, Type_Spec *t)
{
  // TODO: Use read_only nil_typespec instead of actual NULL
  if (!t) return;

  switch (t->kind)
  {
  case TYPE_SPEC_NULL:
    str8_list_pushf(arena, list, "<NULL>");
    break;
  case TYPE_SPEC_NAME:
    str8_list_pushf(arena, list, "%.*s", str8_varg(t->name));
    break;
  case TYPE_SPEC_PROC:
    str8_list_pushf(arena, list, "proc(");
    if (t->proc.param_count > 0)
    {
      for (Type_Spec *it = t->proc.params.first;
           it != 0;
           it = it->next)
      {
        // if (it->name.count > 0)
        // {
        //   str8_list_pushf(arena, list, "%.*s ", str8_varg(it->name));
        // }
        print_type(arena, list, indent, it);
      }
    }
    str8_list_pushf(arena, list, ")");
    break;
  case TYPE_SPEC_ARRAY:
    str8_list_pushf(arena, list, "[");
    print_expr(arena, list, indent, t->array.count);
    str8_list_pushf(arena, list, "]");
    print_type(arena, list, indent, t->array.elem);
    break;
  case TYPE_SPEC_SLICE:
    str8_list_pushf(arena, list, "[]");
    print_type(arena, list, indent, t->slice.elem);
    break;
  case TYPE_SPEC_PTR:
    str8_list_pushf(arena, list, "*");
    print_type(arena, list, indent, t->ptr.pointee);
    break;
  default:
    assert(0);
    break;
  }
}

internal void
print_ln(Arena *arena, String8List *list, int *indent)
{
  str8_list_pushf(arena, list, "\n%.*s", 2 * (*indent), "                                     ");
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
    str8_list_pushf(arena, list, "(proc %.*s ", str8_varg(d->name));
    str8_list_pushf(arena, list, "(");
    for (Proc_Param *it = d->proc.params.first;
         it != 0;
         it = it->next)
    {
      str8_list_pushf(arena, list, "%.*s ", str8_varg(it->name));
      print_type(arena, list, indent, it->type);
      if (it != d->proc.params.last)
      {
        str8_list_pushf(arena, list, ", ");
      }
    }
    str8_list_pushf(arena, list, ") ");
    print_type(arena, list, indent, d->proc.ret);

    str8_list_pushf(arena, list, ")");
    break;
  case DECL_STRUCT:
    break;
  case DECL_UNION:
    break;
  case DECL_ENUM:
    str8_list_pushf(arena, list, "(enum %.*s ", str8_varg(d->name));
    (*indent)++;
    for (Enum_Member *it = d->enum0.members.first;
         it != 0;
         it = it->next)
    {
      print_ln(arena, list, indent);
      str8_list_pushf(arena, list, "(%.*s ", str8_varg(it->name));
      if (it->value)
      {
        //str8_list_pushf(arena, list, "(%.*s ", str8_varg(it->name));
        print_expr(arena, list, indent, it->value);
        // str8_list_pushf(arena, list, "TODO");
      }
      else
      {
        str8_list_pushf(arena, list, "nil");
      }
      str8_list_pushf(arena, list, ")");
    }
    (*indent)--;
    str8_list_pushf(arena, list, ")");
    break;
  case DECL_VAR:
    // (var (dt f32) (/ 1.0 60.0))
    str8_list_pushf(arena, list, "(var ");
    str8_list_pushf(arena, list, "(%.*s ", str8_varg(d->name));
    print_type(arena, list, indent, d->var.type);
    str8_list_pushf(arena, list, ") ");
    print_expr(arena, list, indent, d->var.expr);
    str8_list_pushf(arena, list, ")");
    break;
  // case DECL_CONST: break;
  default:
    assert(0);
    break;
  }
}

internal void
print_expr(Arena *arena, String8List *list, int *indent, Expr *e)
{
  // TODO: Use read_only nil_expr instead of actual NULL
  if (!e) return;

  switch (e->kind)
  {
  case EXPR_NULL: str8_list_pushf(arena, list, "<NULL>\n"); break;
  case EXPR_IDENT:
    str8_list_pushf(arena, list, "%.*s", str8_varg(e->ident));
    break;
  case EXPR_UNARY:
    if (e->unary.op.kind < 128)
    {
      str8_list_pushf(arena, list, "(%c ", e->unary.op.kind);
    }
    else
    {
      str8_list_pushf(arena, list, "(%.*s ", str8_varg(e->unary.op.lexeme));
    }
    print_expr(arena, list, indent, e->unary.right);
    str8_list_pushf(arena, list, ")");
    break;
  case EXPR_BINARY:
    if (e->binary.op.kind < 128)
    {
      str8_list_pushf(arena, list, "(%c ", e->binary.op.kind);
    }
    else
    {
      str8_list_pushf(arena, list, "(%.*s ", str8_varg(e->binary.op.lexeme));
    }
    print_expr(arena, list, indent, e->binary.left);
    str8_list_pushf(arena, list, " ");
    print_expr(arena, list, indent, e->binary.right);
    str8_list_pushf(arena, list, ")");
    break;
  case EXPR_TERNARY:
    str8_list_pushf(arena, list, "(?: ");
    print_expr(arena, list, indent, e->ternary.cond);
    str8_list_pushf(arena, list, " ");
    print_expr(arena, list, indent, e->ternary.then);
    str8_list_pushf(arena, list, " ");
    print_expr(arena, list, indent, e->ternary.else_);
    str8_list_pushf(arena, list, ")");
    break;
  case EXPR_NIL_LITERAL:
    str8_list_pushf(arena, list, "nil");
    break;
  case EXPR_STRING_LITERAL:
    str8_list_pushf(arena, list, "%.*s", str8_varg(e->literal.string));
    break;
  case EXPR_INTEGER_LITERAL:
    str8_list_pushf(arena, list, "%llu", e->literal.integer);
    break;
  case EXPR_FLOAT_LITERAL:
    str8_list_pushf(arena, list, "%.2f", e->literal.floating);
    break;
  case EXPR_BOOL_LITERAL:
    str8_list_pushf(arena, list, e->literal.boolean ? "true" : "false");
    break;
  case EXPR_GROUP:
    str8_list_pushf(arena, list, "(group ");
    print_expr(arena, list, indent, e->group.expr);
    str8_list_pushf(arena, list, ")");
    break;
  case EXPR_CAST:
    str8_list_pushf(arena, list, "(cast ");
    break;
  case EXPR_CALL:
    str8_list_pushf(arena, list, "(call ");
    print_expr(arena, list, indent, e->call.expr);
    for (Expr *arg = e->call.args.first;
         arg != 0;
         arg = arg->next)
    {
      str8_list_pushf(arena, list, " ");
      print_expr(arena, list, indent, arg);
    }
    str8_list_pushf(arena, list, ")");
    break;
  case EXPR_INDEX:
    str8_list_pushf(arena, list, "(index ");
    print_expr(arena, list, indent, e->index.expr);
    str8_list_pushf(arena, list, " ");
    print_expr(arena, list, indent, e->index.index);
    str8_list_pushf(arena, list, ")");
    break;
  case EXPR_FIELD:
    str8_list_pushf(arena, list, "(field ");
    print_expr(arena, list, indent, e->field.expr);
    str8_list_pushf(arena, list, " ");
    str8_list_pushf(arena, list, "%.*s", str8_varg(e->field.name));
    str8_list_pushf(arena, list, ")");
    break;
  case EXPR_COMPOUND:
    str8_list_pushf(arena, list, "(compound");
    if (e->compound.type != TYPE_SPEC_NULL)
    {
      str8_list_pushf(arena, list, " ");
    }
    print_type(arena, list, indent, e->compound.type);
    for (Compound_Arg *arg = e->compound.args.first;
         arg != 0;
         arg = arg->next)
    {
      str8_list_pushf(arena, list, " ");
      if (arg->optional_name.count > 0)
      {
        str8_list_pushf(arena, list, "%.*s=", str8_varg(arg->optional_name));
      }
      print_expr(arena, list, indent, arg->expr);
    }
    str8_list_pushf(arena, list, ")");
    break;
  case EXPR_SIZE_OF_EXPR:
    str8_list_pushf(arena, list, "(sizeof_expr ");
    str8_list_pushf(arena, list, ")");
    break;
  case EXPR_SIZE_OF_TYPE:
    str8_list_pushf(arena, list, "(sizeof_type ");
    str8_list_pushf(arena, list, ")");
    break;
  default:
    assert(!"TODO");
    break;
  }
}

internal void
print_stmt(Arena *arena, String8List *list, int *indent, Stmt *s)
{
  // TODO: Use read_only nil_stmt instead of actual NULL
  if (!s) return;

  switch (s->kind)
  {
  case STMT_NULL:
    str8_list_pushf(arena, list, "<NULL>");
    break;
  case STMT_BLOCK:
    str8_list_pushf(arena, list, "(block ");
    (*indent)++;
    for (Stmt *it = s->block.stmts.first;
         it != 0;
         it = it->next)
    {
      print_ln(arena, list, indent);
      print_stmt(arena, list, indent, it);
    }
    (*indent)--;
    str8_list_pushf(arena, list, ")");
    break;
  case STMT_IF:
    str8_list_pushf(arena, list, "(if ");
    print_expr(arena, list, indent, s->if0.cond);
    (*indent)++;
    print_ln(arena, list, indent);
    print_stmt(arena, list, indent, s->if0.then_block);
    (*indent)--;
    print_ln(arena, list, indent);
    if (s->if0.else_stmt != NULL)
    {
      if (s->if0.else_stmt->kind == STMT_BLOCK)
      {
        (*indent)++;
        str8_list_pushf(arena, list, "(else ");
        print_ln(arena, list, indent);
        print_stmt(arena, list, indent, s->if0.else_stmt);
        str8_list_pushf(arena, list, ")");
        (*indent)--;
      }
      else
      {
        assert(s->if0.else_stmt->kind == STMT_IF);
        str8_list_pushf(arena, list, "(else ");
        print_stmt(arena, list, indent, s->if0.else_stmt);
        str8_list_pushf(arena, list, ")");
      }
    }
    break;
  case STMT_DO_WHILE:
    str8_list_pushf(arena, list, "(do ");
    (*indent)++;
    print_ln(arena, list, indent);
    print_stmt(arena, list, indent, s->do_while.body);
    print_ln(arena, list, indent);
    (*indent)--;
    str8_list_pushf(arena, list, "(while ");
    print_expr(arena, list, indent, s->do_while.cond);
    str8_list_pushf(arena, list, ")");
    str8_list_pushf(arena, list, ")");
    break;
  case STMT_WHILE:
    str8_list_pushf(arena, list, "(while ");
    print_expr(arena, list, indent, s->while0.cond);
    (*indent)++;
    print_ln(arena, list, indent);
    print_stmt(arena, list, indent, s->while0.body);
    (*indent)--;
    str8_list_pushf(arena, list, ")");
    break;
  // case STMT_FOR:      break;
  // case STMT_SWITCH:   break;
  case STMT_RETURN:
    str8_list_pushf(arena, list, "(return");
    if (s->return0.expr)
    {
      str8_list_pushf(arena, list, " ");
      print_expr(arena, list, indent, s->return0.expr);
    }
    str8_list_pushf(arena, list, ")");
    break;
  case STMT_BREAK:
    str8_list_pushf(arena, list, "(break)");
    break;
  case STMT_CONTINUE:
    str8_list_pushf(arena, list, "(continue)");
    break;
  case STMT_EXPR:
    print_expr(arena, list, indent, s->expr);
    break;
  case STMT_DECL:
    print_decl(arena, list, indent, s->decl);
    break;
  default:
    assert(!"TODO");
    break;
  }
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
