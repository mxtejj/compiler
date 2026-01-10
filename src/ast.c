#include "parser.h"
#include "print.h"

#include <stdio.h>
#include <stdarg.h>
#include <ctype.h>

////////////////////////////////
//- Type Specs

internal Type_Spec *
type_spec_alloc(Parser *p, Type_Spec_Kind kind)
{
  Type_Spec *t = push_struct(p->arena, Type_Spec);
  t->kind = kind;
  return t;
}

internal Type_Spec *
type_spec_name(Parser *p, String8 name)
{
  Type_Spec *t = type_spec_alloc(p, TYPE_SPEC_NAME);
  t->name = name;
  return t;
}

internal Type_Spec *
type_spec_proc(Parser *p, Decl_Array params, Type_Spec *ret, Stmt *body)
{
  Type_Spec *t = type_spec_alloc(p, TYPE_SPEC_PROC);
  t->proc.params = params;
  t->proc.ret    = ret;
  t->proc.body   = body;
  return t;
}

internal Type_Spec *
type_spec_enum(Parser *p, Enum_Member_Array members)
{
  Type_Spec *t = type_spec_alloc(p, TYPE_SPEC_ENUM);
  t->enum_members = members;
  return t;
}

internal Type_Spec *
type_spec_aggr(Parser *p, Type_Spec_Kind kind, Aggr_Field_Array fields)
{
  assert(kind == TYPE_SPEC_STRUCT || kind == TYPE_SPEC_UNION);
  Type_Spec *t = type_spec_alloc(p, kind);
  t->aggr_fields = fields;
  return t;
}

internal Type_Spec *type_spec_array(Parser *p, Expr *count, Type_Spec *elem)
{
  Type_Spec *t = type_spec_alloc(p, TYPE_SPEC_ARRAY);
  t->array.count = count;
  t->array.elem  = elem;
  return t;
}

internal Type_Spec *type_spec_slice(Parser *p, Type_Spec *elem)
{
  Type_Spec *t = type_spec_alloc(p, TYPE_SPEC_SLICE);
  t->slice.elem = elem;
  return t;
}

internal Type_Spec *type_spec_ptr(Parser *p, Type_Spec *pointee)
{
  Type_Spec *t = type_spec_alloc(p, TYPE_SPEC_PTR);
  t->ptr.pointee = pointee;
  return t;
}

////////////////////////////////
//- Declarations

internal Decl *
decl_alloc(Parser *p, String8 name, Decl_Kind kind)
{
  Decl *decl = push_struct(p->arena, Decl);
  decl->kind = kind;
  decl->name = name;
  return decl;
}

internal Decl *
decl_var(Parser *p, String8 name, Type_Spec *type_hint, Expr *init_expr)
{
  Decl *decl = decl_alloc(p, name, DECL_VAR);
  decl->type_hint = type_hint;
  decl->init_expr = init_expr;
  return decl;
}

internal Decl *
decl_const(Parser *p, String8 name, Type_Spec *type_hint, Expr *init_expr, Type_Spec *init_type)
{
  Decl *decl = decl_alloc(p, name, DECL_CONST);
  decl->type_hint = type_hint;
  decl->init_expr = init_expr;
  decl->init_type = init_type;
  return decl;
}

////////////////////////////////
//- Expressions

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
expr_char_lit(Parser *p, char c)
{
  Expr *expr = expr_alloc(p, EXPR_CHAR_LITERAL);
  expr->literal.character = c;
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
expr_call(Parser *p, Expr *e, Expr_Array args)
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
expr_compound(Parser *p, Type_Spec *type, Compound_Field_Array args)
{
  Expr *expr = expr_alloc(p, EXPR_COMPOUND);
  expr->compound.type = type;
  expr->compound.args = args;
  return expr;
}

internal Expr *
expr_size_of_expr(Parser *p, Expr *e)
{
  Expr *expr = expr_alloc(p, EXPR_SIZE_OF);
  expr->size_of.is_expr = true;
  expr->size_of.expr = e;
  return expr;
}

internal Expr *
expr_size_of_type(Parser *p, Type_Spec *type)
{
  Expr *expr = expr_alloc(p, EXPR_SIZE_OF);
  expr->size_of.is_expr = false;
  expr->size_of.type = type;
  return expr;
}

/////////////////////////////////////////////////////
//- Statements
internal Stmt *
stmt_alloc(Parser *p, Stmt_Kind kind)
{
  Stmt *s = push_struct(p->arena, Stmt);
  s->kind = kind;
  return s;
}

internal Stmt *
stmt_block(Parser *p, Stmt_Array stmts)
{
  Stmt *stmt = stmt_alloc(p, STMT_BLOCK);
  stmt->block = stmts;
  return stmt;
}

internal Stmt *
stmt_if(Parser *p, Expr *cond, Stmt *then_block, Stmt *else_stmt)
{
  Stmt *stmt = stmt_alloc(p, STMT_IF);
  stmt->if0.cond = cond;
  stmt->if0.then_block = then_block;
  stmt->if0.else_stmt = else_stmt;
  return stmt;
}

internal Stmt *
stmt_while(Parser *p, Expr *cond, Stmt *body)
{
  Stmt *stmt = stmt_alloc(p, STMT_WHILE);
  stmt->while0.cond = cond;
  stmt->while0.body = body;
  return stmt;
}

internal Stmt *
stmt_for(Parser *p, Stmt *init, Expr *cond, Stmt *loop, Stmt *body)
{
  Stmt *stmt = stmt_alloc(p, STMT_FOR);
  stmt->for0.init = init;
  stmt->for0.cond = cond;
  stmt->for0.loop = loop;
  stmt->for0.body = body;
  return stmt;
}

internal Stmt *
stmt_for_in(Parser *p, Expr *item, Expr *iter, Stmt *body)
{
  Stmt *stmt = stmt_alloc(p, STMT_FOR_IN);
  stmt->for_in.item = item;
  stmt->for_in.iter = iter;
  stmt->for_in.body = body;
  return stmt;
}

internal Stmt *
stmt_return(Parser *p, Expr *expr)
{
  Stmt *stmt = stmt_alloc(p, STMT_RETURN);
  stmt->return_expr = expr;
  return stmt;
}

internal Stmt *
stmt_defer(Parser *p, Stmt *s)
{
  Stmt *stmt = stmt_alloc(p, STMT_RETURN);
  stmt->defer_stmt = s;
  return stmt;
}

internal Stmt *
stmt_expr(Parser *p, Expr *expr)
{
  Stmt *stmt = stmt_alloc(p, STMT_EXPR);
  stmt->expr = expr;
  return stmt;
}

internal Stmt *
stmt_decl(Parser *p, Decl *decl)
{
  Stmt *stmt = stmt_alloc(p, STMT_DECL);
  stmt->decl = decl;
  return stmt;
}

internal Stmt *
stmt_switch(Parser *p, Expr *expr, Switch_Case_Array cases)
{
  Stmt *stmt = stmt_alloc(p, STMT_SWITCH);
  stmt->switch0.expr  = expr;
  stmt->switch0.cases = cases;
  return stmt;
}

// TODO: for testing
internal Token
make_token(char c)
{
  return (Token){ .kind = c };
}

internal void
print_type(Arena *arena, String8List *list, int *indent, Type_Spec *t)
{
  // TODO(#2): Use read_only nil_typespec instead of actual NULL
  if (!t)
  {
    str8_list_pushf(arena, list, "nil");
    return;
  }

  Arena_Temp scratch = arena_scratch_get(&arena, 1);

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

    for each_index(i, t->proc.params.count)
    {
      Decl *it = t->proc.params.v[i];
      // if (it->name.count > 0)
      // {
      //   str8_list_pushf(arena, list, "%.*s ", str8_varg(it->name));
      // }
      // print_type(arena, list, indent, it);
      print_decl(arena, list, indent, it);
      if (it != t->proc.params.v[t->proc.params.count-1])
      {
        str8_list_pushf(arena, list, ", ");
      }
    }

    str8_list_pushf(arena, list, ")");
    str8_list_pushf(arena, list, " -> ");
    print_type(arena, list, indent, t->proc.ret);
    (*indent)++;
    print_ln(arena, list, indent);
    print_stmt(arena, list, indent, t->proc.body);
    (*indent)--;
    break;
  case TYPE_SPEC_ENUM:
    str8_list_pushf(arena, list, "(enum ");
    (*indent)++;
    for each_index(i, t->enum_members.count)
    {
      Enum_Member it = t->enum_members.v[i];
      print_ln(arena, list, indent);
      str8_list_pushf(arena, list, "(%.*s ", str8_varg(it.name));
      if (it.value)
      {
        //str8_list_pushf(arena, list, "(%.*s ", str8_varg(it.name));
        print_expr(arena, list, indent, it.value);
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
  case TYPE_SPEC_STRUCT:
  case TYPE_SPEC_UNION:
    str8_list_pushf(arena, list, "(");
    if (t->kind == TYPE_SPEC_STRUCT)
    {
      str8_list_pushf(arena, list, "struct ");
    }
    else
    {
      str8_list_pushf(arena, list, "union ");
    }

    for each_index(i, t->aggr_fields.count)
    {
      Aggr_Field it = t->aggr_fields.v[i];

      (*indent)++;
      print_ln(arena, list, indent);
      StringJoin join = {
        .mid = S(" "),
      };
      String8 names = str8_list_join(scratch.arena, &it.names, &join);
      str8_list_pushf(arena, list, "(%.*s ", str8_varg(names));
      print_type(arena, list, indent, it.type);
      str8_list_pushf(arena, list, ")");
      // (x y z f32)
      (*indent)--;
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

  arena_scratch_release(scratch);
}

internal void
print_ln(Arena *arena, String8List *list, int *indent)
{
  str8_list_pushf(arena, list, "\n%.*s", 2 * (*indent), "                                     ");
}

internal void
print_decl(Arena *arena, String8List *list, int *indent, Decl *d)
{
  if (!d)
  {
    printf(CLR_RED "Decl is NULL!!!\n" CLR_RESET);
    return;
  }

  Arena_Temp scratch = arena_scratch_get(&arena, 1);

  char *tag = (d->kind == DECL_CONST) ? "const" : "var";
  str8_list_pushf(arena, list, "(%s %.*s", tag, str8_varg(d->name));

  if (d->type_hint)
  {
    // str8_list_pushf(arena, list, " type:");
    str8_list_pushf(arena, list, " ");
    print_type(arena, list, indent, d->type_hint);
  }

  // str8_list_pushf(arena, list, " value:");
  str8_list_pushf(arena, list, " ");
  if (d->init_type)
  {
    print_type(arena, list, indent, d->init_type);
  }
  else if (d->init_expr)
  {
    print_expr(arena, list, indent, d->init_expr);
  }
  else
  {
    str8_list_pushf(arena, list, "<uninitialized>");
  }

  str8_list_pushf(arena, list, ")");

  arena_scratch_release(scratch);
}

internal void
print_expr(Arena *arena, String8List *list, int *indent, Expr *e)
{
  // TODO(#3): Use read_only nil_expr instead of actual NULL
  if (!e)
  {
    str8_list_pushf(arena, list, "nil");
    return;
  }

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
  case EXPR_CHAR_LITERAL:
  {
    if (e->literal.character < 128 && isprint(e->literal.character))
    {
      str8_list_pushf(arena, list, "%c", e->literal.character);
    }
    else
    {
      str8_list_pushf(arena, list, "<ASCII %d>", e->literal.character);
    }
    break;
  }
  case EXPR_GROUP:
    str8_list_pushf(arena, list, "(group ");
    print_expr(arena, list, indent, e->group.expr);
    str8_list_pushf(arena, list, ")");
    break;
  case EXPR_CAST:
    str8_list_pushf(arena, list, "(cast ");
    print_type(arena, list, indent, e->cast.type);
    str8_list_pushf(arena, list, " ");
    print_expr(arena, list, indent, e->cast.expr);
    str8_list_pushf(arena, list, ")");
    break;
  case EXPR_CALL:
    str8_list_pushf(arena, list, "(call ");
    print_expr(arena, list, indent, e->call.expr);
    for (Expr **it = e->call.args.v; it != e->call.args.v + e->call.args.count; it += 1)
    {
      str8_list_pushf(arena, list, " ");
      print_expr(arena, list, indent, *it);
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
    str8_list_pushf(arena, list, "(compound ");
    print_type(arena, list, indent, e->compound.type);
    for each_index(i, e->compound.args.count)
    {
      Compound_Field *arg = e->compound.args.v[i];
      str8_list_pushf(arena, list, " ");
      switch (arg->kind)
      {
      case COMPOUND_FIELD_NONE:
        break;
      case COMPOUND_FIELD_NAME:
      {
        if (arg->name.count > 0)
        {
          str8_list_pushf(arena, list, "%.*s=", str8_varg(arg->name));
        }
        break;
      }
      case COMPOUND_FIELD_INDEX:
      {
        print_expr(arena, list, indent, arg->index);
        str8_list_pushf(arena, list, "=");
        break;
      }
      default:
        assert(0);
        break;
      }
      print_expr(arena, list, indent, arg->init);
    }
    str8_list_pushf(arena, list, ")");
    break;
  case EXPR_SIZE_OF:
    str8_list_pushf(arena, list, "(sizeof ");
    if (e->size_of.is_expr)
    {
      print_expr(arena, list, indent, e->size_of.expr);
    }
    else
    {
      print_type(arena, list, indent, e->size_of.type);
    }
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
  // TODO(#4): Use read_only nil_stmt instead of actual NULL
  if (!s)
  {
    str8_list_pushf(arena, list, "nil");
    return;
  }

  switch (s->kind)
  {
  case STMT_NULL:
    str8_list_pushf(arena, list, "<NULL>");
    break;
  case STMT_BLOCK:
    str8_list_pushf(arena, list, "(block ");
    (*indent)++;
    for each_index(i, s->block.count)
    {
      Stmt *it = s->block.v[i];
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
    print_stmt(arena, list, indent, s->while0.body);
    print_ln(arena, list, indent);
    (*indent)--;
    str8_list_pushf(arena, list, "(while ");
    print_expr(arena, list, indent, s->while0.cond);
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
  case STMT_FOR:
    str8_list_pushf(arena, list, "(for ");
    print_stmt(arena, list, indent, s->for0.init);
    str8_list_pushf(arena, list, " ");
    print_expr(arena, list, indent, s->for0.cond);
    str8_list_pushf(arena, list, " ");
    print_stmt(arena, list, indent, s->for0.loop);
    (*indent)++;
    print_ln(arena, list, indent);
    print_stmt(arena, list, indent, s->for0.body);
    (*indent)--;
    str8_list_pushf(arena, list, ")");
    break;
  case STMT_FOR_IN:
    str8_list_pushf(arena, list, "(for-in ");
    print_expr(arena, list, indent, s->for_in.item);
    str8_list_pushf(arena, list, " ");
    print_expr(arena, list, indent, s->for_in.iter);
    str8_list_pushf(arena, list, " ");
    (*indent)++;
    print_ln(arena, list, indent);
    print_stmt(arena, list, indent, s->for_in.body);
    (*indent)--;
    str8_list_pushf(arena, list, ")");
    break;
  case STMT_SWITCH:
    str8_list_pushf(arena, list, "(switch ");
    print_expr(arena, list, indent, s->switch0.expr);
    for each_index(i, s->switch0.cases.count)
    {
      Switch_Case it = s->switch0.cases.v[i];

      (*indent)++;
      print_ln(arena, list, indent);
      str8_list_pushf(arena, list, "(case ");
      for each_index(j, it.labels.count)
      {
        Expr *expr = it.labels.v[j];
        print_expr(arena, list, indent, expr);
        if (j != it.labels.count)
        {
          str8_list_pushf(arena, list, " ");
        }
      }
      {
        (*indent)++;
        print_ln(arena, list, indent);
        print_stmt(arena, list, indent, it.block);
        (*indent)--;
      }
      str8_list_pushf(arena, list, ")");
      (*indent)--;
    }
    str8_list_pushf(arena, list, ")");
    break;
  case STMT_RETURN:
    str8_list_pushf(arena, list, "(return");
    if (s->return_expr)
    {
      str8_list_pushf(arena, list, " ");
      print_expr(arena, list, indent, s->return_expr);
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
