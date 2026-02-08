#include "lexer.h"
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
  t->pos = p->prev.pos; // TODO: this should probably accumulate for []*ident everything there
  t->kind = kind;
  return t;
}

internal Type_Spec *
type_spec_name(Parser *p, String8 name)
{
  Type_Spec *t = type_spec_alloc(p, TypeSpecKind_Name);
  t->name = name;
  return t;
}

internal Type_Spec *
type_spec_proc(Parser *p, Decl_Array params, Type_Spec *ret, Stmt *body)
{
  Type_Spec *t = type_spec_alloc(p, TypeSpecKind_Proc);
  t->proc.params = params;
  t->proc.ret    = ret;
  t->proc.body   = body;
  return t;
}

internal Type_Spec *
type_spec_enum(Parser *p, Enum_Member_Array members)
{
  Type_Spec *t = type_spec_alloc(p, TypeSpecKind_Enum);
  t->enum_members = members;
  return t;
}

internal Type_Spec *
type_spec_aggr(Parser *p, Type_Spec_Kind kind, Aggr_Field_Array fields)
{
  assert(kind == TypeSpecKind_Struct || kind == TypeSpecKind_Union);
  Type_Spec *t = type_spec_alloc(p, kind);
  t->aggr_fields = fields;
  return t;
}

internal Type_Spec *type_spec_array(Parser *p, Expr *count, Type_Spec *elem)
{
  Type_Spec *t = type_spec_alloc(p, TypeSpecKind_Array);
  t->array.count = count;
  t->array.elem  = elem;
  return t;
}

internal Type_Spec *type_spec_slice(Parser *p, Type_Spec *elem)
{
  Type_Spec *t = type_spec_alloc(p, TypeSpecKind_Slice);
  t->slice.elem = elem;
  return t;
}

internal Type_Spec *type_spec_ptr(Parser *p, Type_Spec *pointee)
{
  Type_Spec *t = type_spec_alloc(p, TypeSpecKind_Ptr);
  t->ptr.pointee = pointee;
  return t;
}

////////////////////////////////
//- Declarations

internal Decl *
decl_alloc(Parser *p, String8 name, Decl_Kind kind)
{
  Decl *decl = push_struct(p->arena, Decl);
  decl->pos  = p->curr.pos; // TODO
  decl->kind = kind;
  decl->name = name;
  return decl;
}

internal Decl *
decl_var(Parser *p, String8 name, Type_Spec *type_hint, Expr *init_expr)
{
  Decl *decl = decl_alloc(p, name, DeclKind_Var);
  decl->type_hint = type_hint;
  decl->init_expr = init_expr;
  return decl;
}

internal Decl *
decl_const(Parser *p, String8 name, Type_Spec *type_hint, Expr *init_expr, Type_Spec *init_type)
{
  Decl *decl = decl_alloc(p, name, DeclKind_Const);
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
  // TODO: maybe we should explicitly set the position??
  expr->pos = p->prev.pos; // TODO: this might be wrong since we call `expr_alloc` at the end when we are done parsing??
  expr->kind = kind;
  return expr;
}

internal Expr *
expr_ident(Parser *p, Token ident)
{
  Expr *expr = expr_alloc(p, ExprKind_Ident);
  expr->ident = ident.lexeme;
  return expr;
}

internal Expr *
expr_unary(Parser *p, Token op, Expr *right)
{
  Expr *expr = expr_alloc(p, ExprKind_Unary);
  expr->unary.op    = op;
  expr->unary.right = right;
  return expr;
}

internal Expr *
expr_binary(Parser *p, Expr *left, Token op, Expr *right)
{
  Expr *expr = expr_alloc(p, ExprKind_Binary);
  expr->binary.left  = left;
  expr->binary.op    = op;
  expr->binary.right = right;
  return expr;
}

internal Expr *
expr_ternary(Parser *p, Expr *cond, Expr *then, Expr *else_)
{
  Expr *expr = expr_alloc(p, ExprKind_Ternary);
  expr->ternary.cond = cond;
  expr->ternary.then = then;
  expr->ternary.else_ = else_;
  return expr;
}

internal Expr *
expr_nil_lit(Parser *p)
{
  Expr *expr = expr_alloc(p, ExprKind_NilLiteral);
  return expr;
}

internal Expr *
expr_string_lit(Parser *p, String8 s)
{
  Expr *expr = expr_alloc(p, ExprKind_StringLiteral);
  expr->pos = p->prev.pos;
  expr->literal.string = s;
  return expr;
}

internal Expr *
expr_integer_lit(Parser *p, u64 n)
{
  Expr *expr = expr_alloc(p, ExprKind_IntegerLiteral);
  expr->pos = p->prev.pos;
  expr->literal.integer = n;
  return expr;
}

internal Expr *
expr_float_lit(Parser *p, f64 f)
{
  Expr *expr = expr_alloc(p, ExprKind_FloatLiteral);
  expr->pos = p->prev.pos;
  expr->literal.floating = f;
  return expr;
}

internal Expr *
expr_bool_lit(Parser *p, bool b)
{
  Expr *expr = expr_alloc(p, ExprKind_BoolLiteral);
  expr->pos = p->prev.pos;
  expr->literal.boolean = b;
  return expr;
}

internal Expr *
expr_char_lit(Parser *p, char c)
{
  Expr *expr = expr_alloc(p, ExprKind_CharLiteral);
  expr->pos = p->prev.pos;
  expr->literal.character = c;
  return expr;
}

internal Expr *
expr_group(Parser *p, Expr *e)
{
  Expr *expr = expr_alloc(p, ExprKind_Group);
  expr->group.expr = e;
  return expr;
}

internal Expr *
expr_cast(Parser *p, Type_Spec *type, Expr *e)
{
  Expr *expr = expr_alloc(p, ExprKind_Cast);
  expr->cast.type = type;
  expr->cast.expr = e;
  return expr;
}

internal Expr *
expr_call(Parser *p, Expr *e, Expr_Array args)
{
  Expr *expr = expr_alloc(p, ExprKind_Call);
  expr->call.expr = e;
  expr->call.args = args;
  return expr;
}

internal Expr *
expr_index(Parser *p, Expr *e, Expr *index)
{
  Expr *expr = expr_alloc(p, ExprKind_Index);
  expr->index.expr = e;
  expr->index.index = index;
  return expr;
}

internal Expr *
expr_field(Parser *p, Expr *e, String8 field)
{
  Expr *expr = expr_alloc(p, ExprKind_Field);
  expr->field.expr = e;
  expr->field.name = field;
  return expr;
}

internal Expr *
expr_compound(Parser *p, Type_Spec *type, Compound_Field_Array args)
{
  Expr *expr = expr_alloc(p, ExprKind_Compound);
  expr->compound.type = type;
  expr->compound.args = args;
  return expr;
}

internal Expr *
expr_size_of_expr(Parser *p, Expr *e)
{
  Expr *expr = expr_alloc(p, ExprKind_SizeOf);
  expr->size_of.is_expr = true;
  expr->size_of.expr = e;
  return expr;
}

internal Expr *
expr_size_of_type(Parser *p, Type_Spec *type)
{
  Expr *expr = expr_alloc(p, ExprKind_SizeOf);
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
  s->pos = p->curr.pos; // TODO
  s->kind = kind;
  return s;
}

internal Stmt *
stmt_block(Parser *p, Stmt_Array stmts)
{
  Stmt *stmt = stmt_alloc(p, StmtKind_Block);
  stmt->block = stmts;
  return stmt;
}

internal Stmt *
stmt_if(Parser *p, Expr *cond, Stmt *then_block, Stmt *else_stmt)
{
  Stmt *stmt = stmt_alloc(p, StmtKind_If);
  stmt->if0.cond = cond;
  stmt->if0.then_block = then_block;
  stmt->if0.else_stmt = else_stmt;
  return stmt;
}

internal Stmt *
stmt_while(Parser *p, Expr *cond, Stmt *body)
{
  Stmt *stmt = stmt_alloc(p, StmtKind_While);
  stmt->while0.cond = cond;
  stmt->while0.body = body;
  return stmt;
}

internal Stmt *
stmt_do_while(Parser *p, Expr *cond, Stmt *body)
{
  Stmt *stmt = stmt_alloc(p, StmtKind_DoWhile);
  stmt->while0.cond = cond;
  stmt->while0.body = body;
  return stmt;
}

internal Stmt *
stmt_for(Parser *p, Stmt *init, Expr *cond, Stmt *loop, Stmt *body)
{
  Stmt *stmt = stmt_alloc(p, StmtKind_For);
  stmt->for0.init = init;
  stmt->for0.cond = cond;
  stmt->for0.loop = loop;
  stmt->for0.body = body;
  return stmt;
}

internal Stmt *
stmt_for_in(Parser *p, Expr *item, Expr *iter, Stmt *body)
{
  Stmt *stmt = stmt_alloc(p, StmtKind_ForIn);
  stmt->for_in.item = item;
  stmt->for_in.iter = iter;
  stmt->for_in.body = body;
  return stmt;
}

internal Stmt *
stmt_return(Parser *p, Expr *expr)
{
  Stmt *stmt = stmt_alloc(p, StmtKind_Return);
  stmt->return_expr = expr;
  return stmt;
}

internal Stmt *
stmt_defer(Parser *p, Stmt *s)
{
  Stmt *stmt = stmt_alloc(p, StmtKind_Return);
  stmt->defer_stmt = s;
  return stmt;
}

internal Stmt *
stmt_expr(Parser *p, Expr *expr)
{
  Stmt *stmt = stmt_alloc(p, StmtKind_Expr);
  stmt->expr = expr;
  return stmt;
}

internal Stmt *
stmt_decl(Parser *p, Decl *decl)
{
  Stmt *stmt = stmt_alloc(p, StmtKind_Decl);
  stmt->decl = decl;
  return stmt;
}

internal Stmt *
stmt_switch(Parser *p, Expr *expr, Switch_Case_Array cases)
{
  Stmt *stmt = stmt_alloc(p, StmtKind_Switch);
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
  case TypeSpecKind_Null:
    str8_list_pushf(arena, list, "<NULL>");
    break;
  case TypeSpecKind_Name:
    str8_list_pushf(arena, list, "%.*s", str8_varg(t->name));
    break;
  case TypeSpecKind_Proc:
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
  case TypeSpecKind_Enum:
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
  case TypeSpecKind_Struct:
  case TypeSpecKind_Union:
    str8_list_pushf(arena, list, "(");
    if (t->kind == TypeSpecKind_Struct)
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
  case TypeSpecKind_Array:
    str8_list_pushf(arena, list, "[");
    print_expr(arena, list, indent, t->array.count);
    str8_list_pushf(arena, list, "]");
    print_type(arena, list, indent, t->array.elem);
    break;
  case TypeSpecKind_Slice:
    str8_list_pushf(arena, list, "[]");
    print_type(arena, list, indent, t->slice.elem);
    break;
  case TypeSpecKind_Ptr:
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

  char *tag = (d->kind == DeclKind_Const) ? "const" : "var";
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

internal bool
print_is_symbol(Token_Kind kind) {
  return TokenKind_SymbolBegin <= kind && kind < TokenKind_SymbolEnd;
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
  case ExprKind_Null: str8_list_pushf(arena, list, "<NULL>\n"); break;
  case ExprKind_Ident:
    str8_list_pushf(arena, list, "%.*s", str8_varg(e->ident));
    break;
  case ExprKind_Unary:
    if (print_is_symbol(e->unary.op.kind))
    {
      str8_list_pushf(arena, list, "(%.*s ", str8_varg(str_from_token_kind(arena, e->unary.op.kind)));
    }
    else
    {
      str8_list_pushf(arena, list, "(%.*s ", str8_varg(e->unary.op.lexeme));
    }
    print_expr(arena, list, indent, e->unary.right);
    str8_list_pushf(arena, list, ")");
    break;
  case ExprKind_Binary:
    if (print_is_symbol(e->unary.op.kind))
    {
      str8_list_pushf(arena, list, "(%.*s ", str8_varg(str_from_token_kind(arena, e->unary.op.kind)));
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
  case ExprKind_Ternary:
    str8_list_pushf(arena, list, "(?: ");
    print_expr(arena, list, indent, e->ternary.cond);
    str8_list_pushf(arena, list, " ");
    print_expr(arena, list, indent, e->ternary.then);
    str8_list_pushf(arena, list, " ");
    print_expr(arena, list, indent, e->ternary.else_);
    str8_list_pushf(arena, list, ")");
    break;
  case ExprKind_NilLiteral:
    str8_list_pushf(arena, list, "nil");
    break;
  case ExprKind_StringLiteral:
    str8_list_pushf(arena, list, "%.*s", str8_varg(e->literal.string));
    break;
  case ExprKind_IntegerLiteral:
    str8_list_pushf(arena, list, "%llu", e->literal.integer);
    break;
  case ExprKind_FloatLiteral:
    str8_list_pushf(arena, list, "%.2f", e->literal.floating);
    break;
  case ExprKind_BoolLiteral:
    str8_list_pushf(arena, list, e->literal.boolean ? "true" : "false");
    break;
  case ExprKind_CharLiteral:
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
  case ExprKind_Group:
    str8_list_pushf(arena, list, "(group ");
    print_expr(arena, list, indent, e->group.expr);
    str8_list_pushf(arena, list, ")");
    break;
  case ExprKind_Cast:
    str8_list_pushf(arena, list, "(cast ");
    print_type(arena, list, indent, e->cast.type);
    str8_list_pushf(arena, list, " ");
    print_expr(arena, list, indent, e->cast.expr);
    str8_list_pushf(arena, list, ")");
    break;
  case ExprKind_Call:
    str8_list_pushf(arena, list, "(call ");
    print_expr(arena, list, indent, e->call.expr);
    for (Expr **it = e->call.args.v; it != e->call.args.v + e->call.args.count; it += 1)
    {
      str8_list_pushf(arena, list, " ");
      print_expr(arena, list, indent, *it);
    }
    str8_list_pushf(arena, list, ")");
    break;
  case ExprKind_Index:
    str8_list_pushf(arena, list, "(index ");
    print_expr(arena, list, indent, e->index.expr);
    str8_list_pushf(arena, list, " ");
    print_expr(arena, list, indent, e->index.index);
    str8_list_pushf(arena, list, ")");
    break;
  case ExprKind_Field:
    str8_list_pushf(arena, list, "(field ");
    print_expr(arena, list, indent, e->field.expr);
    str8_list_pushf(arena, list, " ");
    str8_list_pushf(arena, list, "%.*s", str8_varg(e->field.name));
    str8_list_pushf(arena, list, ")");
    break;
  case ExprKind_Compound:
    str8_list_pushf(arena, list, "(compound ");
    print_type(arena, list, indent, e->compound.type);
    for each_index(i, e->compound.args.count)
    {
      Compound_Field *arg = e->compound.args.v[i];
      str8_list_pushf(arena, list, " ");
      switch (arg->kind)
      {
      case CompoundFieldKind_None:
        break;
      case CompoundFieldKind_Name:
      {
        if (arg->name.count > 0)
        {
          str8_list_pushf(arena, list, "%.*s=", str8_varg(arg->name));
        }
        break;
      }
      case CompoundFieldKind_Index:
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
  case ExprKind_SizeOf:
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
  case StmtKind_Null:
    str8_list_pushf(arena, list, "<NULL>");
    break;
  case StmtKind_Block:
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
  case StmtKind_If:
    str8_list_pushf(arena, list, "(if ");
    print_expr(arena, list, indent, s->if0.cond);
    (*indent)++;
    print_ln(arena, list, indent);
    print_stmt(arena, list, indent, s->if0.then_block);
    (*indent)--;
    print_ln(arena, list, indent);
    if (s->if0.else_stmt != NULL)
    {
      if (s->if0.else_stmt->kind == StmtKind_Block)
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
        assert(s->if0.else_stmt->kind == StmtKind_If);
        str8_list_pushf(arena, list, "(else ");
        print_stmt(arena, list, indent, s->if0.else_stmt);
        str8_list_pushf(arena, list, ")");
      }
    }
    break;
  case StmtKind_DoWhile:
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
  case StmtKind_While:
    str8_list_pushf(arena, list, "(while ");
    print_expr(arena, list, indent, s->while0.cond);
    (*indent)++;
    print_ln(arena, list, indent);
    print_stmt(arena, list, indent, s->while0.body);
    (*indent)--;
    str8_list_pushf(arena, list, ")");
    break;
  case StmtKind_For:
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
  case StmtKind_ForIn:
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
  case StmtKind_Switch:
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
  case StmtKind_Return:
    str8_list_pushf(arena, list, "(return");
    if (s->return_expr)
    {
      str8_list_pushf(arena, list, " ");
      print_expr(arena, list, indent, s->return_expr);
    }
    str8_list_pushf(arena, list, ")");
    break;
  case StmtKind_Break:
    str8_list_pushf(arena, list, "(break)");
    break;
  case StmtKind_Continue:
    str8_list_pushf(arena, list, "(continue)");
    break;
  case StmtKind_Expr:
    print_expr(arena, list, indent, s->expr);
    break;
  case StmtKind_Decl:
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
