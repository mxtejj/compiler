#include "string.h"
#include "arena.h"
#include "parser.h"

typedef enum Sym_State Sym_State;
enum Sym_State
{
  SYM_UNRESOLVED,
  SYM_RESOLVING,
  SYM_RESOLVED,
};

typedef struct Sym_Node Sym_Node;
struct Sym_Node
{
  Sym_Node *next;
  Sym v;
};

typedef struct Sym Sym;
struct Sym
{
  String8    name;
  Decl      *decl;
  Sym_State  state;
};

typedef struct Sym_List Sym_List;
struct Sym_List
{
  Sym_Node *first;
  Sym_Node *last;
  u64 count;
};

// TODO: hash table
Arena *sym_arena;
Sym_List sym_list;

internal Sym *
sym_get(String8 name)
{
  for (Sym_Node *it = sym_list.first; it != NULL; it = it->next)
  {
    // TODO: intern string
    if (str8_equal(it->v.name, name))
    {
      return it;
    }
  }
  return NULL;
}

internal void
sym_put(Decl *decl)
{
  if (sym_arena == NULL)
  {
    sym_arena = arena_alloc(GB(1), MB(1), 0);
  }
  assert(decl->name.data);
  assert(!sym_get(decl->name));
  Sym_Node *node = push_struct(sym_arena, Sym_Node);
  node->v.name  = decl->name;
  node->v.decl  = decl;
  node->v.state = SYM_UNRESOLVED;
  sll_queue_push(sym_list.first, sym_list.last, node);
}

internal void
resolve_decl(Decl *decl)
{
  switch (decl->kind)
  {
  case DECL_CONST:
    // Const_Entity const_ent = resolve_constant_expr(decl->const0.expr);
    //https://youtu.be/0WpCnd9E-eg?list=PLU94OURih-CiP4WxKSMt3UcwMSDM3aTtX&t=3073

    break;
  default:
    break;
  }
}

internal void
resolve_sym(Sym *sym)
{
  if (sym->state == SYM_RESOLVED) return;
  if (sym->state == SYM_RESOLVING)
  {
    assert(!"Cyclic dependency");
    return;
  }

  resolve_decl(sym->decl);
}

internal Sym *
resolve_name(String8 name)
{
  Sym *sym = sym_get(name);
  assert(sym && "Unknown symbol name");
  resolve_sym(sym);
  return sym;
}

internal void
resolve_test()
{
  assert(sym_get(str8_lit("foo")) == NULL);

  Decl decl = {0};
  decl.kind = DECL_CONST;
  decl.name = str8_lit("foo");

  Expr expr = {0};
  expr.kind = EXPR_INTEGER_LITERAL;
  expr.literal.integer = 42;
  
  decl.const0.expr = &expr;

  sym_put(&decl);

  assert(sym_get(str8_lit("foo")) && sym_get(str8_lit("foo"))->decl == &decl);

  printf("resolve test OK\n");
}
