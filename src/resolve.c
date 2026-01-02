#include "string.h"
#include "arena.h"
#include "parser.h"

ENUM(Type_Kind)
{
  TYPE_INT,
  TYPE_FLOAT,
  TYPE_PTR,
  TYPE_ARRAY,
  TYPE_STRUCT,
  TYPE_UNION,
  TYPE_PROC,
};

STRUCT(Type_Field);
STRUCT(Type_Field_Array)
{
  Type_Field *v;
  u64 count;
};

STRUCT(Type);
typedef Type *Type_Param;

STRUCT(Type_Param_Array)
{
  Type_Param *v;
  u64 count;
};

STRUCT(Type)
{
  Type_Kind kind;

  union
  {
    struct
    {
      Type *base;
    }
    ptr;

    struct
    {
      Type *base;
      u64 length;
    }
    array;

    struct
    {
      Type_Field_Array fields;
    }
    aggregate;

    struct
    {
      Type_Param_Array params;
      Type *ret;
    }
    proc;
  };
};

STRUCT(Type_Field)
{
  String8 name;
  Type   *type;
};

// STRUCT(Type_Field_Node)
// {
//   Type_Field_Node *next;
//   Type_Field v;
// };

// STRUCT(Type_Field_List)
// {
  
//   u64 count;
// };

Arena *sym_arena;

internal Type *
type_alloc(Type_Kind kind)
{
  Type *t = push_struct(sym_arena, Type);
  t->kind = kind;
  return t;
}

Type type_int_val = { .kind = TYPE_INT };
Type type_float_val = { .kind = TYPE_FLOAT };

Type *type_int = &type_int_val;
Type *type_float = &type_float_val;

STRUCT(Cached_Ptr_Type)
{
  Cached_Ptr_Type *next;
  Type v;
};

STRUCT(Cached_Ptr_Type_List)
{
  Cached_Ptr_Type *first;
  Cached_Ptr_Type *last;
  u64 count;
};

// TODO: hash table
Cached_Ptr_Type_List cached_ptr_types;

internal Type *
type_ptr(Type *base)
{
  for (Cached_Ptr_Type *it = cached_ptr_types.first;
       it != 0;
       it = it->next)
  {
    if (it->v.ptr.base == base)
    {
      return &it->v;
    }
  }

  // Type *t = type_alloc(TYPE_PTR);
  // t->ptr.base = base;
  Type t = (Type){ .kind = TYPE_PTR, .ptr.base = base };

  Cached_Ptr_Type *cached = push_struct(sym_arena, Cached_Ptr_Type);
  cached->v = t;

  sll_queue_push(cached_ptr_types.first, cached_ptr_types.last, cached);

  return &cached->v;
}

STRUCT(Cached_Array_Type)
{
  Cached_Array_Type *next;
  Type v;
};

STRUCT(Cached_Array_Type_List)
{
  Cached_Array_Type *first;
  Cached_Array_Type *last;
  u64 count;
};

// TODO: hash table
Cached_Array_Type_List cached_array_types;

internal Type *
type_array(Type *base, u64 length)
{
  for (Cached_Array_Type *it = cached_array_types.first;
       it != 0;
       it = it->next)
  {
    if (it->v.array.base == base && it->v.array.length == length)
    {
      return &it->v;
    }
  }

  // Type *t = type_alloc(TYPE_PTR);
  // t->ptr.base = base;
  Type t = (Type){ .kind = TYPE_ARRAY, .array.base = base, .array.length = length };

  Cached_Array_Type *cached = push_struct(sym_arena, Cached_Array_Type);
  cached->v = t;

  sll_queue_push(cached_array_types.first, cached_array_types.last, cached);

  return &cached->v;
}

STRUCT(Cached_Proc_Type)
{
  Cached_Proc_Type *next;
  Type v;
};

STRUCT(Cached_Proc_Type_List)
{
  Cached_Proc_Type *first;
  Cached_Proc_Type *last;
  u64 count;
};

// TODO: hash table
Cached_Proc_Type_List cached_proc_types;

internal Type *
type_proc(Type_Param *params, u64 num_params, Type *ret)
{
  for (Cached_Proc_Type *it = cached_proc_types.first;
       it != 0;
       it = it->next)
  {
    if (it->v.proc.params.count == num_params && it->v.proc.ret == ret)
    {
      for (u32 i = 0; i < num_params; i += 1)
      {
        Type_Param a = it->v.proc.params.v[i];
        Type_Param b = params[i];
        if (a != b)
        {
          goto next;
        }
      }
      return &it->v;
    }
  next: ;
  }

  Type t = (Type){ .kind = TYPE_PROC };
  t.proc.params.v = push_array_nz(sym_arena, Type_Param, num_params);
  t.proc.params.count = num_params;
  t.proc.ret = ret;
  mem_copy(t.proc.params.v, params, num_params * sizeof(Type_Param));

  Cached_Proc_Type *cached = push_struct(sym_arena, Cached_Proc_Type);
  cached->v = t;

  sll_queue_push(cached_proc_types.first, cached_proc_types.last, cached);

  return &cached->v;
}

internal Type *
type_struct(Type_Field *fields, u64 num_fields)
{
  Type *t = type_alloc(TYPE_STRUCT);
  t->aggregate.fields.v = push_array_nz(sym_arena, Type_Field, num_fields);
  t->aggregate.fields.count = num_fields;

  mem_copy(t->aggregate.fields.v, fields, num_fields * sizeof(Type_Field));

  return t;
}

internal Type *
type_union(Type_Field *fields, u64 num_fields)
{
  Type *t = type_alloc(TYPE_UNION);
  t->aggregate.fields.v = push_array_nz(sym_arena, Type_Field, num_fields);
  t->aggregate.fields.count = num_fields;

  mem_copy(t->aggregate.fields.v, fields, num_fields * sizeof(Type_Field));

  return t;
}

STRUCT(Const_Entity)
{
  Type *type;
  union
  {
    u64 int_value;
    f64 float_value;
  };
};

// typedef struct Entity Entity;

// ENUM(Entity_Kind)
// {
// };

// STRUCT(Entity)
// {
//   Entity_Kind kind;
//   union
//   {
//   };
// };

ENUM(Sym_State)
{
  SYM_UNRESOLVED,
  SYM_RESOLVING,
  SYM_RESOLVED,
};

STRUCT(Sym)
{
  Sym *next;

  String8    name;
  Decl      *decl;
  Sym_State  state;
};

STRUCT(Sym_List)
{
  Sym *first;
  Sym *last;
  u64 count;
};

// TODO: hash table
Sym_List sym_list;

internal Sym *
sym_get(String8 name)
{
  for (Sym *it = sym_list.first; it != NULL; it = it->next)
  {
    // TODO: intern string
    if (str8_equal(it->name, name))
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

  Sym *sym = push_struct(sym_arena, Sym);
  sym->name  = decl->name;
  sym->decl  = decl;
  sym->state = SYM_UNRESOLVED;

  sll_queue_push(sym_list.first, sym_list.last, sym);
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

  Type *int_ptr = type_ptr(type_int);
  assert(type_ptr(type_int) == int_ptr);

  Type *float_ptr = type_ptr(type_float);
  assert(type_ptr(type_float) == float_ptr);
  assert(int_ptr != float_ptr);

  Type *int_ptr_ptr = type_ptr(type_ptr(type_int));
  assert(type_ptr(type_ptr(type_int)) == int_ptr_ptr);

  Type *float4_array = type_array(type_float, 4);
  assert(type_array(type_float, 4) == float4_array);

  Type *another_float4_array = type_array(type_float, 4);
  assert(another_float4_array == float4_array);

  Type *float3_array = type_array(type_float, 3);
  assert(type_array(type_float, 3) == float3_array);
  assert(float4_array != float3_array);

  Type *int_proc_int = type_proc(&type_int, 1, type_int);
  assert(type_proc(&type_int, 1, type_int) == int_proc_int);

  Type *int_proc_int2 = type_proc(&type_int, 1, type_int);
  assert(int_proc_int == int_proc_int2);

  Type *proc_int = type_proc(NULL, 0, type_int);
  assert(proc_int != int_proc_int);
  assert(proc_int == type_proc(NULL, 0, type_int));

  printf("resolve test OK\n");
}
