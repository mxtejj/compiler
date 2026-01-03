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
  SYM_UNORDERED,
  SYM_ORDERING,
  SYM_ORDERED,
};

ENUM(Sym_Kind)
{
  SYM_INVALID,
  SYM_DECL,
  SYM_BUILTIN,
  SYM_ENUM_CONST,
};

STRUCT(Sym)
{
  Sym *next;

  Sym_Kind   kind;
  String8    name;
  // Decl      *decl;
  Sym_State  state;
  // Resolved  *resolved;

  union
  {
    Decl *decl;
  };
};

STRUCT(Sym_List)
{
  Sym *first;
  Sym *last;
  u64 count;
};

// TODO: hash table
Sym_List sym_list;
Decl_List ordered_decls;

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
fatal(char *fmt, ...)
{
  Arena_Temp scratch = arena_scratch_get(0, 0);

  va_list args;
  va_start(args, fmt);
  String8 s = str8fv(scratch.arena, fmt, args);
  printf(CLR_RED "%.*s\n" CLR_RESET, str8_varg(s));
  va_end(args);

  arena_scratch_release(scratch);
}

internal void
order_decl(Decl *decl);

internal void
order_name(String8 name)
{
  Sym *sym = sym_get(name);
  if (!sym)
  {
    fatal("Non-existent name '%.*s'", str8_varg(name));
    return;
  }

  if (sym->state == SYM_ORDERED)
  {
    return;
  }

  if (sym->state == SYM_ORDERING)
  {
    fatal("Cyclic dependency");
    return;
  }

  sym->state = SYM_ORDERING;
  sym->state = SYM_ORDERED;

  if (sym->kind == SYM_DECL)
  {
    // @CLEANUP
    order_decl(sym->decl);
    sll_queue_push(ordered_decls.first, ordered_decls.last, sym->decl);
  }
  else if (sym->kind == SYM_ENUM_CONST)
  {
    order_name(sym->decl->name);
  }
  else
  {
    assert(0);
  }
}

internal void
order_typespec(Type_Spec *typespec);

internal void
order_expr(Expr *expr)
{
  if (!expr) return; // @CLEANUP

  switch (expr->kind)
  {
  case EXPR_IDENT:
    order_name(expr->ident);
    break;
  case EXPR_UNARY:
    order_expr(expr->unary.right);
    break;
  case EXPR_BINARY:
    order_expr(expr->binary.left);
    order_expr(expr->binary.right);
    break;
  case EXPR_TERNARY:
    order_expr(expr->ternary.cond);
    order_expr(expr->ternary.then);
    order_expr(expr->ternary.else_);
    break;
  case EXPR_NIL_LITERAL:
  case EXPR_STRING_LITERAL:
  case EXPR_INTEGER_LITERAL:
  case EXPR_FLOAT_LITERAL:
  case EXPR_BOOL_LITERAL:
    // Do nothing
    break;
  case EXPR_GROUP:
    order_expr(expr->group.expr);
    break;
  case EXPR_CAST:
    order_typespec(expr->cast.type);
    order_expr(expr->cast.expr);
    break;
  case EXPR_CALL:
    order_expr(expr->call.expr);
    for (Expr *it = expr->call.args.first;
         it != 0;
         it = it->next)
    {
      order_expr(it);
    }
    break;
  case EXPR_INDEX:
    order_expr(expr->index.expr);
    order_expr(expr->index.index);
    break;
  case EXPR_FIELD:
    order_expr(expr->field.expr);
    order_name(expr->field.name);
    break;
  case EXPR_COMPOUND:
    if (expr->compound.type)
    {
      order_typespec(expr->compound.type);
    }
    for (Compound_Arg *it = expr->compound.args.first;
         it != 0;
         it = it->next)
    {
      // TODO: name?
      // order_name(it->optional_name);
      order_expr(it->expr);
    }
    break;
  case EXPR_SIZE_OF_EXPR:
    order_expr(expr->size_of_expr);
    break;
  case EXPR_SIZE_OF_TYPE:
    order_typespec(expr->size_of_type);
    break;
  default:
    assert(0);
    break;
  }
}

internal void
order_typespec(Type_Spec *typespec)
{
  if (!typespec) return; // @CLEANUP

  switch (typespec->kind)
  {
  case TYPE_SPEC_NAME:
    order_name(typespec->name);
    break;
  case TYPE_SPEC_PROC:
    for (Type_Spec *param = typespec->proc.params.first;
         param != 0;
         param = param->next)
    {
      order_typespec(param);
    }
    order_typespec(typespec->proc.ret);
    break;
  case TYPE_SPEC_ARRAY:
    order_expr(typespec->array.count);
    order_typespec(typespec->array.elem);
    break;
  case TYPE_SPEC_SLICE:
    order_typespec(typespec->slice.elem);
    break;
  case TYPE_SPEC_PTR:
    // TODO: Think about forward declaration, etc
    break;
  default:
    assert(0);
    break;
  }
}

internal void
order_decl(Decl *decl)
{
  if (!decl) return; // @CLEANUP

  switch (decl->kind)
  {
  case DECL_PROC:
    // Do nothing
    break;
  case DECL_STRUCT:
  case DECL_UNION:
    for (Aggr_Field *it = decl->aggr.fields.first;
         it != 0;
         it = it->next)
    {
      // for (String8Node *name = it->names.first;
      //      name != 0;
      //      name = name->next)
      // {
      //   order_name(name->string);
      // }
      order_typespec(it->type);
    }
    break;
  case DECL_ENUM:
    for (Enum_Member *it = decl->enum0.members.first;
         it != 0;
         it = it->next)
    {
      if (it->value)
      {
        order_expr(it->value);
      }
    }
    break;
  case DECL_VAR:
    order_typespec(decl->var.type);
    order_expr(decl->var.expr);
    break;
  case DECL_CONST:
    order_expr(decl->const0.expr);
    break;
  default:
    assert(0);
    break;
  }
}

internal void
sym_put(Sym_Kind kind, String8 name, Sym_State state, Decl *decl)
{
  if (sym_arena == NULL)
  {
    sym_arena = arena_alloc(GB(1), MB(1), 0);
  }

  if (sym_get(name))
  {
    assert(!"Duplicate name");
  }

  Sym *sym = push_struct(sym_arena, Sym);
  sym->kind  = kind;
  sym->name  = name;
  sym->state = state;
  sym->decl  = decl;

  sll_queue_push(sym_list.first, sym_list.last, sym);
}

internal void
sym_builtin(String8 name)
{
  sym_put(SYM_BUILTIN, name, SYM_ORDERED, NULL);
}

internal void
sym_enum_const(String8 name, Decl *decl)
{
  sym_put(SYM_ENUM_CONST, name, SYM_UNORDERED, decl);
}

internal void
sym_decl(Decl *decl)
{
  sym_put(SYM_DECL, decl->name, SYM_UNORDERED, decl);
  if (decl->kind == DECL_ENUM)
  {
    for (Enum_Member *it = decl->enum0.members.first;
         it != 0;
         it = it->next)
    {
      sym_enum_const(it->name, decl);
    }
  }
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
  if (sym->state == SYM_ORDERED) return;
  if (sym->state == SYM_ORDERING)
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
#if 0
  assert(sym_get(str8_lit("foo")) == NULL);

  Decl decl = {0};
  decl.kind = DECL_CONST;
  decl.name = str8_lit("foo");

  Expr expr = {0};
  expr.kind = EXPR_INTEGER_LITERAL;
  expr.literal.integer = 42;

  decl.const0.expr = &expr;

  sym_decl(&decl);

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
#endif
}

internal void
order_test()
{
  // local_persist String8 decls_tests[] =
  // {
  //   S("var a = b;"),
  //   S("var b = 1;"),
  // };

  printf("\n");

  String8 source = S(
    // "var a = b;\n"
    // "var b = 1;\n"
    // "struct Person { name: string, }\n"

    // "struct S { t: T, }\n"
    // "struct T { i: [N]int, }\n"
    // "const N = 1024;\n"

    "struct S { t: [B]T, }\n"
    "struct T { s: *S, }\n"
    "enum E { A, B, C, }\n"
  );

  {
    sym_builtin(str8_lit("u8"));
    sym_builtin(str8_lit("u16"));
    sym_builtin(str8_lit("u32"));
    sym_builtin(str8_lit("u64"));
    sym_builtin(str8_lit("s8"));
    sym_builtin(str8_lit("s16"));
    sym_builtin(str8_lit("s32"));
    sym_builtin(str8_lit("s64"));
    sym_builtin(str8_lit("bool"));
    sym_builtin(str8_lit("string"));
    sym_builtin(str8_lit("uintptr"));
    sym_builtin(str8_lit("int"));
    sym_builtin(str8_lit("uint"));
    sym_builtin(str8_lit("f32"));
    sym_builtin(str8_lit("f64"));
  }

  Lexer l = lexer_init(source);
  Parser p = parser_init(&l);

  Decl_List list = parse_declarations(&p);
  for (Decl *it = list.first; it != NULL; it = it->next)
  {
    sym_decl(it);
  }

  // for (u32 i = 0; i < array_count(decls_tests); i += 1)
  // {
  //   String8 source = decls_tests[i];

  //   Decl *decl = parse_decl(&p);
  //   // sym_decl(decl);
  //   // resolve_decl(decl);
  //   sym_decl(decl);
  // }

  for (Sym *it = sym_list.first;
       it != 0;
       it = it->next)
  {
    // resolve_sym(it);
    order_name(it->name);
    // order_name(it->decl->name);
  }

  for (Decl *it = ordered_decls.first;
       it != 0;
       it = it->next)
  {
    printf("%.*s\n", str8_varg(it->name));
  }

  printf("order test OK\n");
}
