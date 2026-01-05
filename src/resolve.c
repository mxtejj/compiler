#include "string.h"
#include "arena.h"
#include "parser.h"
#include "print.h"

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

ENUM(Type_Kind)
{
  TYPE_NONE,
  TYPE_INCOMPLETE,
  TYPE_COMPLETING,
  TYPE_INT,
  TYPE_FLOAT,
  TYPE_PTR,
  TYPE_ARRAY,
  TYPE_STRUCT,
  TYPE_UNION,
  TYPE_ENUM,
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

STRUCT(Entity);

STRUCT(Type)
{
  Type_Kind kind;
  usize size;
  usize align;
  Entity *entity;

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

internal void complete_type(Type *type);

internal Type *
type_alloc(Type_Kind kind)
{
  if (!sym_arena) sym_arena = arena_alloc(GB(1), MB(1), 0);
  Type *t = push_struct(sym_arena, Type);
  t->kind = kind;
  return t;
}

Type type_int_val   = { .kind = TYPE_INT, .size = 4 };
Type type_float_val = { .kind = TYPE_FLOAT, .size = 4 };

Type *type_int   = &type_int_val;
Type *type_float = &type_float_val;
const usize PTR_SIZE = 8;

internal usize
type_size_of(Type *type)
{
  assert(type->kind > TYPE_COMPLETING);
  assert(type->size != 0);
  return type->size;
}

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
  t.size = PTR_SIZE;

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

  complete_type(base);

  // Type *t = type_alloc(TYPE_PTR);
  // t->ptr.base = base;
  Type t = (Type){ .kind = TYPE_ARRAY, .array.base = base, .array.length = length };
  t.size = length * type_size_of(base);

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
type_proc(Type_Param_Array params, Type *ret)
{
  for (Cached_Proc_Type *it = cached_proc_types.first;
       it != 0;
       it = it->next)
  {
    if (it->v.proc.params.count == params.count && it->v.proc.ret == ret)
    {
      b32 match = true;
      for (u32 i = 0; i < params.count; i += 1)
      {
        Type_Param a = it->v.proc.params.v[i];
        Type_Param b = params.v[i];
        if (a != b)
        {
          match = false;
          break;
        }
      }
      if (match)
      {
        return &it->v;
      }
    }
  }

  // TODO: do we need to copy?
  Type t = (Type){ .kind = TYPE_PROC };
  t.size = PTR_SIZE;
  t.proc.params.v = push_array_nz(sym_arena, Type_Param, params.count);
  t.proc.params.count = params.count;
  t.proc.ret = ret;
  mem_copy(t.proc.params.v, params.v, params.count * sizeof(Type_Param));

  Cached_Proc_Type *cached = push_struct(sym_arena, Cached_Proc_Type);
  cached->v = t;

  sll_queue_push(cached_proc_types.first, cached_proc_types.last, cached);

  return &cached->v;
}

internal void
type_complete_struct(Type *type, Type_Field_Array fields)
{
  assert(type->kind == TYPE_COMPLETING);
  type->kind = TYPE_STRUCT;
  type->size = 0;
  for (Type_Field *it = fields.v; it != fields.v + fields.count; it += 1)
  {
    // TODO: alignment etc
    type->size += type_size_of(it->type);
  }

  type->aggregate.fields.v = push_array_nz(sym_arena, Type_Field, fields.count);
  type->aggregate.fields.count = fields.count;

  mem_copy(type->aggregate.fields.v, fields.v, fields.count * sizeof(Type_Field));
}

internal void
type_complete_union(Type *type, Type_Field_Array fields)
{
  assert(type->kind == TYPE_COMPLETING);
  type->kind = TYPE_UNION;
  type->size = 0;
  for (Type_Field *it = fields.v; it != fields.v + fields.count; it += 1)
  {
    assert(it->type->kind > TYPE_COMPLETING);
    type->size = max(type->size, type_size_of(it->type));
  }

  type->aggregate.fields.v = push_array_nz(sym_arena, Type_Field, fields.count);
  type->aggregate.fields.count = fields.count;

  mem_copy(type->aggregate.fields.v, fields.v, fields.count * sizeof(Type_Field));
}

internal Type *
type_incomplete(Entity *en)
{
  Type *type = type_alloc(TYPE_INCOMPLETE);
  type->entity = en;
  return type;
}

ENUM(Entity_Kind)// 176
{
  ENTITY_NONE,
  ENTITY_VAR,
  ENTITY_CONST,
  ENTITY_PROC,
  ENTITY_TYPE,
  ENTITY_ENUM_CONST,
  ENTITY_TYPEDEF,
};

ENUM(Entity_State)
{
  ENTITY_UNRESOLVED,
  ENTITY_RESOLVING,
  ENTITY_RESOLVED,
};

STRUCT(Entity)
{
  String8       name;
  Entity_Kind   kind;
  Entity_State  state;
  Decl         *decl;
  Type         *type;
  s64           const_value;
};

STRUCT(Entity_Node)
{
  Entity_Node *next;
  Entity *v;
};

STRUCT(Entity_List)
{
  Entity_Node *first;
  Entity_Node *last;
  u64 count;
};

Arena *resolve_arena;
Entity_List entities;

internal void
entity_list_push(Entity_List *list, Entity *en)
{
  Entity_Node *node = push_struct(resolve_arena, Entity_Node);
  node->v = en;
  sll_queue_push(list->first, list->last, node);
  list->count += 1;
}

internal Entity *
entity_alloc(Entity_Kind kind, String8 name, Decl *decl) // #202
{
  if (resolve_arena == NULL)
  {
    resolve_arena = arena_alloc(GB(1), MB(1), 0);
  }

  // assert(decl);

  Entity *en = push_struct(resolve_arena, Entity);
  en->kind = kind;
  en->name = name;
  en->decl = decl;

  return en;
}

internal Entity *
entity_decl(Decl *decl)
{
  Entity_Kind kind = ENTITY_NONE;
  switch (decl->kind)
  {
  case DECL_STRUCT:
  case DECL_UNION:
  case DECL_TYPEDEF:
  case DECL_ENUM: // TODO
    kind = ENTITY_TYPE;
    break;
  case DECL_VAR:
    kind = ENTITY_VAR;
    break;
  case DECL_CONST:
    kind = ENTITY_CONST;
    break;
  case DECL_PROC:
    kind = ENTITY_PROC;
    break;
    // kind = ENTITY_TYPE;
  // case DECL_ENUM:
    // kind = ENTITY_ENUM_CONST;
    // break;
  default:
    // assert(0);
    break;
  }

  Entity *en = entity_alloc(kind, decl->name, decl);
  if (decl->kind == DECL_STRUCT || decl->kind == DECL_UNION)
  {
    en->state = ENTITY_RESOLVED;
    en->type  = type_incomplete(en);
  }
  return en;
}

internal Entity *
entity_enum_const(String8 name, Decl *decl)
{
  return entity_alloc(ENTITY_ENUM_CONST, name, decl);
}

internal Entity *
entity_get(String8 name)
{
  for (Entity_Node *it = entities.first; it != 0; it = it->next)
  {
    if (str8_equal(it->v->name, name))
    {
      return it->v;
    }
  }
  return NULL;
}

internal Entity *
entity_install_decl(Decl *decl)
{
  Entity *en = entity_decl(decl);
  entity_list_push(&entities, en);
  if (decl->kind == DECL_ENUM)
  {
    for (Enum_Member *it = decl->enum0.members.first;
         it != 0;
         it = it->next)
    {
      entity_list_push(&entities, entity_enum_const(it->name, decl));
    }
  }
  return en;
}

internal Entity *
entity_install_type(String8 name, Type *type)
{
  Entity *en = entity_alloc(ENTITY_TYPE, name, NULL);
  en->state = ENTITY_RESOLVED;
  en->type  = type;
  entity_list_push(&entities, en);
  return en;
}

STRUCT(Resolved_Expr)
{
  Type *type;
  b32 is_lvalue;
  b32 is_const;
  s64 const_value;
};

read_only global Resolved_Expr nil_resolved_expr;

internal Resolved_Expr
resolved_rvalue(Type *type)
{
  return (Resolved_Expr){ .type = type };
}

internal Resolved_Expr
resolved_lvalue(Type *type)
{
  return (Resolved_Expr){ .type = type, .is_lvalue = true };
}

internal Resolved_Expr
resolved_const(s64 const_value)
{
  return (Resolved_Expr){ .type = type_int, .is_const = true, .const_value = const_value };
}

internal Entity *resolve_name(String8 name);
internal s64 resolve_int_const_expr(Expr *expr);
internal Resolved_Expr resolve_expr(Expr *expr);

internal Type *
resolve_typespec(Type_Spec *typespec)
{
  switch (typespec->kind)
  {
  case TYPE_SPEC_NULL:
    break;
  case TYPE_SPEC_NAME:
  {
    Entity *en = resolve_name(typespec->name);
    if (en->kind != ENTITY_TYPE)
    {
      fatal("%.*s must denote a type", str8_varg(typespec->name));
      return NULL;
    }
    return en->type;
  }
  case TYPE_SPEC_PROC:
  {
    assert(typespec->proc.param_count > 0);
    Type_Param_Array params = {0};
    params.count = typespec->proc.param_count;
    params.v = push_array_nz(resolve_arena, Type_Param, params.count);
    // @CLEANUP
    u32 i = 0;
    for (Type_Spec *it = typespec->proc.params.first;
         it != 0;
         it = it->next, i += 1)
    {
      params.v[i] = resolve_typespec(it);
    }
    // for (u32 i = 0; i < params.count; i += 1)
    // {
    //   Type_Param *param = push_struct(resolve_arena, Type_Param);
    //   resolve_typespec(typespec->proc.params)
    //   params.v[i] = push_struct(resolve_arena, param);
    // }
    Type *ret = NULL;//type_void;
    if (typespec->proc.ret)
    {
      ret = resolve_typespec(typespec->proc.ret);
    }
    return type_proc(params, ret);
  }
  case TYPE_SPEC_ARRAY:
    return type_array(resolve_typespec(typespec->array.elem), resolve_int_const_expr(typespec->array.count));
  // case TYPE_SPEC_SLICE:
    // break;
  case TYPE_SPEC_PTR:
    return type_ptr(resolve_typespec(typespec->ptr.pointee));
  default:
    assert(0);
    return NULL;
  }

  assert(!"Unreachable");
  return NULL;
}

Entity_List ordered_entities;

internal void
complete_type(Type *type)
{
  if (type->kind == TYPE_COMPLETING)
  {
    fatal("Type completion cycle");
    return;
  }
  else if (type->kind != TYPE_INCOMPLETE)
  {
    return;
  }
  type->kind = TYPE_COMPLETING;
  Decl *decl = type->entity->decl;

  assert(decl->kind == DECL_STRUCT || decl->kind == DECL_UNION);

  u32 total_field_count = 0;
  for (Aggr_Field *it = decl->aggr.fields.first;
       it != 0;
       it = it->next)
  {
    total_field_count += it->names.node_count;
  }

  if (total_field_count == 0)
  {
    fatal("No fields");
  }

  printf("total_field_count: %lu\n", total_field_count);

  Type_Field_Array fields = {0};
  fields.count = total_field_count;
  fields.v = push_array(resolve_arena, Type_Field, fields.count);

  assert(decl->aggr.fields.count > 0);

  // @CLEANUP
  u32 i = 0;
  for (Aggr_Field *it = decl->aggr.fields.first;
       it != 0;
       it = it->next)
  {
    Type *field_type = resolve_typespec(it->type);
    complete_type(field_type);

    u32 j = 0;
    for (String8Node *name = it->names.first;
         name != 0;
         name = name->next, j += 1)
    {
      fields.v[i+j] = (Type_Field){
        .name = name->string,
        .type = field_type,
      };
    }
  }

  if (decl->kind == DECL_STRUCT)
  {
    type_complete_struct(type, fields);
  }
  else
  {
    assert(decl->kind == DECL_UNION);
    type_complete_union(type, fields);
  }

  entity_list_push(&ordered_entities, type->entity);
}

internal Type *
resolve_decl_type(Decl *decl)
{
  // TODO add typedef
  assert(decl->kind == DECL_TYPEDEF);
  return resolve_typespec(decl->typedef0.type);
}

internal Type *
resolve_decl_var(Decl *decl)
{
  assert(decl->kind == DECL_VAR);
  Type *type = NULL;
  if (decl->var.type)
  {
    type = resolve_typespec(decl->var.type);
  }
  if (decl->var.expr)
  {
    Resolved_Expr result = resolve_expr(decl->var.expr);
    if (type && result.type != type)
    {
      fatal("Declared var type does not match inferred type");
    }
    type = result.type;
  }
  complete_type(type);
  return type;
}

internal Type *
resolve_decl_const(Decl *decl, s64 *const_value)
{
  assert(decl->kind == DECL_CONST);
  Resolved_Expr result = resolve_expr(decl->const0.expr);
  *const_value = result.const_value;
  return result.type;
}

internal void
resolve_entity(Entity *en)
{
  if (en->state == ENTITY_RESOLVED)
  {
    return;
  }
  else if (en->state == ENTITY_RESOLVING)
  {
    fatal("Cyclic dependency");
    return;
  }

  assert(en->state == ENTITY_UNRESOLVED);
  en->state = ENTITY_RESOLVING;

  switch (en->kind)
  {
  case ENTITY_TYPE:
    en->type = resolve_decl_type(en->decl);
    break;
  case ENTITY_VAR:
    en->type = resolve_decl_var(en->decl);
    break;
  case ENTITY_CONST:
    en->type = resolve_decl_const(en->decl, &en->const_value);
    break;
  case ENTITY_TYPEDEF:
    en->type = resolve_decl_type(en->decl);
    break;
  default:
    assert(0);
    break;
  }

  en->state = ENTITY_RESOLVED;
  entity_list_push(&ordered_entities, en);
}

internal void
complete_entity(Entity *en)
{
  resolve_entity(en);
  if (en->kind == ENTITY_TYPE)
  {
    complete_type(en->type);
  }
}

internal Entity *
resolve_name(String8 name)
{
  Entity *en = entity_get(name);
  if (!en)
  {
    fatal("Non-existent name '%.*s'", str8_varg(name));
    return NULL;
  }
  resolve_entity(en);
  return en;
}

internal Resolved_Expr
resolve_expr_field(Expr *expr)
{
  assert(expr->kind == EXPR_FIELD);
  Resolved_Expr left = resolve_expr(expr->field.expr);
  Type *type = left.type;
  complete_type(type);
  if (type->kind != TYPE_STRUCT && type->kind != TYPE_UNION)
  {
    fatal("Can only access fields on aggregate types");
    return nil_resolved_expr;
  }
  for (Type_Field *it = type->aggregate.fields.v;
       it != type->aggregate.fields.v + type->aggregate.fields.count;
       it += 1)
  {
    if (str8_equal(it->name, expr->field.name))
    {
      return left.is_lvalue ? resolved_lvalue(it->type) : resolved_rvalue(it->type);
    }
  }
  fatal("No field named '%.*s'", str8_varg(expr->field.name));
  return nil_resolved_expr;
}

internal Resolved_Expr
resolve_expr_name(Expr *expr)
{
  assert(expr->kind == EXPR_IDENT);
  Entity *en = resolve_name(expr->ident);

  switch (en->kind)
  {
  case ENTITY_VAR:
    return resolved_lvalue(en->type);
  case ENTITY_CONST:
    return resolved_const(en->const_value);
  // TODO: PROC
  default:
    fatal("%.*s must be a var or const", str8_varg(expr->ident));
    return nil_resolved_expr;
  }

  assert(!"Unreachable");
  return nil_resolved_expr;
}

internal Resolved_Expr
resolve_expr_unary(Expr *expr)
{
  assert(expr->kind == EXPR_UNARY);
  Resolved_Expr operand = resolve_expr(expr->unary.right);
  Type *type = operand.type;
  switch (expr->unary.op.kind)
  {
  case TOKEN_DEREF:
    if (type->kind != TYPE_PTR)
    {
      fatal("Cannot dereference non-pointer type");
    }
    return resolved_lvalue(type->ptr.base);
  case '&':
    if (!operand.is_lvalue)
    {
      fatal("Cannot take address of non-lvalue");
    }
    return resolved_rvalue(type_ptr(type));
  default:
    assert(0);
    return nil_resolved_expr;
  }

  assert(!"Unreachable");
  return nil_resolved_expr;
}

internal Resolved_Expr
resolve_expr_binary(Expr *expr)
{
  assert(expr->kind == EXPR_BINARY);
  assert(expr->binary.op.kind == '+');

  Resolved_Expr left  = resolve_expr(expr->binary.left);
  Resolved_Expr right = resolve_expr(expr->binary.right);

  if (left.type != type_int)
  {
    fatal("left operand of + must be int");
  }
  if (left.type != right.type)
  {
    fatal("left and right operand of + must have same type");
  }
  if (left.is_const && right.is_const)
  {
    return resolved_const(left.const_value + right.const_value);
  }
  return resolved_rvalue(left.type);
}

internal Resolved_Expr
resolve_expr(Expr *expr)
{
  switch (expr->kind)
  {
  case EXPR_INTEGER_LITERAL:
    return resolved_const(expr->literal.integer);
  case EXPR_IDENT:
    return resolve_expr_name(expr);
  case EXPR_FIELD:
    return resolve_expr_field(expr);
  case EXPR_UNARY:
    return resolve_expr_unary(expr);
  case EXPR_BINARY:
    return resolve_expr_binary(expr);
  case EXPR_SIZE_OF_EXPR:
  {
    Resolved_Expr result = resolve_expr(expr->size_of_expr);
    Type *type = result.type;
    complete_type(type);
    return resolved_const(type_size_of(type));
  }
  case EXPR_SIZE_OF_TYPE:
  {
    Type *type = resolve_typespec(expr->size_of_type);
    complete_type(type);
    return resolved_const(type_size_of(type));
  }
  default:
    assert(0);
    return nil_resolved_expr;
  }

  assert(!"Unreachable");
  return nil_resolved_expr;
}

internal s64
resolve_int_const_expr(Expr *expr)
{
  Resolved_Expr result = resolve_expr(expr);
  if (!result.is_const)
  {
    fatal("Expected constant expression");
  }
  return result.const_value;
}

internal void
resolve_test()
{
  entity_install_type(str8_lit("int"), type_int);

  String8 source = S(
    "const N = 1 + size_of(p);\n" // 9
    "var p: *T;\n" // 4
    "var u = p.*;\n" // deref
    "struct T { a: [N]int, }\n"
    "var r = &t.a;\n"
    "var t: T;\n"
    "typedef S = [N+M]int;\n"
    "const M = size_of(t.a);\n" // 36
    "var i = N+M;\n" // [9 + 36] = 45
    "var q = &i;\n"
  );

  Lexer l = lexer_init(source);
  Parser p = parser_init(&l);

  Decl_List list = parse_declarations(&p);
  for (Decl *it = list.first; it != NULL; it = it->next)
  {
    entity_install_decl(it);
  }

  for (Entity_Node *it = entities.first; it != 0; it = it->next)
  {
    complete_entity(it->v);
  }

  Arena_Temp scratch = arena_scratch_get(0, 0);

  for (Entity_Node *it = ordered_entities.first; it != 0; it = it->next)
  {
    Entity *en = it->v;

    String8 result;
    if (en->decl)
    {
      String8List list = {0};
      int indent = 0;
      print_decl(scratch.arena, &list, &indent, en->decl);
      result = str8_list_join(scratch.arena, &list, NULL);
    }
    else
    {
      result = en->name;
    }

    printf("%.*s\n", str8_varg(result));
  }

  arena_scratch_release(scratch);
}
