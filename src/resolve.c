#include "string.h"
#include "arena.h"
#include "parser.h"
#include "print.h"
#include "os.h"

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
  trap();
  os_exit(1);
}

ENUM(Type_Kind)
{
  TYPE_NONE,
  TYPE_INCOMPLETE,
  TYPE_COMPLETING,
  TYPE_VOID,
  TYPE_CHAR,
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

Arena *resolve_arena;

internal void complete_type(Type *type);

internal Type *
type_alloc(Type_Kind kind)
{
  if (!resolve_arena) resolve_arena = arena_alloc(GB(1), MB(1), 0);
  Type *t = push_struct(resolve_arena, Type);
  t->kind = kind;
  return t;
}

Type *type_void  = &(Type){ .kind = TYPE_VOID,  .size = 0 };
Type *type_char  = &(Type){ .kind = TYPE_CHAR,  .size = 1 };
Type *type_int   = &(Type){ .kind = TYPE_INT,   .size = 4 };
Type *type_float = &(Type){ .kind = TYPE_FLOAT, .size = 4 };
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

  Cached_Ptr_Type *cached = push_struct(resolve_arena, Cached_Ptr_Type);
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

  Cached_Array_Type *cached = push_struct(resolve_arena, Cached_Array_Type);
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
  t.proc.params.v = push_array_nz(resolve_arena, Type_Param, params.count);
  t.proc.params.count = params.count;
  t.proc.ret = ret;
  mem_copy(t.proc.params.v, params.v, params.count * sizeof(Type_Param));

  Cached_Proc_Type *cached = push_struct(resolve_arena, Cached_Proc_Type);
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
  // for (Type_Field *it = fields.v; it != fields.v + fields.count; it += 1)
  for each_index(i, fields.count)
  {
    Type_Field it = fields.v[i];
    // TODO: alignment etc
    type->size += type_size_of(it.type);
  }

  type->aggregate.fields.v = push_array_nz(resolve_arena, Type_Field, fields.count);
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

  type->aggregate.fields.v = push_array_nz(resolve_arena, Type_Field, fields.count);
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
    for each_index(i, decl->enum0.members.count)
    {
      Enum_Member it = decl->enum0.members.v[i];
      entity_list_push(&entities, entity_enum_const(it.name, decl));
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
internal s64 resolve_const_expr(Expr *expr);
internal Resolved_Expr resolve_expr(Expr *expr);
internal Resolved_Expr resolve_expected_expr(Expr *expr, Type *expected_type);

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
    assert(typespec->proc.params.count > 0);
    Type_Param_Array params = {0};
    params.count = typespec->proc.params.count;
    params.v = push_array_nz(resolve_arena, Type_Param, params.count);
    // @CLEANUP
    for each_index(i, params.count)
    {
      Type_Spec *it = typespec->proc.params.v[i];
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
    return type_array(resolve_typespec(typespec->array.elem), resolve_const_expr(typespec->array.count));
  // case TYPE_SPEC_SLICE:
    // break;
  case TYPE_SPEC_PTR:
    return type_ptr(resolve_typespec(typespec->ptr.pointee));
  default:
    assert(0);
    return NULL;
  }

  // assert(!"Unreachable");
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
  for each_index(i, decl->aggr.fields.count)
  {
    Aggr_Field it = decl->aggr.fields.v[i];
    total_field_count += it.names.node_count;
  }

  if (total_field_count == 0)
  {
    fatal("No fields");
  }

  Type_Field_Array fields = {0};
  fields.count = total_field_count;
  fields.v = push_array(resolve_arena, Type_Field, fields.count);

  assert(decl->aggr.fields.count > 0);

  for each_index(i, decl->aggr.fields.count)
  {
    Aggr_Field it = decl->aggr.fields.v[i];
    Type *field_type = resolve_typespec(it.type);
    complete_type(field_type);

    u32 j = 0;
    for each_node(name, String8Node, it.names.first)
    {
      fields.v[i+j] = (Type_Field){
        .name = name->string,
        .type = field_type,
      };
      j += 1;
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
    Resolved_Expr result = resolve_expected_expr(decl->var.expr, type);
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
  if (!result.is_const)
  {
    fatal("Initializer for const is not a constant expression");
  }
  *const_value = result.const_value;
  return result.type;
}

internal Type *
resolve_decl_proc(Decl *decl)
{
  assert(decl->kind == DECL_PROC);

  // TODO: make decl.proc.params be a array
  u64 params_count = 0;
  for (Proc_Param *it = decl->proc.params.first;
       it != 0;
       it = it->next)
  {
    params_count += 1;
  }

  Type_Param_Array param_types = {0};
  param_types.count = params_count;
  param_types.v = push_array(resolve_arena, Type_Param, param_types.count);

  u64 i = 0;
  for (Proc_Param *it = decl->proc.params.first;
       it != 0;
       it = it->next)
  {
    param_types.v[i] = resolve_typespec(it->type);
    i += 1;
  }

  return type_proc(param_types, resolve_typespec(decl->proc.ret));
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
  // case ENTITY_TYPEDEF:
    // en->type = resolve_decl_type(en->decl);
    // break;
  case ENTITY_PROC:
    en->type = resolve_decl_proc(en->decl);
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
ptr_decay(Resolved_Expr expr)
{
  // TODO: dont do this
  if (expr.type->kind == TYPE_ARRAY)
  {
    return resolved_rvalue(type_ptr(expr.type->array.base));
  }
  return expr;
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
  case ENTITY_PROC:
    return resolved_rvalue(en->type);
  default:
    fatal("%.*s must be a var or const", str8_varg(expr->ident));
    return nil_resolved_expr;
  }

  assert(!"Unreachable");
  return nil_resolved_expr;
}

internal s64
eval_int_unary(Token_Kind op, s64 value)
{
  switch (op)
  {
  case '+':
    return +value;
  case '-':
    return -value;
  case '!':
    return !value;
  case '~':
    return ~value;
  default:
    assert(0);
    break;
  }
  return 0;
}

internal Resolved_Expr
resolve_expr_unary(Expr *expr)
{
  assert(expr->kind == EXPR_UNARY);

  Arena_Temp scratch = arena_scratch_get(0, 0);

  Resolved_Expr operand = resolve_expr(expr->unary.right);
  Type *type = operand.type;
  switch (expr->unary.op.kind)
  {
  case TOKEN_DEREF:
  {
    operand = ptr_decay(operand);
    if (type->kind != TYPE_PTR)
    {
      fatal("Cannot dereference non-pointer type");
    }
    return resolved_lvalue(type->ptr.base);
  }
  case '&':
  {
    if (!operand.is_lvalue)
    {
      fatal("Cannot take address of non-lvalue");
    }
    return resolved_rvalue(type_ptr(type));
  }
  case '+':
  case '-':
  case '!':
  case '~':
  {
    if (type->kind != TYPE_INT)
    {
      fatal("Can use unary %.*s with ints only", str8_varg(str_from_token_kind(scratch.arena, expr->unary.op.kind)));
    }
    if (operand.is_const)
    {
      return resolved_const(eval_int_unary(expr->unary.op.kind, operand.const_value));
    }
    return resolved_rvalue(type);
  }
  default:
    assert(0);
    return nil_resolved_expr;
  }

  arena_scratch_release(scratch);

  assert(!"Unreachable");
  return nil_resolved_expr;
}

internal s64
eval_int_binary(Token_Kind op, s64 left, s64 right)
{
  switch (op)
  {
  case '*': return left * right;
  case '/':
  {
    if (right == 0)
    {
      // fatal("Divide by zero in constant expression");
      return 0;
    }
    return left / right;
  }
  case '%':
  {
    if (right == 0)
    {
      // fatal("Divide by zero in constant expression");
      return 0;
    }
    return left % right;
  }
  // TODO: Handle UB in shifts etc.
  case TOKEN_LSHIFT: return left << right; // TODO: handle signed vs unsigned
  case TOKEN_RSHIFT: return left >> right;
  case '+': return left + right;
  case '-': return left - right;
  // TODO: %%
  case '^':          return left ^ right;
  case '&':          return left & right;
  case '|':          return left | right;
  // TODO: comparison operators
  case TOKEN_EQ:          return left == right;
  case TOKEN_NEQ:         return left != right;
  case TOKEN_GTEQ:        return left >= right;
  case TOKEN_LTEQ:        return left <= right;
  case '>':               return left > right;
  case '<':               return left < right;
  case TOKEN_LOGICAL_OR:  return (left != 0) || (right != 0); // TODO: handle separately = short circuiting
  case TOKEN_LOGICAL_AND: return (left != 0) && (right != 0);
  default:
    assert(0);
    break;
  }
  return 0;
}

internal Resolved_Expr
resolve_expr_binary(Expr *expr)
{
  assert(expr->kind == EXPR_BINARY);

  Resolved_Expr left  = resolve_expr(expr->binary.left);
  Resolved_Expr right = resolve_expr(expr->binary.right);

  Arena_Temp scratch = arena_scratch_get(0, 0);
  if (left.type != type_int)
  {
    fatal("left operand of %.*s must be int", str8_varg(str_from_token_kind(scratch.arena, expr->binary.op.kind)));
  }
  if (left.type != right.type)
  {
    fatal("left and right operand of %.*s must have same type", str8_varg(str_from_token_kind(scratch.arena, expr->binary.op.kind)));
  }
  arena_scratch_release(scratch);

  if (left.is_const && right.is_const)
  {
    // return resolved_const(left.const_value + right.const_value);
    return resolved_const(eval_int_binary(expr->binary.op.kind, left.const_value, right.const_value));
  }
  return resolved_rvalue(left.type);
}

internal usize
aggregate_field_index(Type *type, String8 name)
{
  assert(type->kind == TYPE_STRUCT || type->kind == TYPE_UNION);
  for each_index(i, type->aggregate.fields.count)
  {
    // TODO: string interning
    if (str8_equal(type->aggregate.fields.v[i].name, name))
    {
      return i;
    }
  }
  fatal("Field '%.*s' in compound literal not found in struct/union", str8_varg(name));
  return SIZE_MAX;
}

internal Resolved_Expr
resolve_expr_compound(Expr *expr, Type *expected_type)
{
  assert(expr->kind == EXPR_COMPOUND);

  if (!expected_type && !expr->compound.type)
  {
    fatal("Implicitly typed compound literal used in context without expected type");
  }

  Type *type = NULL;
  if (expr->compound.type)
  {
    type = resolve_typespec(expr->compound.type);
    // if (expected_type && expected_type != type)
    // {
    //   fatal("Explicit compound literal type does not match expected type");
    // }
  }
  else
  {
    type = expected_type;
  }
  complete_type(type);

  assert(expr->compound.args.count > 0);

  if (type->kind != TYPE_STRUCT && type->kind != TYPE_UNION && type->kind != TYPE_ARRAY)
  {
    // TODO: slices
    fatal("Compound literals can only be used with struct, union and array types");
  }

  if (type->kind == TYPE_STRUCT || type->kind == TYPE_UNION)
  {
    assert(type->aggregate.fields.count > 0);
    // if (expr->compound.args.count > type->aggregate.fields.count)
    // {
    //   fatal("Compound literal has too many fields");
    // }

    u32 index = 0;
    for each_index(i, expr->compound.args.count)
    {
      Compound_Field *field = expr->compound.args.v[i];
      if (field->kind == COMPOUND_FIELD_INDEX)
      {
        fatal("Index field initializer not allowed for struct/union compound literal");
      }
      else if (field->kind == COMPOUND_FIELD_NAME)
      {
        index = aggregate_field_index(type, field->name);
      }
      if (index >= type->aggregate.fields.count)
      {
        fatal("Field initializer in struct/union compound literal out of range");
      }
      Resolved_Expr init = resolve_expected_expr(field->init, type->aggregate.fields.v[index].type);
      if (init.type != type->aggregate.fields.v[index].type)
      {
        fatal("Compound literal field type mismatch");
      }
      // if (arg->optional_name.count > 0 && !str8_equal(arg->optional_name, type->aggregate.fields.v[i].name))
      // {
      //   // TODO: Make it so u can specify the named fields out of order
      //   fatal("Compound literal field name mismatch");
      // }

      index += 1;
    }
  }
  else
  {
    // assert(0);
    assert(type->kind == TYPE_ARRAY);
    // if (expr->compound.args.count > type->array.length)
    // {
    //   fatal("Compound literal has too many elements");
    // }
    u32 index = 0;
    for each_index(i, expr->compound.args.count)
    {
      Compound_Field *field = expr->compound.args.v[i];
      if (field->kind == COMPOUND_FIELD_NAME)
      {
        fatal("Named field initializer not allowed in array compound literal");
      }
      else if (field->kind == COMPOUND_FIELD_INDEX)
      {
        s64 result = resolve_const_expr(field->index);
        if (result < 0)
        {
          fatal("Field initializer index cannoty be negative");
        }
        index = result;
      }
      if (index >= type->array.length)
      {
        fatal("Field initializer in array compound literal out of range");
      }
      Resolved_Expr init = resolve_expected_expr(field->init, type->array.base);
      if (init.type != type->array.base)
      {
        fatal("Compound literal element type mismatch");
      }
      index += 1;
    }
  }

  return resolved_rvalue(type);
}

internal Resolved_Expr
resolve_expr_call(Expr *expr)
{
  assert(expr->kind == EXPR_CALL);

  Resolved_Expr proc = resolve_expr(expr->call.expr);
  complete_type(proc.type);

  if (proc.type->kind != TYPE_PROC)
  {
    fatal("Trying to call non-procedure value");
  }
  if (expr->call.args.count < proc.type->proc.params.count)
  {
    fatal("Too few arguments for procedure call");
  }
  else if (expr->call.args.count > proc.type->proc.params.count)
  {
    fatal("Too many arguments for procedure call");
  }

  for (u32 i = 0; i < expr->call.args.count; i += 1)
  {
    Type *param_type = proc.type->proc.params.v[i];
    Resolved_Expr arg = resolve_expected_expr(expr->call.args.v[i], param_type);
    if (arg.type != param_type)
    {
      fatal("Procedure call argument type mismatch");
    }
  }

  return resolved_rvalue(proc.type->proc.ret);
}

internal Resolved_Expr
resolve_expr_ternary(Expr *expr, Type *expected_type)
{
  // TODO: have actual bool types
  assert(expr->kind == EXPR_TERNARY);
  Resolved_Expr cond = ptr_decay(resolve_expr(expr->ternary.cond));
  if (cond.type->kind != TYPE_INT && cond.type->kind != TYPE_PTR)
  {
    fatal("Ternary condition expression must have type int or ptr");
  }
  Resolved_Expr then_expr = ptr_decay(resolve_expr(expr->ternary.then));
  Resolved_Expr else_expr = ptr_decay(resolve_expr(expr->ternary.else_));
  if (then_expr.type != else_expr.type)
  {
    fatal("Ternary then/else expression must have matching types");
  }
  if (cond.is_const && then_expr.is_const && else_expr.const_value)
  {
    return resolved_const(cond.const_value ? then_expr.const_value : else_expr.const_value);
  }
  return resolved_rvalue(then_expr.type);
}

internal Resolved_Expr
resolve_expr_index(Expr *expr)
{
  assert(expr->kind == EXPR_INDEX);

  Resolved_Expr operand = ptr_decay(resolve_expr(expr->index.expr));
  if (operand.type->kind != TYPE_PTR && operand.type->kind != TYPE_ARRAY)
  {
    // IMPORTANT TODO: make it so u can only index arrays and add multipointer like in odin [^] == [*]
    fatal("Can only index arrays or pointers");
  }
  Resolved_Expr index   = resolve_expr(expr->index.index);
  if (index.type->kind != TYPE_INT)
  {
    fatal("Index expression must have type int");
  }
  if (operand.type->kind == TYPE_PTR)
  {
    return resolved_lvalue(operand.type->ptr.base);
  }
  assert(operand.type->kind == TYPE_ARRAY);
  return resolved_lvalue(operand.type->array.base);
}

internal Resolved_Expr
resolve_expr_cast(Expr *expr)
{
  assert(expr->kind == EXPR_CAST);

  Type *type = resolve_typespec(expr->cast.type);
  Resolved_Expr result = ptr_decay(resolve_expr(expr->cast.expr));

  // ptr -> ptr, ptr -> int, int -> ptr
  if (type->kind == TYPE_PTR)
  {
    if (result.type->kind != TYPE_PTR && result.type->kind != TYPE_INT)
    {
      fatal("Invalid cast to pointer type");
    }
  }
  else if (type->kind == TYPE_INT)
  {
    if (result.type->kind != TYPE_PTR && result.type->kind != TYPE_INT)
    {
      fatal("Invalid cast to int type");
    }
  }
  else
  {
    fatal("Invalid target cast type");
  }

  return resolved_rvalue(type);
}

internal Resolved_Expr
resolve_expected_expr(Expr *expr, Type *expected_type)
{
  // TODO: address constants
  switch (expr->kind)
  {
  case EXPR_INTEGER_LITERAL:
    return resolved_const(expr->literal.integer);
  case EXPR_FLOAT_LITERAL:
    return resolved_rvalue(type_float);
  case EXPR_STRING_LITERAL:
    // IMPORTANT TODO: get rid of char * and use sized string!!
    return resolved_rvalue(type_ptr(type_char));
  case EXPR_CHAR_LITERAL:
    // return resolved_rvalue(type_char);
    // TODO(mxtej): for now this will just be converted to int const
    return resolved_const(expr->literal.character);
  case EXPR_IDENT:
    return resolve_expr_name(expr);
  case EXPR_COMPOUND:
    return resolve_expr_compound(expr, expected_type);
  case EXPR_FIELD:
    return resolve_expr_field(expr);
  case EXPR_INDEX:
    return resolve_expr_index(expr);
  case EXPR_UNARY:
    return resolve_expr_unary(expr);
  case EXPR_BINARY:
    return resolve_expr_binary(expr);
  case EXPR_TERNARY:
    return resolve_expr_ternary(expr, expected_type);
  case EXPR_CALL:
    return resolve_expr_call(expr);
  case EXPR_CAST:
    return resolve_expr_cast(expr);
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
  case EXPR_GROUP:
  {
    return resolve_expr(expr->group.expr);
  }
  default:
    assert(0);
    return nil_resolved_expr;
  }

  assert(!"Unreachable");
  return nil_resolved_expr;
}

internal Resolved_Expr
resolve_expr(Expr *expr)
{
  return resolve_expected_expr(expr, NULL);
}

internal s64
resolve_const_expr(Expr *expr)
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
  printf("\n");
  printf("--- RESOLVE TEST\n");
  printf("\n");

  entity_install_type(str8_lit("void"), type_void);
  entity_install_type(str8_lit("char"), type_char);
  entity_install_type(str8_lit("int"),  type_int);

  String8 source = S(
    "union Int_Or_Ptr { i: int, p: *int, }\n"
    "var u1 = Int_Or_Ptr.{i = 42};\n"
    "var u2 = Int_Or_Ptr.{p = cast(*int)42};\n"
    "var a: [256]int = .{1, 2, ['a'] = 42, [255] = 123};\n"

    // "var a: [3]int = {1,2,3};\n"
    // "var b: [4]int;\n"
    // "var p = &a[1];\n"
    // "var i = p[1];\n"
    // "var j = p.*;\n"
    // "const N = size_of(1 ? a : b);\n"
    // "const M = size_of(&a[0]);\n"

    // "struct Vector { x, y: int, }\n"
    // "var v: Vector = 0 ? {1,2} : {3,4};\n"
    // "var vs: [2][2]Vector = {{{1,2},{3,4}}, {{5,6},{7,8}}};\n"

    // "var pi = 3.14;\n"
    // "var name = \"stuff\";\n"

    // "var v: Vector = {1,2};\n"
    // "var a = 42;\n"
    // "var p = cast(*void)a;\n"
    // "var j = cast(int)p;\n"
    // "var q = cast(*int)j;\n"

    // "const i = 42;\n"
    // "const j = +i;\n"
    // "const k = -i;\n"
    // "const l = !!i;\n"
    // "const m = ~7 + 1 == -7;\n"
    // "const a = 1000/((2*3-5) << 1);\n"

    // "const K = 1 ? 2 : 3;\n"
    // "proc add(a: Vector, b: Vector) -> Vector { return { a.x + b.x, a.y + b.y }; }\n"
    // "var x = add(Vector{1,2}, Vector{3,4});\n"
    // "var a: [3]int = {1,2,3};\n"
    // // "var i = a[1];\n"
    // "var p = &a[1];\n"
    // "var i = p[1];\n"
    // "var w = Vector{3,4};\n"
    // "var i = 42;\n"
    // "var u = Int_Or_Ptr{ i, &i };\n"

    // "const N = 1 + size_of(p);\n" // 9
    // "var p: *T;\n" // 4
    // "var u = p.*;\n" // deref
    // "struct T { a: [N]int, }\n"
    // "var r = &t.a;\n"
    // "var t: T;\n"
    // "typedef S = [N+M]int;\n"
    // "const M = size_of(t.a);\n" // 36
    // "var i = N+M;\n" // [9 + 36] = 45
    // "var q = &i;\n"
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
      str8_list_pushf(scratch.arena, &list, "  [=%d]", en->const_value);
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
