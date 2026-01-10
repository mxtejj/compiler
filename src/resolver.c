#include "string.h"
#include "arena.h"
#include "parser.h"
#include "print.h"
#include "os.h"

//
// TODO:
// [ ] Make sure that all control paths return
// [ ] Unify both size_of exprs
// [ ] Fix compound literal parsing
//

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

STRUCT(Sym);

STRUCT(Type)
{
  Type_Kind kind;
  usize size;
  usize align;
  Sym *sym;

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

Type *type_void  = &(Type){ .kind = TYPE_VOID,  .size = 0, .align = 0 };
Type *type_char  = &(Type){ .kind = TYPE_CHAR,  .size = 1, .align = 1 };
Type *type_int   = &(Type){ .kind = TYPE_INT,   .size = 4, .align = 4 };
Type *type_float = &(Type){ .kind = TYPE_FLOAT, .size = 4, .align = 4 };

const usize PTR_SIZE  = 8;
const usize PTR_ALIGN = 8;

internal usize
type_size_of(Type *type)
{
  assert(type->kind > TYPE_COMPLETING);
  assert(type->size != 0);
  return type->size;
}

internal usize
type_align_of(Type *type)
{
  assert(type->kind > TYPE_COMPLETING);
  assert(is_pow2(type->align));
  return type->align;
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

// TODO(#13): hash table
Cached_Ptr_Type_List cached_ptr_types;

internal Type *
type_ptr(Type *base)
{
  for each_node(it, Cached_Ptr_Type, cached_ptr_types.first)
  {
    if (it->v.ptr.base == base)
    {
      return &it->v;
    }
  }

  // Type *t = type_alloc(TYPE_PTR);
  // t->ptr.base = base;
  Type t = (Type){ .kind = TYPE_PTR, .ptr.base = base };
  t.size  = PTR_SIZE;
  t.align = PTR_ALIGN;

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
  for each_node(it, Cached_Array_Type, cached_array_types.first)
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
  t.align = type_align_of(base);

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
  for each_node(it, Cached_Proc_Type, cached_proc_types.first)
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
  t.size  = PTR_SIZE;
  t.align = PTR_ALIGN;
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
  type->kind  = TYPE_STRUCT;
  type->size  = 0;
  type->align = 0;
  // for (Type_Field *it = fields.v; it != fields.v + fields.count; it += 1)
  for each_index(i, fields.count)
  {
    // TODO: store offsets of fields in the type
    Type_Field it = fields.v[i];
    type->size = type_size_of(it.type) + align_up_pow2(type->size, type_align_of(it.type));
    type->align = clamp_bot(type->align, type_align_of(it.type));
  }

  type->aggregate.fields.v = push_array_nz(resolve_arena, Type_Field, fields.count);
  type->aggregate.fields.count = fields.count;

  mem_copy(type->aggregate.fields.v, fields.v, fields.count * sizeof(Type_Field));
}

internal void
type_complete_union(Type *type, Type_Field_Array fields)
{
  assert(type->kind == TYPE_COMPLETING);
  type->kind  = TYPE_UNION;
  type->size  = 0;
  type->align = 0;
  for (Type_Field *it = fields.v; it != fields.v + fields.count; it += 1)
  {
    assert(it->type->kind > TYPE_COMPLETING);
    type->size  = MAX(type->size, type_size_of(it->type));
    type->align = MAX(type->align, type_align_of(it->type));
  }

  type->aggregate.fields.v = push_array_nz(resolve_arena, Type_Field, fields.count);
  type->aggregate.fields.count = fields.count;

  mem_copy(type->aggregate.fields.v, fields.v, fields.count * sizeof(Type_Field));
}

internal Type *
type_incomplete(Sym *sym)
{
  Type *type = type_alloc(TYPE_INCOMPLETE);
  type->sym = sym;
  return type;
}

ENUM(Sym_Kind)// 176
{
  SYM_NONE,
  SYM_VAR,
  SYM_CONST,
  SYM_PROC,
  SYM_TYPE,
  SYM_ENUM_CONST,
};

ENUM(Sym_State)
{
  SYM_UNRESOLVED,
  SYM_RESOLVING,
  SYM_RESOLVED,
};

STRUCT(Sym)
{
  String8       name;
  Sym_Kind   kind;
  Sym_State  state;
  Decl         *decl;
  Type         *type;
  s64           const_value;
};

STRUCT(Sym_Node)
{
  Sym_Node *next;
  Sym *v;
};

STRUCT(Sym_List)
{
  Sym_Node *first;
  Sym_Node *last;
  u64 count;
};

#define MAX_LOCAL_SYMS 1024

Sym_List global_syms;
Sym *local_syms[MAX_LOCAL_SYMS];
Sym **local_syms_end = local_syms;

internal void
sym_list_push(Sym_List *list, Sym *sym)
{
  Sym_Node *node = push_struct(resolve_arena, Sym_Node);
  node->v = sym;
  sll_queue_push(list->first, list->last, node);
  list->count += 1;
}

internal Sym *
sym_alloc(Sym_Kind kind, String8 name, Decl *decl)
{
  if (resolve_arena == NULL)
  {
    resolve_arena = arena_alloc(GB(1), MB(1), 0);
  }

  // assert(decl);

  Sym *sym = push_struct(resolve_arena, Sym);
  sym->kind = kind;
  sym->name = name;
  sym->decl = decl;

  return sym;
}

internal Sym *
sym_var(String8 name, Type *type)
{
  Sym *sym = sym_alloc(SYM_VAR, name, NULL);
  sym->state = SYM_RESOLVED;
  sym->type  = type;
  return sym;
}

internal Sym *
sym_decl(Decl *decl)
{
  Sym_Kind kind = SYM_NONE;
  switch (decl->kind)
  {
  // case DECL_STRUCT:
  // case DECL_UNION:
  // case DECL_TYPEDEF:
  // case DECL_ENUM: // TODO
  //   kind = SYM_TYPE;
  //   break;
  case DECL_VAR:
    kind = SYM_VAR;
    break;
  case DECL_CONST:
    if (decl->init_type)
    {
      if (decl->init_type->kind == TYPE_SPEC_PROC)
      {
        kind = SYM_PROC;
      }
      else
      {
        kind = SYM_TYPE;
      }
    }
    else if (decl->init_expr)
    {
      kind = SYM_CONST;
    }
    break;
  // case DECL_PROC:
  //   kind = SYM_PROC;
  //   break;
    // kind = SYM_TYPE;
  // case DECL_ENUM:
    // kind = SYM_ENUM_CONST;
    // break;
  default:
    // assert(0);
    break;
  }

  Sym *sym = sym_alloc(kind, decl->name, decl);
  if (decl->init_type && (decl->init_type->kind == TYPE_SPEC_STRUCT || decl->init_type->kind == TYPE_SPEC_UNION))
  {
    sym->state = SYM_RESOLVED;
    sym->type  = type_incomplete(sym);
  }
  return sym;
}

internal Sym *
sym_enum_const(String8 name, Decl *decl)
{
  return sym_alloc(SYM_ENUM_CONST, name, decl);
}

internal Sym *
sym_get(String8 name)
{
  for (Sym **it = local_syms_end; it != local_syms; it -= 1)
  {
    Sym *sym = it[-1];
    // TODO: string interning
    if (str8_equal(sym->name, name))
    {
      return sym;
    }
  }

  for each_node(it, Sym_Node, global_syms.first)
  {
    if (str8_equal(it->v->name, name))
    {
      return it->v;
    }
  }
  return NULL;
}

internal void
sym_push(Sym *sym)
{
  if (local_syms_end == local_syms + MAX_LOCAL_SYMS)
  {
    fatal("Too many local symbols");
  }
  *local_syms_end++ = sym;
}

internal Sym **
sym_enter()
{
  return local_syms_end;
}

internal void
sym_leave(Sym **ptr)
{
  local_syms_end = ptr;
}

internal Sym *
sym_global_decl(Decl *decl)
{
  Sym *sym = sym_decl(decl);
  sym_list_push(&global_syms, sym);
  // if (decl->kind == DECL_ENUM)
  decl->sym = sym;
  if (decl->init_type && decl->init_type->kind == TYPE_SPEC_ENUM)
  {
    for each_index(i, decl->init_type->enum_members.count)
    {
      Enum_Member it = decl->init_type->enum_members.v[i];
      sym_list_push(&global_syms, sym_enum_const(it.name, decl));
    }
  }
  return sym;
}

internal Sym *
sym_global_type(String8 name, Type *type)
{
  Sym *sym = sym_alloc(SYM_TYPE, name, NULL);
  sym->state = SYM_RESOLVED;
  sym->type  = type;
  sym_list_push(&global_syms, sym);
  return sym;
}

STRUCT(Operand)
{
  Type *type;
  b32 is_lvalue;
  b32 is_const;
  s64 const_value;
};

read_only global Operand nil_operand;

internal Operand
resolved_rvalue(Type *type)
{
  return (Operand){ .type = type };
}

internal Operand
resolved_lvalue(Type *type)
{
  return (Operand){ .type = type, .is_lvalue = true };
}

internal Operand
resolved_const(s64 const_value)
{
  return (Operand){ .type = type_int, .is_const = true, .const_value = const_value };
}

// :forward declarations
internal Sym *resolve_name(String8 name);
internal s64 resolve_const_expr(Expr *expr);
internal Operand resolve_expr(Expr *expr);
internal Operand resolve_expected_expr(Expr *expr, Type *expected_type);
internal Type *resolve_decl_var(Decl *decl);
internal void resolve_sym(Sym *sym);

internal Type *
resolve_typespec(Type_Spec *typespec)
{
  if (!typespec)
  {
    // @HACK
    return type_void;
  }

  switch (typespec->kind)
  {
  case TYPE_SPEC_NULL:
    break;
  case TYPE_SPEC_NAME:
  {
    Sym *sym = resolve_name(typespec->name);
    if (sym->kind != SYM_TYPE)
    {
      fatal("%.*s must denote a type", str8_varg(typespec->name));
      return NULL;
    }
    return sym->type;
  }
  case TYPE_SPEC_PROC:
  {
    Type_Param_Array params = {0};
    if (typespec->proc.params.count > 0)
    {
      params.count = typespec->proc.params.count;
      params.v = push_array_nz(resolve_arena, Type_Param, params.count);

      for each_index(i, params.count)
      {
        Decl *it = typespec->proc.params.v[i];
        params.v[i] = resolve_decl_var(it);
      }
    }
    Type *ret = type_void;
    if (typespec->proc.ret)
    {
      ret = resolve_typespec(typespec->proc.ret);
    }
    return type_proc(params, ret);
  }
  case TYPE_SPEC_ENUM:
  {
    // For inline enum types, we need to create a type
    // This is handled similar to struct/union
    Type *type = type_alloc(TYPE_ENUM);
    type->size = 4;  // enums are int-sized
    type->align = 4;
    // TODO: process enum members if needed
    return type;
  }
  case TYPE_SPEC_STRUCT:
  case TYPE_SPEC_UNION:
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

Sym_List ordered_global_syms;

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
  Decl *decl = type->sym->decl;

  assert(decl->init_type && (decl->init_type->kind == TYPE_SPEC_STRUCT || decl->init_type->kind == TYPE_SPEC_UNION));

  u32 total_field_count = 0;
  for each_index(i, decl->init_type->aggr_fields.count)
  {
    Aggr_Field it = decl->init_type->aggr_fields.v[i];
    total_field_count += it.names.node_count;
  }

  if (total_field_count == 0)
  {
    fatal("No fields");
  }

  Type_Field_Array fields = {0};
  fields.count = total_field_count;
  fields.v = push_array(resolve_arena, Type_Field, fields.count);

  assert(decl->init_type->aggr_fields.count > 0);

  for each_index(i, decl->init_type->aggr_fields.count)
  {
    Aggr_Field it = decl->init_type->aggr_fields.v[i];
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

  if (decl->init_type->kind == TYPE_SPEC_STRUCT)
  {
    type_complete_struct(type, fields);
  }
  else
  {
    assert(decl->init_type->kind == TYPE_SPEC_UNION);
    type_complete_union(type, fields);
  }

  sym_list_push(&ordered_global_syms, type->sym);
}

internal Type *
resolve_decl_type(Decl *decl)
{
  assert(decl->init_type);
  return resolve_typespec(decl->init_type);
}

internal Type *
resolve_decl_var(Decl *decl)
{
  assert(decl->kind == DECL_VAR);
  Type *type = NULL;
  if (decl->type_hint)
  {
    type = resolve_typespec(decl->type_hint);
  }
  if (decl->init_expr)
  {
    Operand result = resolve_expected_expr(decl->init_expr, type);
    if (type && result.type != type)
    {
      fatal("Declared var type does not match inferred type");
    }
    type = result.type;
  }
  else if (decl->init_type)
  {
    // if we have a resolved type hint it must match init type?
    Type *type_hint = type;
    type = resolve_typespec(decl->init_type);
    if (type_hint && type != type_hint)
    {
      fatal("Declared var type does not match type hint");
    }
  }
  complete_type(type);
  return type;
}

internal Type *
resolve_decl_const(Decl *decl, s64 *const_value)
{
  assert(decl->kind == DECL_CONST);
  assert(decl->init_expr);
  Operand result = resolve_expr(decl->init_expr);
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
  assert(decl->kind == DECL_CONST);
  assert(decl->init_type && decl->init_type->kind == TYPE_SPEC_PROC);

  Type_Spec *proc_spec = decl->init_type;
  Decl_Array params = proc_spec->proc.params;

  Type_Param_Array param_types = {0};
  param_types.count = params.count;
  param_types.v = push_array(resolve_arena, Type_Param, param_types.count);

  for each_index(i, params.count)
  {
    // param_types.v[i] = resolve_typespec(params.v[i]->type_hint);
    param_types.v[i] = resolve_decl_var(params.v[i]);
  }

  return type_proc(param_types, resolve_typespec(proc_spec->proc.ret));
}

internal void resolve_stmt_block(Stmt_Array block, Type *ret_type);

internal void
resolve_cond_expr(Expr *expr)
{
  Operand cond = resolve_expr(expr);
  if (cond.type != type_int)
  {
    // TODO(#14): make it so cond has to be `bool` not `int`
    fatal("condition expression must be of type int");
  }
}

internal void
resolve_stmt(Stmt *stmt, Type *ret_type)
{
  switch (stmt->kind)
  {
  case STMT_BLOCK: resolve_stmt_block(stmt->block, ret_type); break;
  case STMT_IF:
  {
    resolve_cond_expr(stmt->if0.cond);
    resolve_stmt_block(stmt->if0.then_block->block, ret_type);
    if (stmt->if0.else_stmt != NULL)
    {
      if (stmt->if0.else_stmt->kind == STMT_BLOCK)
      {
        resolve_stmt_block(stmt->if0.else_stmt->block, ret_type);
      }
      else
      {
        assert(stmt->if0.else_stmt->kind == STMT_IF);
        resolve_stmt(stmt->if0.else_stmt, ret_type);
      }
    }
    break;
  }
  case STMT_DO_WHILE:
  case STMT_WHILE:
  {
    resolve_cond_expr(stmt->while0.cond);
    resolve_stmt_block(stmt->while0.body->block, ret_type);
    break;
  }
  case STMT_FOR:
  {
    Sym **syms = sym_enter();
    resolve_stmt(stmt->for0.init, ret_type);
    resolve_cond_expr(stmt->for0.cond);
    resolve_stmt_block(stmt->for0.body->block, ret_type);
    resolve_stmt(stmt->for0.loop, ret_type);
    sym_leave(syms);
    break;
  }
  case STMT_FOR_IN:
  {
    // TODO: is this right?
    Sym **syms = sym_enter();
    resolve_expr(stmt->for_in.item);
    resolve_expr(stmt->for_in.iter);
    resolve_stmt_block(stmt->for_in.body->block, ret_type);
    sym_leave(syms);
    break;
  }
  case STMT_SWITCH:
  {
    Operand result = resolve_expr(stmt->switch0.expr);
    for each_index(i, stmt->switch0.cases.count)
    {
      Switch_Case it = stmt->switch0.cases.v[i];
      for each_index(j, it.labels.count)
      {
        Expr *expr = it.labels.v[j];
        Operand case_result = resolve_expr(expr);
        if (case_result.type != result.type)
        {
          fatal("Switch case expression type mismatch");
        }
        resolve_stmt_block(it.block->block, ret_type);
      }
    }
    break;
  }
  case STMT_RETURN:
  {
    // TODO(#15): support multiple returns
    if (stmt->return_expr)
    {
      Operand result = resolve_expected_expr(stmt->return_expr, ret_type);
      if (result.type != ret_type)
      {
        fatal("Return type mismatch");
      }
    }
    else
    {
      // Empty return statement
      if (ret_type && ret_type != type_void)
      {
        fatal("Cannot return without a value from procedure with non-void return type");
      }
    }
    break;
  }
  case STMT_BREAK:
  case STMT_CONTINUE:
    // Do nothing
    break;
  case STMT_EXPR:
  {
    resolve_expr(stmt->expr);
    break;
  }
  case STMT_DECL:
  {
    Decl *decl = stmt->decl;
    Sym *sym = sym_decl(decl);
    resolve_sym(sym);
    sym_push(sym);
    break;
  }
  default:
    assert(0);
    break;
  }
}

internal void
resolve_stmt_block(Stmt_Array block, Type *ret_type)
{
  Sym **syms = sym_enter();
  for each_index(i , block.count)
  {
    Stmt *it = block.v[i];
    resolve_stmt(it, ret_type);
  }
  sym_leave(syms);
}

internal void
resolve_proc(Sym *sym)
{
  Decl *decl = sym->decl;
  assert(decl->kind == DECL_CONST);
  assert(decl->init_type && decl->init_type->kind == TYPE_SPEC_PROC);
  assert(sym->state == SYM_RESOLVED);

  Type_Spec *proc_spec = decl->init_type;
  Decl_Array params = proc_spec->proc.params;

  Sym **syms = sym_enter();
  for each_index(i, params.count)
  {
    Decl *param = params.v[i];
    sym_push(sym_var(param->name, resolve_typespec(param->type_hint)));
    // resolve_decl_var(param);
  }
  if (proc_spec->proc.body)
  {
    resolve_stmt_block(proc_spec->proc.body->block, resolve_typespec(proc_spec->proc.ret));
  }
  sym_leave(syms);
}

/*

internal void
resolve_proc(Sym *sym)
{
  Decl *decl = sym->decl;
  assert(decl->kind == DECL_PROC);
  assert(sym->state == SYM_RESOLVED);
  Type *type = sym->type;
  assert(type->kind == TYPE_PROC);

  Sym **syms = sym_enter();
  for each_index(i, type->proc.params.count)
  {
    // Type_Param param = type->proc.params.v[i];
    // sym_push(sym_var());
  }
  sym_leave(syms);
}

*/

internal void
resolve_sym(Sym *sym)
{
  if (sym->state == SYM_RESOLVED)
  {
    return;
  }
  else if (sym->state == SYM_RESOLVING)
  {
    fatal("Cyclic dependency");
    return;
  }

  assert(sym->state == SYM_UNRESOLVED);
  sym->state = SYM_RESOLVING;

  switch (sym->kind)
  {
  case SYM_TYPE:
    sym->type = resolve_decl_type(sym->decl);
    break;
  case SYM_VAR:
    sym->type = resolve_decl_var(sym->decl);
    break;
  case SYM_CONST:
  {
    // TODO(#16): `distinct` keyword
    // My_Int :: distinct int
    // #assert(My_Int != int)

    // Check if this is actually a type alias (e.g., My_Int :: int)
    Decl *decl = sym->decl;
    if (decl->init_expr && decl->init_expr->kind == EXPR_IDENT)
    {
      Sym *ref_sym = sym_get(decl->init_expr->ident);
      if (ref_sym && ref_sym->kind == SYM_TYPE)
      {
        // This is a type alias, not a constant
        // @CLEANUP
        sym->kind = SYM_TYPE;
        resolve_sym(ref_sym);
        sym->type = ref_sym->type;
        break;
      }
    }
    sym->type = resolve_decl_const(decl, &sym->const_value);
    break;
  }
  // case SYM_TYPEDEF:
    // sym->type = resolve_decl_type(sym->decl);
    // break;
  case SYM_PROC:
    sym->type = resolve_decl_proc(sym->decl);
    break;
  default:
    assert(0);
    break;
  }

  sym->state = SYM_RESOLVED;
  sym_list_push(&ordered_global_syms, sym);
}

internal void
complete_sym(Sym *sym)
{
  resolve_sym(sym);
  if (sym->kind == SYM_TYPE)
  {
    complete_type(sym->type);
  }
  else if (sym->kind == SYM_PROC)
  {
    resolve_proc(sym);
  }
}

internal Sym *
resolve_name(String8 name)
{
  Sym *sym = sym_get(name);
  if (!sym)
  {
    fatal("Non-existent name '%.*s'", str8_varg(name));
    return NULL;
  }
  resolve_sym(sym);
  return sym;
}

internal Operand
resolve_expr_field(Expr *expr)
{
  assert(expr->kind == EXPR_FIELD);
  Operand left = resolve_expr(expr->field.expr);
  Type *type = left.type;
  complete_type(type);
  if (type->kind != TYPE_STRUCT && type->kind != TYPE_UNION)
  {
    fatal("Can only access fields on aggregate types");
    return nil_operand;
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
  return nil_operand;
}

internal Operand
ptr_decay(Operand expr)
{
  // TODO(#17): dont do this
  if (expr.type->kind == TYPE_ARRAY)
  {
    return resolved_rvalue(type_ptr(expr.type->array.base));
  }
  return expr;
}

internal Operand
resolve_expr_name(Expr *expr)
{
  assert(expr->kind == EXPR_IDENT);
  Sym *sym = resolve_name(expr->ident);

  switch (sym->kind)
  {
  case SYM_VAR:
    return resolved_lvalue(sym->type);
  case SYM_CONST:
    return resolved_const(sym->const_value);
  case SYM_PROC:
  case SYM_TYPE:
    return resolved_rvalue(sym->type);
  default:
    fatal("%.*s must be a var or const", str8_varg(expr->ident));
    return nil_operand;
  }

  assert(!"Unreachable");
  return nil_operand;
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

internal Operand
resolve_expr_unary(Expr *expr)
{
  assert(expr->kind == EXPR_UNARY);

  Arena_Temp scratch = arena_scratch_get(0, 0);

  Operand operand = resolve_expr(expr->unary.right);
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
    return nil_operand;
  }

  arena_scratch_release(scratch);

  assert(!"Unreachable");
  return nil_operand;
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
  // TODO(#18): Handle UB in shifts etc.
  case TOKEN_LSHIFT: return left << right; // TODO(#19): handle signed vs unsigned
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
  case TOKEN_LOGICAL_OR:  return (left != 0) || (right != 0); // TODO(#20): handle separately = short circuiting
  case TOKEN_LOGICAL_AND: return (left != 0) && (right != 0);
  default:
    assert(0);
    break;
  }
  return 0;
}

internal Operand
resolve_expr_binary(Expr *expr)
{
  assert(expr->kind == EXPR_BINARY);

  Operand left  = resolve_expr(expr->binary.left);
  Operand right = resolve_expected_expr(expr->binary.right, left.type);

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

  // Handle assignment operators
  if (is_assign_op(expr->binary.op.kind))
  {
    if (!left.is_lvalue)
    {
      fatal("Left side of assignment must be an lvalue");
    }
    // Types already checked above, assignment is valid
    // Return lvalue so assignments can be chained: a = b = c
    return resolved_lvalue(left.type);
  }

  // Regular binary operation
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

internal Operand
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
    // TODO(#21): slices
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
      Operand init = resolve_expected_expr(field->init, type->aggregate.fields.v[index].type);
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
          fatal("Field initializer index cannot be negative");
        }
        index = result;
      }
      if (index >= type->array.length)
      {
        fatal("Field initializer in array compound literal out of range");
      }
      Operand init = resolve_expected_expr(field->init, type->array.base);
      if (init.type != type->array.base)
      {
        fatal("Compound literal element type mismatch");
      }
      index += 1;
    }
  }

  return resolved_rvalue(type);
}

internal Operand
resolve_expr_call(Expr *expr)
{
  assert(expr->kind == EXPR_CALL);

  Operand proc = resolve_expr(expr->call.expr);
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
    Operand arg = resolve_expected_expr(expr->call.args.v[i], param_type);
    if (arg.type != param_type)
    {
      fatal("Procedure call argument type mismatch");
    }
  }

  return resolved_rvalue(proc.type->proc.ret);
}

internal Operand
resolve_expr_ternary(Expr *expr, Type *expected_type)
{
  // TODO(#22): have actual bool types
  assert(expr->kind == EXPR_TERNARY);
  Operand cond = ptr_decay(resolve_expr(expr->ternary.cond));
  if (cond.type->kind != TYPE_INT && cond.type->kind != TYPE_PTR)
  {
    fatal("Ternary condition expression must have type int or ptr");
  }
  Operand then_expr = ptr_decay(resolve_expected_expr(expr->ternary.then, expected_type));
  Operand else_expr = ptr_decay(resolve_expected_expr(expr->ternary.else_, expected_type));
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

internal Operand
resolve_expr_index(Expr *expr)
{
  assert(expr->kind == EXPR_INDEX);

  Operand operand = ptr_decay(resolve_expr(expr->index.expr));
  if (operand.type->kind != TYPE_PTR && operand.type->kind != TYPE_ARRAY)
  {
    // IMPORTANT TODO(#23): make it so u can only index arrays and add multipointer like in odin [^] == [*]
    fatal("Can only index arrays or pointers");
  }
  Operand index   = resolve_expr(expr->index.index);
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

internal Operand
resolve_expr_cast(Expr *expr)
{
  assert(expr->kind == EXPR_CAST);

  Type *type = resolve_typespec(expr->cast.type);
  Operand result = ptr_decay(resolve_expr(expr->cast.expr));

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

internal Operand
resolve_expected_expr(Expr *expr, Type *expected_type)
{
  Operand result = {0};

  // TODO(#24): address constants
  switch (expr->kind)
  {
  case EXPR_INTEGER_LITERAL:
    result = resolved_const(expr->literal.integer);
    break;
  case EXPR_FLOAT_LITERAL:
    result = resolved_rvalue(type_float);
    break;
  case EXPR_STRING_LITERAL:
    // IMPORTANT TODO: get rid of char * and use sized string!!
    result = resolved_rvalue(type_ptr(type_char));
    break;
  case EXPR_CHAR_LITERAL:
    // result = resolved_rvalue(type_char);
    // TODO: for now this will just be converted to int const
    result = resolved_const(expr->literal.character);
    break;
  case EXPR_IDENT:
    result = resolve_expr_name(expr);
    break;
  case EXPR_COMPOUND:
    result = resolve_expr_compound(expr, expected_type);
    break;
  case EXPR_FIELD:
    result = resolve_expr_field(expr);
    break;
  case EXPR_INDEX:
    result = resolve_expr_index(expr);
    break;
  case EXPR_UNARY:
    result = resolve_expr_unary(expr);
    break;
  case EXPR_BINARY:
    result = resolve_expr_binary(expr);
    break;
  case EXPR_TERNARY:
    result = resolve_expr_ternary(expr, expected_type);
    break;
  case EXPR_CALL:
    result = resolve_expr_call(expr);
    break;
  case EXPR_CAST:
    result = resolve_expr_cast(expr);
    break;
  case EXPR_SIZE_OF:
  {
    Type *type = NULL;
    if (expr->size_of.is_expr)
    {
      type = resolve_expr(expr->size_of.expr).type;
    }
    else
    {
      type = resolve_typespec(expr->size_of.type);
    }
    complete_type(type);
    result = resolved_const(type_size_of(type));
    break;
  }
  case EXPR_GROUP:
  {
    result = resolve_expr(expr->group.expr);
    break;
  }
  default:
    assert(0);
    result = nil_operand;
    break;
  }

  if (result.type)
  {
    expr->type = result.type;
  }
  return result;
}

internal Operand
resolve_expr(Expr *expr)
{
  return resolve_expected_expr(expr, NULL);
}

internal s64
resolve_const_expr(Expr *expr)
{
  Operand result = resolve_expr(expr);
  if (!result.is_const)
  {
    fatal("Expected constant expression");
  }
  return result.const_value;
}

internal void
init_global_syms()
{
  sym_global_type(str8_lit("void"),  type_void);
  sym_global_type(str8_lit("char"),  type_char);
  sym_global_type(str8_lit("int"),   type_int);
  sym_global_type(str8_lit("float"), type_float);
}

internal void
sym_global_decl_list(Decl_List list)
{
  for each_node(it, Decl, list.first)
  {
    sym_global_decl(it);
  }
}

internal void
sym_global_complete_syms()
{
  for each_node(it, Sym_Node, global_syms.first)
  {
    complete_sym(it->v);
  }
}

internal void
resolve_test()
{
  printf("\n");
  printf("--- RESOLVE TEST\n");
  printf("\n");

  init_global_syms();

  /*
  TODO:
  [ ] Fix parsing of compound literals
  [ ] Fix being able to index array with constant index out of bounds
  */

  String8 source = S(
    "i: int\n"

    "f1 :: proc() {\n"
    "  N :: 8\n"
    "  a: [N]int\n"
    "  j := 0\n"
    "  i += 1\n"
    "  return\n"
    "}\n"

    "f2 :: proc() {\n"
    "  N :: 4\n"
    "  a: [N]int\n"
    // "  i += j\n" // this should error
    "}\n"

    "f3 :: proc(n: int) -> int {\n"
    "  square :: proc(m: int) -> int {\n"
    "    return 2*m\n" // TODO: this inner proc can access symbol `n`, this is wrong
    "  }\n"
    "  return square(n)\n"
    "}\n"

    "f4 :: proc(x: int) -> int {\n"
    "  if x {\n"
    "    return -x\n"
    "  } else if x % 2 == 0 {\n"
    "    return 2\n"
    "  } else {\n"
    "    return -1\n"
    "  }\n"
    "}\n"

    "f5 :: proc(n: int) -> int {\n"
    "  for i in 0 ..< n {\n"
    "    if i % 3 == 0 {\n"
    "      return n\n"
    "    }\n"
    "  }\n"
    "  return 0\n"
    "}\n"

    "f6 :: proc(x: int) -> int {\n"
    "  switch x {\n"
    "  case 0, 1:\n"
    "    return 42\n"
    "  case 3:\n"
    "    fallthrough\n"
    "  case:\n"
    "    return -1\n"
    "  }\n"
    "  return 0\n"
    "}\n"

    "f7 :: proc(n: int) -> int {\n"
    "  p := 1\n"
    "  while n > 0 {\n"
    "    p *= 2\n"
    "    n -= 1\n"
    "  }\n"
    "  return p\n"
    "}\n"

    "f8 :: proc(n: int) -> int {\n"
    "  p := 1\n"
    "  do {\n"
    "    p *= 2\n"
    "    n -= 1\n"
    "  } while n > 0\n"
    "  return p\n"
    "}\n"


    "some_number: int = 0\n"
    // "some_number += 1;\n"
    // "add_ints :: proc(a := 0, b: int) -> int {\n" // TODO: a gets inferred type void
    "add_ints :: proc(a: int, b: int) -> int {\n"
    "  c: int\n"
    "  c += a\n"
    "  c += b\n"
    "  return c\n"
    "}\n"

    "Int_Or_Ptr :: union { i: int, p: *int, }\n"
    "u1 := Int_Or_Ptr.{i = 42}\n"
    "u2 := Int_Or_Ptr.{p = cast(*int)42}\n"
    "ints := [256]int.{1, 2, ['a'] = 42, [255] = 123}\n"

    "a: [3]int = .{1,2,3}\n"
    // "a: [3]int\n"
    "a := [3]int.{1,2,3}\n"
    "b: [4]int\n"
    "p := &a[1]\n"
    "i := p[1]\n"
    "j := p.*\n"
    "N :: 5 + size_of(1 ? a : b)\n"
    "M :: size_of(&a[0])\n"

    "Vector :: struct { x, y: int, }\n"
    "v: Vector\n"
    "V_SIZE :: size_of(Vector)\n"
    "v: Vector = 0 ? .{1,2} : .{3,4}\n"
    "vs: [2][2]Vector = .{.{.{1,2},.{3,4}}, .{.{5,6},.{7,8}}}\n"

    "pi := 3.14\n"
    "name := \"stuff\"\n"

    "v: Vector = .{1,2}\n"
    "a := 42\n"
    "p := cast(*void)a\n"
    "j := cast(int)p\n"
    "q := cast(*int)j\n"

    "I :: 42\n"
    "J :: +I\n"
    "K :: -I\n"
    "L :: !!I\n"
    "M :: ~7 + 1 == -7\n"
    "A :: 1000/((2*3-5) << 1)\n"

    "K :: 1 ? 2 : 3\n"
    "Vector_Alias :: Vector\n"
    "addv :: proc(a: Vector_Alias, b: Vector) -> Vector { return .{ a.x + b.x, a.y + b.y }; }\n"
    "x := addv(Vector.{1,2}, Vector.{3,4})\n"
    "a: [3]int = .{1,2,3}\n"
    "i := a[1]\n"
    "p := &a[1]\n"
    "i := p[1]\n"
    "w := Vector.{3,4}\n"
    "i := 42\n"
    "u := Int_Or_Ptr.{ i, &i }\n"

    "P :: 1 + size_of(p)\n" // 9
    "p: *T\n" // 4
    "u := p.*\n" // deref
    "T :: struct { a: [N]int, }\n"
    "r := &t.a\n"
    "t: T\n"
    "S :: [N+M]int\n"
    "M :: size_of(t.a)\n" // 36
    "nm := N+M\n" // [9 + 36] = 45
    "ai := &i\n"
    /*
    */
  );

  Lexer l = lexer_init(source);
  Parser p = parser_init(&l);

  Decl_List list = parse_declarations(&p);
  sym_global_decl_list(list);
  sym_global_complete_syms();

  Arena_Temp scratch = arena_scratch_get(0, 0);

  for each_node(it, Sym_Node, ordered_global_syms.first)
  {
    Sym *sym = it->v;

    String8 result;
    if (sym->decl)
    {
      String8List list = {0};
      int indent = 0;
      print_decl(scratch.arena, &list, &indent, sym->decl);
      str8_list_pushf(scratch.arena, &list, "  [=%d]", sym->const_value);
      result = str8_list_join(scratch.arena, &list, NULL);
    }
    else
    {
      result = sym->name;
    }

    printf("%.*s\n", str8_varg(result));
  }

  arena_scratch_release(scratch);
}
