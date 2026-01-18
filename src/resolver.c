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

String8 source_text; // @HACK for getting the source from lexer without having the pass it to every function
// TODO: we can also add source_path here

internal void
fatal(Source_Pos pos, char *fmt, ...)
{
  va_list args;
  va_start(args, fmt);

  // printf(CLR_WHT "%.*s(%llu:%llu) ", str8_varg(source_file_path), pos.row, pos.col);
  printf(CLR_WHT "(%llu:%llu) ", pos.row, pos.col);
  printf(CLR_RED "error: " CLR_RESET);
  vprintf(fmt, args);
  printf("\n");

  u64 line_start = 0;
  u64 current_line = 1;

  // find the start of the error line
  for (u64 i = 0; i < source_text.count && current_line < pos.row; i++)
  {
    if (source_text.data[i] == '\n')
    {
      current_line++;
      line_start = i + 1;
    }
  }

  // find the end of the error line
  u64 line_end = line_start;
  while (line_end < source_text.count && 
         source_text.data[line_end] != '\n' && 
         source_text.data[line_end] != '\r')
  {
    line_end++;
  }

  int line_length = line_end - line_start;
  String8 line = str8_substr(source_text, line_start, line_end);

  u64 col0 = pos.col - 1;
  u64 tok_start = clamp_top(col0, line.count);
  u64 tok_end   = clamp_top(tok_start + pos.length, line.count);

  String8 before = str8_substr(line, 0, tok_start);
  String8 token  = str8_substr(line, tok_start, tok_end);
  String8 after  = str8_substr(line, tok_end, line.count);

  printf(CLR_CYN "%.*s", str8_varg(before));
  printf(CLR_RED "%.*s", str8_varg(token));
  printf(CLR_CYN "%.*s\n", str8_varg(after));

  printf("%*s", (int)tok_start, "");
  printf(CLR_GRN);
  for (u64 i = 0; i < token.count; i++) printf("^");
  printf(CLR_RESET "\n");

  va_end(args);
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
  TYPE_S8,
  TYPE_S16,
  TYPE_S32,
  TYPE_S64,
  TYPE_U8,
  TYPE_U16,
  TYPE_U32,
  TYPE_U64,
  TYPE_INT,
  TYPE_UINT,
  TYPE_F32,
  TYPE_F64,
  TYPE_STRING,
  TYPE_PTR,
  TYPE_ARRAY,
  TYPE_STRUCT,
  TYPE_UNION,
  TYPE_ENUM,
  TYPE_PROC,

  TYPE_UNTYPED_INT,
  TYPE_UNTYPED_FLOAT,
  TYPE_UNTYPED_BOOL,
  TYPE_UNTYPED_STRING, // TODO: string, cstring, cstring16 ...
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

internal Type *type_ptr(Type *base);

Type *type_void   = &(Type){ .kind = TYPE_VOID,   .size = 0,  .align = 0 };
Type *type_s8     = &(Type){ .kind = TYPE_S8,     .size = 1,  .align = 1 };
Type *type_s16    = &(Type){ .kind = TYPE_S16,    .size = 2,  .align = 2 };
Type *type_s32    = &(Type){ .kind = TYPE_S32,    .size = 4,  .align = 4 };
Type *type_s64    = &(Type){ .kind = TYPE_S64,    .size = 8,  .align = 8 };
Type *type_u8     = &(Type){ .kind = TYPE_U8,     .size = 1,  .align = 1 };
Type *type_u16    = &(Type){ .kind = TYPE_U16,    .size = 2,  .align = 2 };
Type *type_u32    = &(Type){ .kind = TYPE_U32,    .size = 4,  .align = 4 };
Type *type_u64    = &(Type){ .kind = TYPE_U64,    .size = 8,  .align = 8 };
Type *type_int    = &(Type){ .kind = TYPE_INT,    .size = 4,  .align = 4 }; // platform-sized   signed integer
Type *type_uint   = &(Type){ .kind = TYPE_UINT,   .size = 4,  .align = 4 }; // platform-sized unsigned integer
Type *type_f32    = &(Type){ .kind = TYPE_F32,    .size = 4,  .align = 4 };
Type *type_f64    = &(Type){ .kind = TYPE_F64,    .size = 8,  .align = 8 };
Type *type_string = &(Type){ .kind = TYPE_STRING, .size = 16, .align = 8 };

Type *type_untyped_int    = &(Type){ .kind = TYPE_UNTYPED_INT,    .size = 0, .align = 0 };
Type *type_untyped_float  = &(Type){ .kind = TYPE_UNTYPED_FLOAT,  .size = 0, .align = 0 };
Type *type_untyped_bool   = &(Type){ .kind = TYPE_UNTYPED_BOOL,   .size = 0, .align = 0 };
Type *type_untyped_string = &(Type){ .kind = TYPE_UNTYPED_STRING, .size = 0, .align = 0 };

internal void
init_string_type_fields()
{
  Type_Field_Array fields = {0};
  fields.count = 2;
  fields.v = push_array_nz(resolve_arena, Type_Field, fields.count);

  fields.v[0] = (Type_Field){ .name = S("data"), .type = type_ptr(type_u8) };
  fields.v[1] = (Type_Field){ .name = S("len"),  .type = type_u64 };

  type_string->aggregate.fields = fields;
}

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

  // Empty structs: size = 1, align = 1 (for C compatibility with opaque types)
  if (fields.count == 0)
  {
    type->size = 1;
    type->align = 1;
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

  // Empty unions: size = 1, align = 1
  if (fields.count == 0)
  {
    type->size = 1;
    type->align = 1;
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
  SymKind_Null,
  SymKind_Var,
  SymKind_Const,
  SymKind_Proc,
  SymKind_Type,
  SymKind_EnumConst,
};

ENUM(Sym_State)
{
  SymState_Unresolved,
  SymState_Resolving,
  SymState_Resolved,
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
SymKind_Var(String8 name, Type *type)
{
  Sym *sym = sym_alloc(SymKind_Var, name, NULL);
  sym->state = SymState_Resolved;
  sym->type  = type;
  return sym;
}

internal Sym *
sym_decl(Decl *decl)
{
  Sym_Kind kind = SymKind_Null;
  switch (decl->kind)
  {
  case DECL_VAR:
    kind = SymKind_Var;
    break;
  case DECL_CONST:
    // Type declarations: Matrix :: [4][4]int, Color :: enum {...}, etc.
    if (decl->init_type)
    {
      if (decl->init_type->kind == TYPE_SPEC_PROC)
      {
        kind = SymKind_Proc;
      }
      else
      {
        // All non-proc init_types are type aliases
        kind = SymKind_Type;
      }
    }
    // Constant value declarations: PI :: 3.14, N :: size_of(int), etc.
    else if (decl->init_expr)
    {
      kind = SymKind_Const;
    }
    else
    {
      // Should not happen - parser should ensure const has either init_type or init_expr
      fatal(decl->pos, "constant declaration '%.*s' has neither a type nor an expression", str8_varg(decl->name));
    }
    break;
  default:
    break;
  }

  Sym *sym = sym_alloc(kind, decl->name, decl);
  if (decl->init_type && (decl->init_type->kind == TYPE_SPEC_STRUCT || decl->init_type->kind == TYPE_SPEC_UNION))
  {
    sym->state = SymState_Resolved;
    sym->type  = type_incomplete(sym);
  }
  return sym;
}

internal Sym *
SymKind_EnumConst(String8 name, Decl *decl)
{
  return sym_alloc(SymKind_EnumConst, name, decl);
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
    fatal(sym->decl->pos, "too many local symbols");
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
      sym_list_push(&global_syms, SymKind_EnumConst(it.name, decl));
    }
  }
  return sym;
}

internal Sym *
sym_global_type(String8 name, Type *type)
{
  Sym *sym = sym_alloc(SymKind_Type, name, NULL);
  sym->state = SymState_Resolved;
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
internal Sym *resolve_name(Source_Pos pos, String8 name);
internal s64 resolve_const_expr(Expr *expr);
internal Operand resolve_expr(Expr *expr);
internal Operand resolve_expected_expr(Expr *expr, Type *expected_type);
internal Type *resolve_decl_var(Decl *decl);
internal void resolve_sym(Sym *sym);
internal String8 string_from_type(Arena *arena, Type *type);

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
    Sym *sym = resolve_name(typespec->pos, typespec->name);
    if (sym->kind != SymKind_Type)
    {
      fatal(sym->decl->pos, "%.*s must denote a type", str8_varg(typespec->name));
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
    fatal(type->sym->decl->pos, "type completion cycle");
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

  // Empty structs are allowed (useful for opaque/forward declarations in FFI)
  Type_Field_Array fields = {0};
  fields.count = total_field_count;
  if (total_field_count > 0)
  {
    fields.v = push_array(resolve_arena, Type_Field, fields.count);
  }

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

internal String8
string_from_type(Arena *arena, Type *type)
{
  // TODO: better strings like `[2]int` instead of `array`
  //                           `Person` instead of `struct`
  switch (type->kind)
  {
  case TYPE_NONE:       return str8_lit("<NONE>");
  case TYPE_INCOMPLETE: return str8_lit("<INCOMPLETE>");
  case TYPE_COMPLETING: return str8_lit("<COMPLETING>");
  case TYPE_VOID:       return str8_lit("void");
  case TYPE_CHAR:       return str8_lit("char");
  case TYPE_S8:         return str8_lit("s8");
  case TYPE_S16:        return str8_lit("s16");
  case TYPE_S32:        return str8_lit("s32");
  case TYPE_S64:        return str8_lit("s64");
  case TYPE_U8:         return str8_lit("u8");
  case TYPE_U16:        return str8_lit("u16");
  case TYPE_U32:        return str8_lit("u32");
  case TYPE_U64:        return str8_lit("u64");
  case TYPE_INT:        return str8_lit("int");
  case TYPE_UINT:       return str8_lit("uint");
  case TYPE_F32:        return str8_lit("f32");
  case TYPE_F64:        return str8_lit("f64");
  case TYPE_STRING:     return str8_lit("string");
  case TYPE_PTR:
  {
    return str8f(arena, "*%.*s", str8_varg(string_from_type(arena, type->ptr.base)));
  }
  case TYPE_ARRAY:
  {
    return str8f(arena, "[%llu]%.*s", type->array.length, str8_varg(string_from_type(arena, type->array.base)));
  }
  case TYPE_STRUCT:
  case TYPE_UNION:
  {
    return str8f(arena, "%.*s", str8_varg(type->sym->decl->name));
  }
  // case TYPE_ENUM:       return str8_lit("enum");
  // case TYPE_PROC:       return str8_lit("proc");
  }
  assert(0);
  return str8_lit("unreachable");
}

internal Type *
resolve_decl_var(Decl *decl)
{
  assert(decl->kind == DECL_VAR);

  Arena_Temp scratch = arena_scratch_get(0, 0);

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
      // TODO: the length of the decl->init_expr->pos is 1 even tho the string literal is like way longer???
      fatal(decl->init_expr->pos, "cannot assign `%.*s` to variable of type `%.*s`",
        str8_varg(string_from_type(scratch.arena, result.type)), str8_varg(string_from_type(scratch.arena, type)));
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
      // TODO: improve
      fatal(decl->init_type->pos, "declared variable type does not match type hint");
    }
  }

  arena_scratch_release(scratch);

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
    fatal(decl->init_expr->pos, "initializer is not a constant expression");
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
    fatal(expr->pos, "condition expression must be of type int");
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
          fatal(stmt->pos, "switch case expression type mismatch");
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
        fatal(stmt->pos, "return type mismatch");
      }
    }
    else
    {
      // Empty return statement
      if (ret_type && ret_type != type_void)
      {
        fatal(stmt->pos, "cannot return without a value from procedure with non-void return type");
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
    decl->sym = sym; // @HACK
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
  assert(sym->state == SymState_Resolved);

  if (decl->is_foreign)
  {
    // Skip body resolution for foreign procedures
    return;
  }

  Type_Spec *proc_spec = decl->init_type;
  Decl_Array params = proc_spec->proc.params;

  Sym **syms = sym_enter();
  for each_index(i, params.count)
  {
    Decl *param = params.v[i];
    sym_push(SymKind_Var(param->name, resolve_typespec(param->type_hint)));
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
  assert(sym->state == SymState_Resolved);
  Type *type = sym->type;
  assert(type->kind == TYPE_PROC);

  Sym **syms = sym_enter();
  for each_index(i, type->proc.params.count)
  {
    // Type_Param param = type->proc.params.v[i];
    // sym_push(SymKind_Var());
  }
  sym_leave(syms);
}

*/

internal void
resolve_sym(Sym *sym)
{
  if (sym->state == SymState_Resolved)
  {
    return;
  }
  else if (sym->state == SymState_Resolving)
  {
    fatal(sym->decl->pos, "cyclic dependency");
    return;
  }

  assert(sym->state == SymState_Unresolved);
  sym->state = SymState_Resolving;

  switch (sym->kind)
  {
  case SymKind_Type:
    sym->type = resolve_decl_type(sym->decl);
    break;
  case SymKind_Var:
    sym->type = resolve_decl_var(sym->decl);
    break;
  case SymKind_Const:
  {
    // TODO(#16): `distinct` keyword
    // My_Int :: distinct int
    // #assert(My_Int != int)

    // Check if this is actually a type alias (e.g., My_Int :: int)
    Decl *decl = sym->decl;
    // TODO i dont think we need this?
    // if (decl->init_expr && decl->init_expr->kind == EXPR_IDENT)
    // {
    //   Sym *ref_sym = sym_get(decl->init_expr->ident);
    //   if (ref_sym && ref_sym->kind == SymKind_Type)
    //   {
    //     // This is a type alias, not a constant
    //     // @CLEANUP
    //     sym->kind = SymKind_Type;
    //     resolve_sym(ref_sym);
    //     sym->type = ref_sym->type;
    //     break;
    //   }
    // }
    sym->type = resolve_decl_const(decl, &sym->const_value);
    break;
  }
  // case SymKind_TypeDEF:
    // sym->type = resolve_decl_type(sym->decl);
    // break;
  case SymKind_Proc:
    sym->type = resolve_decl_proc(sym->decl);
    break;
  default:
    assert(0);
    break;
  }

  sym->state = SymState_Resolved;

  // Only add to ordered_global_syms if it's actually in the global symbol table
  // (not a local variable or parameter inside a procedure)
  bool is_global = false;
  for each_node(it, Sym_Node, global_syms.first)
  {
    if (it->v == sym)
    {
      is_global = true;
      break;
    }
  }

  if (is_global)
  {
    sym_list_push(&ordered_global_syms, sym);
  }
}

internal void
complete_sym(Sym *sym)
{
  resolve_sym(sym);
  if (sym->kind == SymKind_Type)
  {
    complete_type(sym->type);
  }
  else if (sym->kind == SymKind_Proc)
  {
    resolve_proc(sym);
  }
}

internal Sym *
resolve_name(Source_Pos pos, String8 name)
{
  Sym *sym = sym_get(name);
  if (!sym)
  {
    // TODO: we could do a fuzzy search through all symbols and suggest????
    fatal(pos, "undeclared identifier `%.*s`", str8_varg(name));
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
  // TODO: for now we can do s.data, s.len on strings
  if (type->kind != TYPE_STRUCT && type->kind != TYPE_UNION && type->kind != TYPE_STRING)
  {
    fatal(expr->pos, "cannot access field on non struct/union/string type");
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
  Arena_Temp scratch = arena_scratch_get(0, 0);
  fatal(expr->pos, "`%.*s` has no field `%.*s`", str8_varg(string_from_type(scratch.arena, type)), str8_varg(expr->field.name));
  arena_scratch_release(scratch);
  // TODO: maybe we can show the struct definition?
  return nil_operand;
}

internal Operand
resolve_expr_name(Expr *expr)
{
  assert(expr->kind == EXPR_IDENT);
  Sym *sym = resolve_name(expr->pos, expr->ident);

  switch (sym->kind)
  {
  case SymKind_Var:
    return resolved_lvalue(sym->type);
  case SymKind_Const:
    return resolved_const(sym->const_value);
  case SymKind_Proc:
  case SymKind_Type:
    return resolved_rvalue(sym->type);
  default:
    fatal(expr->pos, "`%.*s` must be a var or const", str8_varg(expr->ident));
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
  case '+': return +value;
  case '-': return -value;
  case '!': return !value;
  case '~': return ~value;
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
    if (type->kind != TYPE_PTR)
    {
      // TODO: say which type
      fatal(expr->pos, "cannot dereference non-pointer type");
    }
    return resolved_lvalue(type->ptr.base);
  }
  case '&':
  {
    if (!operand.is_lvalue)
    {
      // TODO: say which type
      fatal(expr->pos, "cannot take address of non-lvalue");
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
      fatal(expr->pos, "can use unary `%.*s` with ints only", str8_varg(str_from_token_kind(scratch.arena, expr->unary.op.kind)));
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
  case '+':          return left + right;
  case '-':          return left - right;
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
  if (!is_integer_type(left.type))
  {
    fatal(expr->pos, "left operand of operator `%.*s` must be integer", str8_varg(str_from_token_kind(scratch.arena, expr->binary.op.kind)));
  }
  // if (left.type != right.type)
  if (!is_integer_type(left.type) && !is_integer_type(right.type))
  {
    String8 op  = str_from_token_kind(scratch.arena, expr->binary.op.kind);
    String8 lhs = string_from_type(scratch.arena, left.type);
    String8 rhs = string_from_type(scratch.arena, right.type);
    fatal(expr->pos, "operator `%.*s` cannot be applied to types `%.*s` and `%.*s`", str8_varg(op), str8_varg(lhs), str8_varg(rhs));
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
      fatal(expr->pos, "left side of assignment must be an lvalue");
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
  Arena_Temp scratch = arena_scratch_get(0, 0);
  fatal(type->sym->decl->pos, "`%.*s` has no field `%.*s`", str8_varg(string_from_type(scratch.arena, type)), str8_varg(name));
  arena_scratch_release(scratch);
  return SIZE_MAX;
}

internal Operand
resolve_expr_compound(Expr *expr, Type *expected_type)
{
  assert(expr->kind == EXPR_COMPOUND);

  if (!expected_type && !expr->compound.type)
  {
    fatal(expr->pos, "implicitly typed compound literal used in context without expected type");
  }

  Type *type = NULL;
  if (expr->compound.type)
  {
    type = resolve_typespec(expr->compound.type);
    // if (expected_type && expected_type != type)
    // {
    //   fatal("explicit compound literal type does not match expected type");
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
    fatal(expr->pos, "compound literals can only be used with struct, union and array types");
  }

  if (type->kind == TYPE_STRUCT || type->kind == TYPE_UNION)
  {
    assert(type->aggregate.fields.count > 0);
    // if (expr->compound.args.count > type->aggregate.fields.count)
    // {
    //   fatal("compound literal has too many fields");
    // }

    u32 index = 0;
    for each_index(i, expr->compound.args.count)
    {
      Compound_Field *field = expr->compound.args.v[i];
      if (field->kind == COMPOUND_FIELD_INDEX)
      {
        fatal(field->pos, "index field initializer not allowed for struct/union compound literal");
      }
      else if (field->kind == COMPOUND_FIELD_NAME)
      {
        index = aggregate_field_index(type, field->name);
      }
      if (index >= type->aggregate.fields.count)
      {
        fatal(field->pos, "field initializer in struct/union compound literal out of range");
      }
      Operand init = resolve_expected_expr(field->init, type->aggregate.fields.v[index].type);
      if (init.type != type->aggregate.fields.v[index].type)
      {
        fatal(field->pos, "compound literal field type mismatch");
      }
      // if (arg->optional_name.count > 0 && !str8_equal(arg->optional_name, type->aggregate.fields.v[i].name))
      // {
      //   // TODO: Make it so u can specify the named fields out of order
      //   fatal("compound literal field name mismatch");
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
        fatal(field->pos, "named field initializer not allowed in array compound literal");
      }
      else if (field->kind == COMPOUND_FIELD_INDEX)
      {
        s64 result = resolve_const_expr(field->index);
        if (result < 0)
        {
          fatal(field->pos, "field initializer index cannot be negative");
        }
        index = result;
      }
      if (index >= type->array.length)
      {
        fatal(field->pos, "array initializer has too many elements");
      }
      Operand init = resolve_expected_expr(field->init, type->array.base);
      if (init.type != type->array.base)
      {
        fatal(field->pos, "compound literal element type mismatch");
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
    fatal(expr->pos, "trying to call non-procedure value");
  }
  if (expr->call.args.count < proc.type->proc.params.count)
  {
    fatal(expr->pos, "too few arguments for procedure call");
  }
  else if (expr->call.args.count > proc.type->proc.params.count)
  {
    fatal(expr->pos, "too many arguments for procedure call");
  }

  for (u32 i = 0; i < expr->call.args.count; i += 1)
  {
    Type *param_type = proc.type->proc.params.v[i];
    Expr *arg_expr = expr->call.args.v[i];
    Operand arg = resolve_expected_expr(arg_expr, param_type);
    if (arg.type != param_type)
    {
      // TODO: improve
      fatal(arg_expr->pos, "procedure call argument type mismatch");
    }
  }

  return resolved_rvalue(proc.type->proc.ret);
}

internal Operand
resolve_expr_ternary(Expr *expr, Type *expected_type)
{
  // TODO(#22): have actual bool types
  assert(expr->kind == EXPR_TERNARY);
  Operand cond = resolve_expr(expr->ternary.cond);
  if (cond.type->kind != TYPE_INT && cond.type->kind != TYPE_PTR)
  {
    fatal(expr->pos, "ternary condition expression must have type int or ptr");
  }
  Operand then_expr = resolve_expected_expr(expr->ternary.then, expected_type);
  Operand else_expr = resolve_expected_expr(expr->ternary.else_, expected_type);
  if (then_expr.type != else_expr.type)
  {
    fatal(expr->pos, "ternary then/else expression must have matching types");
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

  Operand operand = resolve_expr(expr->index.expr);
  if (operand.type->kind != TYPE_PTR && operand.type->kind != TYPE_ARRAY)
  {
    // IMPORTANT TODO(#23): make it so u can only index arrays and add multipointer like in odin [^] == [*]
    fatal(expr->pos, "can only index arrays or pointers");
  }
  Operand index   = resolve_expr(expr->index.index);
  if (index.type->kind != TYPE_INT)
  {
    fatal(expr->pos, "index expression must be of type int");
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
  Operand result = resolve_expr(expr->cast.expr);

  // ptr -> ptr, ptr -> int, int -> ptr
  if (type->kind == TYPE_PTR)
  {
    if (result.type->kind != TYPE_PTR && !is_integer_type(result.type))
    {
      fatal(expr->pos, "invalid cast to pointer type");
    }
  }
  else if (is_integer_type(type))
  {
    if (result.type->kind != TYPE_PTR && !is_integer_type(result.type))
    {
      fatal(expr->pos, "invalid cast to int type");
    }
  }
  else
  {
    fatal(expr->pos, "invalid target cast type");
  }

  return resolved_rvalue(type);
}

internal b32
is_untyped(Type *type)
{
  switch (type->kind)
  {
  case TYPE_UNTYPED_INT:
  case TYPE_UNTYPED_FLOAT:
  case TYPE_UNTYPED_BOOL:
  case TYPE_UNTYPED_STRING:
    return true;
  }
  return false;
}

internal b32
is_integer_type(Type *type)
{
  switch (type->kind)
  {
  case TYPE_U8:
  case TYPE_U16:
  case TYPE_U32:
  case TYPE_U64:
  case TYPE_S8:
  case TYPE_S16:
  case TYPE_S32:
  case TYPE_S64:
  case TYPE_INT:
  case TYPE_UINT:
    return true;
  }
  return false;
}

internal b32
is_float_type(Type *type)
{
  switch (type->kind)
  {
  case TYPE_F32:
  case TYPE_F64:
    return true;
  }
  return false;
}

internal b32
can_convert_untyped(Type *from, Type *to)
{
  if (from->kind == TYPE_UNTYPED_INT)
  {
    return is_integer_type(to) || is_float_type(to);
  }
  if (from->kind == TYPE_UNTYPED_FLOAT)
  {
    return is_float_type(to);
  }
  if (from->kind == TYPE_UNTYPED_BOOL)
  {
    return to->kind == TYPE_INT; // TODO; dedicated bool type
  }
  return false;
}

internal Operand
convert_untyped(Operand operand, Type *target)
{
  if (!is_untyped(operand.type))
  {
    return operand;
  }
  if (target && can_convert_untyped(operand.type, target))
  {
    operand.type = target;
    return operand;
  }

  // default conversions when no target type
  switch (operand.type->kind)
  {
  case TYPE_UNTYPED_INT:   operand.type = type_int; break;
  case TYPE_UNTYPED_FLOAT: operand.type = type_f64; break;
  case TYPE_UNTYPED_BOOL:  operand.type = type_int; break; // TODO: actual bool type
  default: assert(0);
  }

  return operand;
}

internal Operand
resolve_expected_expr(Expr *expr, Type *expected_type)
{
  Operand result = {0};

  // TODO(#24): address constants
  switch (expr->kind)
  {
  case EXPR_INTEGER_LITERAL:
    // result = resolved_const(expr->literal.integer);
    result.type = type_untyped_int;
    result.is_const = true;
    result.const_value = expr->literal.integer;
    break;
  case EXPR_FLOAT_LITERAL:
    // result = resolved_rvalue(type_f32);
    result.type = type_untyped_float;
    result.is_const = true;
    // result.const_value_f32 = expr->literal.floating; // TODO
    printf("TODO: Expr float literal value.\n");
    break;
  case EXPR_STRING_LITERAL:
    result = resolved_rvalue(type_string);
    break;
  case EXPR_CHAR_LITERAL: // TODO: rename to rune
    // result = resolved_rvalue(type_char);
    // TODO(#26): for now this will just be converted to int const
    result = resolved_const(expr->literal.character);
    break;
  case EXPR_BOOL_LITERAL:
    // TODO: for now this will just be converted to int const
    // result = resolved_const(expr->literal.boolean);
    result.type = type_untyped_bool;
    result.is_const = true;
    result.const_value = expr->literal.boolean;
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

  // convert untyped to expected type
  result = convert_untyped(result, expected_type);

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
    fatal(expr->pos, "expected constant expression");
  }
  return result.const_value;
}

internal void
init_global_syms()
{
  // TODO: get rid of void?
  sym_global_type(str8_lit("void"),   type_void);
  sym_global_type(str8_lit("s8"),     type_s8);
  sym_global_type(str8_lit("s16"),    type_s16);
  sym_global_type(str8_lit("s32"),    type_s32);
  sym_global_type(str8_lit("s64"),    type_s64);
  sym_global_type(str8_lit("u8"),     type_u8);
  sym_global_type(str8_lit("u16"),    type_u16);
  sym_global_type(str8_lit("u32"),    type_u32);
  sym_global_type(str8_lit("u64"),    type_u64);
  sym_global_type(str8_lit("int"),    type_int);
  sym_global_type(str8_lit("uint"),   type_uint);
  sym_global_type(str8_lit("f32"),    type_f32);
  sym_global_type(str8_lit("f64"),    type_f64);
  sym_global_type(str8_lit("string"), type_string);

  init_string_type_fields();
}

internal void
sym_set_source_text(String8 source)
{
  // @HACK for getting the lexer source text into here
  source_text = source;
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
