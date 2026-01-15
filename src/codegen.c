#include "base.h"
#include "arena.h"
#include "strings.h"
#include "parser.h"
#include <stdarg.h>

//
//- TODO
// CLEAN UP EVERYTHING :d
//

//=============================================================================
//- C Declaration Builder (from old codegen)
//=============================================================================

STRUCT(CDecl_Builder)
{
  String8List type;   // "int ", "float ", ...
  String8List prefix; // "*", "("
  String8List suffix; // ")", "[3]"
};

STRUCT(Codegen)
{
  Arena *arena;
  String8List list;
  int indent;
  int temp_counter;
  
  // Track parameter -> temp mapping for current function
  String8 *param_names;
  String8 *param_temps;
  u64 param_count;
};

internal String8 gen_temp(Codegen *g, Type *type);

internal void
gen_pushf(Codegen *g, char *fmt, ...)
{
  va_list args;
  va_start(args, fmt);
  String8 result = str8fv(g->arena, fmt, args);
  va_end(args);
  str8_list_push(g->arena, &g->list, result);
}

internal void
gen_pushlnf(Codegen *g, char *fmt, ...)
{
  str8_list_pushf(g->arena, &g->list, "\n%.*s", 2*g->indent, "                                                  ");
  va_list args;
  va_start(args, fmt);
  String8 result = str8fv(g->arena, fmt, args);
  va_end(args);
  str8_list_push(g->arena, &g->list, result);
}

internal void
gen_preamble(Codegen *g)
{
  // TODO: inline this ??
  gen_pushf(g, "#include \"preamble.h\"\n\n");
}

internal String8 cdecl_make(Arena *arena, Type *type, String8 ident);
internal String8 gen_array_type_name(Arena *arena, Type *base, u64 length);

internal void gen_stmt(Codegen *g, Stmt *stmt, Type *ret_type);
internal void gen_stmt_block(Codegen *g, Stmt_Array block, Type *ret_type);

internal String8
cdecl_name(Arena *arena, Type *type)
{
  switch (type->kind)
  {
  case TYPE_VOID:   return str8_lit("void");
  case TYPE_S8:     return str8_lit("s8");
  case TYPE_S16:    return str8_lit("s16");
  case TYPE_S32:    return str8_lit("s32");
  case TYPE_S64:    return str8_lit("s64");
  case TYPE_U8:     return str8_lit("u8");
  case TYPE_U16:    return str8_lit("u16");
  case TYPE_U32:    return str8_lit("u32");
  case TYPE_U64:    return str8_lit("u64");
  case TYPE_INT:    return str8_lit("s64"); // TODO: use isize (platform-sized   signed integer)
  case TYPE_UINT:   return str8_lit("u64"); // TODO: use usize (platform-sized unsigned integer)
  case TYPE_F32:    return str8_lit("f32");
  case TYPE_F64:    return str8_lit("f64");
  case TYPE_STRING: return str8_lit("string");

  case TYPE_ARRAY: return gen_array_type_name(arena, type->array.base, type->array.length);

  case TYPE_STRUCT:
  case TYPE_UNION:
    return type->sym->name;

  case TYPE_PTR:
  {
    String8 base = cdecl_name(arena, type->ptr.base);
    return str8f(arena, "%.*s*", str8_varg(base));
  }

  default:
    assert(0);
    return (String8){0};
  }
}

// [type-specifiers] [prefix] IDENT [suffix]
internal void
cdecl_from_type(Arena *arena, CDecl_Builder *b, Type *type)
{
  Arena_Temp scratch = arena_scratch_get(&arena, 1);

  switch (type->kind)
  {
  case TYPE_VOID:
  case TYPE_S8:
  case TYPE_S16:
  case TYPE_S32:
  case TYPE_S64:
  case TYPE_U8:
  case TYPE_U16:
  case TYPE_U32:
  case TYPE_U64:
  case TYPE_INT:
  case TYPE_UINT:
  case TYPE_F32:
  case TYPE_F64:
  case TYPE_STRING:
  case TYPE_STRUCT:
  case TYPE_UNION:
    str8_list_push_frontf(arena, &b->type, "%.*s ", str8_varg(cdecl_name(scratch.arena, type)));
    break;
  case TYPE_PTR:
    cdecl_from_type(arena, b, type->ptr.base);

    if (type->ptr.base->kind == TYPE_ARRAY || type->ptr.base->kind == TYPE_PROC)
    {
      str8_list_push_frontf(arena, &b->prefix, "*(");
      str8_list_pushf(arena, &b->suffix, ")");
    }
    else
    {
      str8_list_push_frontf(arena, &b->prefix, "*");
    }
    break;
  case TYPE_ARRAY:
  {
    // Use struct wrapper to prevent array decay
    String8 struct_name = gen_array_type_name(arena, type->array.base, type->array.length);
    str8_list_push_frontf(arena, &b->type, "%.*s ", str8_varg(struct_name));
    break;
  }
  case TYPE_PROC:
    // return type
    cdecl_from_type(arena, b, type->proc.ret);

    //
    // TODO: if this is a forward decl it should not be a function pointer
    // @CLEANUP
    //
    // wrap
    // str8_list_push_frontf(arena, &b->prefix, "(*");
    // str8_list_pushf(arena, &b->suffix, ")");

    // build parameter list
    str8_list_pushf(arena, &b->suffix, "(");

    if (type->proc.params.count == 0)
    {
      str8_list_pushf(arena, &b->suffix, "void");
    }
    else
    {
      for each_index(i, type->proc.params.count)
      {
        if (i != 0)
        {
          str8_list_pushf(arena, &b->suffix, ", ");
        }

        CDecl_Builder param_builder = {0};
        cdecl_from_type(arena, &param_builder, type->proc.params.v[i]);

        str8_list_concat(&b->suffix, &param_builder.type);
        str8_list_concat(&b->suffix, &param_builder.prefix);
        str8_list_concat(&b->suffix, &param_builder.suffix);
      }
    }

    str8_list_pushf(arena, &b->suffix, ")");
    break;
  default:
    assert(0);
    break;
  }

  arena_scratch_release(scratch);
}

internal String8
cdecl_make(Arena *arena, Type *type, String8 ident)
{
  CDecl_Builder b = {0};
  cdecl_from_type(arena, &b, type);

  String8List out = {0};
  str8_list_concat(&out, &b.type);
  str8_list_concat(&out, &b.prefix);
  str8_list_push(arena, &out, ident);
  str8_list_concat(&out, &b.suffix);

  return str8_list_join(arena, &out, NULL);
}

internal String8
gen_array_type_name(Arena *arena, Type *base, u64 length)
{
  // Recursively build array type name
  Arena_Temp scratch = arena_scratch_get(&arena, 1);

  String8 base_name;
  if (base->kind == TYPE_ARRAY)
  {
    base_name = gen_array_type_name(arena, base->array.base, base->array.length);
  }
  else
  {
    base_name = cdecl_name(arena, base);
  }

  String8 result = str8f(arena, "Array_%llu_%.*s", length, str8_varg(base_name));

  arena_scratch_release(scratch);
  return result;
}

//=============================================================================
//- New Codegen Infrastructure
//=============================================================================

internal void
gen_array_typedefs(Codegen *g)
{
  gen_pushf(g, "//- Array Type Definitions\n");
  
  // Iterate through all cached array types from resolver
  for each_node(it, Cached_Array_Type, cached_array_types.first)
  {
    Type *array_type = &it->v;
    String8 struct_name = gen_array_type_name(g->arena, array_type->array.base, array_type->array.length);
    String8 elem_type_name;
    
    // Get element type name (might also be an array struct)
    if (array_type->array.base->kind == TYPE_ARRAY)
    {
      elem_type_name = gen_array_type_name(g->arena, 
                                           array_type->array.base->array.base, 
                                           array_type->array.base->array.length);
    }
    else
    {
      elem_type_name = cdecl_name(g->arena, array_type->array.base);
    }
    
    gen_pushf(g, 
      "typedef struct %.*s {\n"
      "  %.*s data[%llu];\n"
      "} %.*s;\n",
      str8_varg(struct_name),
      str8_varg(elem_type_name),
      array_type->array.length,
      str8_varg(struct_name)
    );
  }
  
  gen_pushf(g, "\n");
}

internal void
gen_forward_decls(Codegen *g)
{
  Arena_Temp scratch = arena_scratch_get(&g->arena, 1);

  gen_pushf(g, "//- Forward Declarations\n");

  for each_node(it, Sym_Node, global_syms.first)
  {
    Sym *sym = it->v;
    Decl *decl = sym->decl;
    if (!decl) continue;
    if (decl->kind != DECL_CONST) continue;
    if (!decl->init_type) continue;

    switch (decl->init_type->kind)
    {
    case TYPE_SPEC_STRUCT:
      gen_pushlnf(g, "typedef struct %.*s %.*s;", str8_varg(sym->name), str8_varg(sym->name));
      break;
    case TYPE_SPEC_UNION:
      gen_pushlnf(g, "typedef union %.*s %.*s;", str8_varg(sym->name), str8_varg(sym->name));
      break;
    case TYPE_SPEC_ENUM:
      gen_pushlnf(g, "typedef enum %.*s %.*s;", str8_varg(sym->name), str8_varg(sym->name));
      break;
    case TYPE_SPEC_PROC:
      gen_pushlnf(g, "%.*s;", str8_varg(cdecl_make(scratch.arena, sym->type, sym->name)));
      break;
    }
  }

  gen_pushf(g, "\n");
  arena_scratch_release(scratch);
}

internal void
gen_aggregate(Codegen *g, Sym *sym)
{
  assert(sym->type->kind == TYPE_STRUCT || sym->type->kind == TYPE_UNION);
  Decl *decl = sym->decl;
  Type *type = sym->type;

  gen_pushf(g, "%s %.*s {", 
    (sym->type->kind == TYPE_STRUCT ? "struct" : "union"), 
    str8_varg(decl->name));

  g->indent++;
  for each_index(i, type->aggregate.fields.count)
  {
    Type_Field it = type->aggregate.fields.v[i];
    String8 result = cdecl_make(g->arena, it.type, it.name);
    gen_pushlnf(g, "%.*s;", str8_varg(result));
  }
  g->indent--;

  gen_pushlnf(g, "};\n");
}

internal void
gen_proc(Codegen *g, Sym *sym)
{
  assert(sym->type->kind == TYPE_PROC);
  Decl *decl = sym->decl;
  Type *type = sym->type;

  // Generate function signature WITH NAMED PARAMETERS (preserves debug info!)
  gen_pushf(g, "%.*s %.*s(", 
    str8_varg(cdecl_name(g->arena, type->proc.ret)), 
    str8_varg(sym->name));
  
  // Parameters keep their original names for debugging!
  Decl_Array params = decl->init_type->proc.params;
  if (params.count == 0)
  {
    gen_pushf(g, "void");
  }
  else
  {
    for each_index(i, params.count)
    {
      if (i > 0) gen_pushf(g, ", ");
      
      Decl *param = params.v[i];
      String8 param_decl = cdecl_make(g->arena, type->proc.params.v[i], param->name);
      gen_pushf(g, "%.*s", str8_varg(param_decl));
    }
  }
  
  gen_pushf(g, ") {");
  g->indent++;
  
  // Generate temporaries for all parameters (fixes eval order UB)
  g->param_count = params.count;
  if (params.count > 0)
  {
    g->param_names = push_array(g->arena, String8, params.count);
    g->param_temps = push_array(g->arena, String8, params.count);
    
    for each_index(i, params.count)
    {
      Decl *param = params.v[i];
      g->param_names[i] = param->name;
      g->param_temps[i] = gen_temp(g, type->proc.params.v[i]);
      
      String8 type_name = cdecl_name(g->arena, type->proc.params.v[i]);
      gen_pushlnf(g, "%.*s %.*s = %.*s;", 
        str8_varg(type_name), str8_varg(g->param_temps[i]), str8_varg(param->name));
    }
  }
  
  // Generate body - references to parameters will use temps
  if (decl->init_type->proc.body)
  {
    gen_stmt_block(g, decl->init_type->proc.body->block, type->proc.ret);
  }
  
  // Clear param tracking
  g->param_count = 0;
  g->param_names = NULL;
  g->param_temps = NULL;
  
  g->indent--;
  gen_pushlnf(g, "}\n");
}

internal void
gen_sym(Codegen *g, Sym *sym)
{
  if (!sym->decl) return;
  Decl *decl = sym->decl;

  switch (decl->kind)
  {
  case DECL_VAR:
  {
    String8 result = cdecl_make(g->arena, sym->type, sym->name);
    gen_pushf(g, "%.*s = {0};\n", str8_varg(result));
    break;
  }
  case DECL_CONST:
  {
    // TODO: dont generate !!!:
    // N :: 40            -> int N = {0};
    // Alias :: My_Struct -> My_Struct Alias = {0};
    if (decl->init_type)
    {
      switch (decl->init_type->kind)
      {
      case TYPE_SPEC_STRUCT:
      case TYPE_SPEC_UNION:
        gen_aggregate(g, sym);
        break;
      case TYPE_SPEC_PROC:
        gen_proc(g, sym);
        break;
      default:
        break;
      }
    }
    break;
  }
  default:
    break;
  }
}

internal void
gen_ordered_syms(Codegen *g)
{
  gen_pushf(g, "//- Ordered Declarations\n");
  
  for each_node(it, Sym_Node, ordered_global_syms.first)
  {
    Sym *sym = it->v;
    gen_sym(g, sym);
  }
}

internal String8
gen_all(Arena *arena)
{
  Codegen g = {0};
  g.arena = arena;
  
  // 1. Preamble with safe operations
  gen_preamble(&g);
  
  // 2. Array struct typedefs
  gen_array_typedefs(&g);
  
  // 3. Forward declarations
  gen_forward_decls(&g);
  
  // 4. Ordered declarations (structs, procs, globals)
  gen_ordered_syms(&g);
  
  return str8_list_join(arena, &g.list, NULL);
}

//=============================================================================
//- Expression Generation (inline with temps only for function args)
//=============================================================================

internal String8
gen_temp(Codegen *g, Type *type)
{
  // Generate unique temporary variable name
  return str8f(g->arena, "__t%d", g->temp_counter++);
}

internal String8 gen_expr(Codegen *g, Expr *expr);

internal String8
gen_binary_op_name(Codegen *g, Token_Kind op, Type *type)
{
  char *base_name = NULL;
  switch (op)
  {
  case '+':           base_name = "add"; break;
  case '-':           base_name = "sub"; break;
  case '*':           base_name = "mul"; break;
  case '/':           base_name = "div"; break;
  case '%':           base_name = "mod"; break;
  case TOKEN_LSHIFT:  base_name = "shl"; break;
  case TOKEN_RSHIFT:  base_name = "shr"; break;
  case '&':           base_name = "and"; break;
  case '|':           base_name = "or";  break;
  case '^':           base_name = "xor"; break;
  case TOKEN_EQ:      base_name = "eq";  break;
  case TOKEN_NEQ:     base_name = "neq"; break;
  case '<':           base_name = "lt";  break;
  case '>':           base_name = "gt";  break;
  case TOKEN_LTEQ:    base_name = "lte"; break;
  case TOKEN_GTEQ:    base_name = "gte"; break;
  default: assert(0); return (String8){0};
  }
  
  char *type_suffix = NULL; // use cdecl name function here TODO
  switch (type->kind)
  {
  case TYPE_VOID:   type_suffix = "void";   break;
  case TYPE_S8:     type_suffix = "s8";     break;
  case TYPE_S16:    type_suffix = "s16";    break;
  case TYPE_S32:    type_suffix = "s32";    break;
  case TYPE_S64:    type_suffix = "s64";    break;
  case TYPE_U8:     type_suffix = "u8";     break;
  case TYPE_U16:    type_suffix = "u16";    break;
  case TYPE_U32:    type_suffix = "u32";    break;
  case TYPE_U64:    type_suffix = "u64";    break;
  case TYPE_INT:    type_suffix = "s64";    break; // TODO: use isize (platform-sized   signed integer)
  case TYPE_UINT:   type_suffix = "u64";    break; // TODO: use usize (platform-sized unsigned integer)
  case TYPE_F32:    type_suffix = "f32";    break;
  case TYPE_F64:    type_suffix = "f64";    break;
  case TYPE_STRING: type_suffix = "string"; break;
  default: assert(0); break;
  }
  
  return str8f(g->arena, "%s_%s", base_name, type_suffix);
}

// Generate expression as a C string - only uses temps for function arguments
internal String8
gen_expr(Codegen *g, Expr *expr)
{
  switch (expr->kind)
  {
  case EXPR_INTEGER_LITERAL:
    return str8f(g->arena, "%lld", expr->literal.integer);
    
  case EXPR_FLOAT_LITERAL:
    return str8f(g->arena, "%f", expr->literal.floating);
    
  case EXPR_STRING_LITERAL:
    return str8f(g->arena, "STR(\"%.*s\")", str8_varg(expr->literal.string));
    
  case EXPR_IDENT:
  {
    // Check if this identifier is a parameter - if so, use the temp
    for each_index(i, g->param_count)
    {
      if (str8_equal(expr->ident, g->param_names[i]))
      {
        return g->param_temps[i];
      }
    }
    // Not a parameter, use the identifier directly
    return expr->ident;
  }
  
  case EXPR_UNARY:
  {
    String8 operand = gen_expr(g, expr->unary.right);
    
    switch (expr->unary.op.kind)
    {
    case '-':
    {
      String8 op_func = str8f(g->arena, "neg_%s", 
        expr->type->kind == TYPE_INT ? "s32" : "f32");
      return str8f(g->arena, "%.*s(%.*s)", str8_varg(op_func), str8_varg(operand));
    }
    case '!':
      return str8f(g->arena, "!%.*s", str8_varg(operand));
    case '~':
    {
      String8 op_func = str8f(g->arena, "not_%s", 
        expr->type->kind == TYPE_INT ? "s32" : "s32");
      return str8f(g->arena, "%.*s(%.*s)", str8_varg(op_func), str8_varg(operand));
    }
    case '+':
      return operand;
    case TOKEN_DEREF:
      return str8f(g->arena, "*%.*s", str8_varg(operand));
    case '&':
      return str8f(g->arena, "&%.*s", str8_varg(operand));
    default:
      assert(0);
      return str8_lit("0");
    }
  }
    
  case EXPR_BINARY:
  {
    if (expr->binary.op.kind == '=')
    {
      // Assignment - emit the assignment statement and return lhs
      String8 lhs = gen_expr(g, expr->binary.left);
      String8 rhs = gen_expr(g, expr->binary.right);
      gen_pushlnf(g, "%.*s = %.*s;", str8_varg(lhs), str8_varg(rhs));
      return lhs;
    }
    else
    {
      // Regular binary operation
      String8 left = gen_expr(g, expr->binary.left);
      String8 right = gen_expr(g, expr->binary.right);
      String8 op_func = {0};
      bool is_compound_assign = false;
      switch (expr->binary.op.kind)
      {
      case TOKEN_LOGICAL_AND: return str8f(g->arena, "%.*s && %.*s", str8_varg(left), str8_varg(right)); break;
      case TOKEN_LOGICAL_OR:  return str8f(g->arena, "%.*s || %.*s", str8_varg(left), str8_varg(right)); break;
      case TOKEN_LSHIFT_ASSIGN: op_func = gen_binary_op_name(g, TOKEN_LSHIFT, expr->type); is_compound_assign = true; break;
      case TOKEN_RSHIFT_ASSIGN: op_func = gen_binary_op_name(g, TOKEN_RSHIFT, expr->type); is_compound_assign = true; break;
      case TOKEN_ADD_ASSIGN:    op_func = gen_binary_op_name(g, '+', expr->type);          is_compound_assign = true; break;
      case TOKEN_SUB_ASSIGN:    op_func = gen_binary_op_name(g, '-', expr->type);          is_compound_assign = true; break;
      case TOKEN_DIV_ASSIGN:    op_func = gen_binary_op_name(g, '/', expr->type);          is_compound_assign = true; break;
      case TOKEN_MUL_ASSIGN:    op_func = gen_binary_op_name(g, '*', expr->type);          is_compound_assign = true; break;
      case TOKEN_AND_ASSIGN:    op_func = gen_binary_op_name(g, '&', expr->type);          is_compound_assign = true; break;
      case TOKEN_OR_ASSIGN:     op_func = gen_binary_op_name(g, '|', expr->type);          is_compound_assign = true; break;
      case TOKEN_XOR_ASSIGN:    op_func = gen_binary_op_name(g, '^', expr->type);          is_compound_assign = true; break;
      default: op_func = gen_binary_op_name(g, expr->binary.op.kind, expr->type); break;
      }

      if (is_compound_assign)
      {
        return str8f(g->arena, "%.*s = %.*s(%.*s, %.*s)", str8_varg(left), str8_varg(op_func), str8_varg(left), str8_varg(right));
      }
      return str8f(g->arena, "%.*s(%.*s, %.*s)", str8_varg(op_func), str8_varg(left), str8_varg(right));
    }
  }
  
  case EXPR_CALL:
  {
    // Just generate the call directly - temporaries handled in function body
    String8 callee_name = expr->call.expr->ident;
    String8List call_parts = {0};
    str8_list_pushf(g->arena, &call_parts, "%.*s(", str8_varg(callee_name));
    
    for each_index(i, expr->call.args.count)
    {
      if (i > 0) str8_list_push(g->arena, &call_parts, str8_lit(", "));
      Expr *arg = expr->call.args.v[i];
      String8 arg_expr = gen_expr(g, arg);
      str8_list_pushf(g->arena, &call_parts, "%.*s", str8_varg(arg_expr));
    }
    
    str8_list_push(g->arena, &call_parts, str8_lit(")"));
    return str8_list_join(g->arena, &call_parts, NULL);
  }
  
  case EXPR_INDEX:
  {
    // Array indexing: a[i] becomes a.data[i]
    String8 array = gen_expr(g, expr->index.expr);
    String8 index = gen_expr(g, expr->index.index);
    return str8f(g->arena, "%.*s.data[%.*s]", str8_varg(array), str8_varg(index));
  }
  
  case EXPR_FIELD:
  {
    // Struct field access: v.x
    String8 operand = gen_expr(g, expr->field.expr);
    return str8f(g->arena, "%.*s.%.*s", str8_varg(operand), str8_varg(expr->field.name));
  }
  
  case EXPR_CAST:
  {
    String8 operand = gen_expr(g, expr->cast.expr);
    String8 target_type = cdecl_name(g->arena, expr->type);
    return str8f(g->arena, "(%.*s)%.*s", str8_varg(target_type), str8_varg(operand));
  }
  
  case EXPR_TERNARY:
  {
    String8 cond = gen_expr(g, expr->ternary.cond);
    String8 then = gen_expr(g, expr->ternary.then);
    String8 else_ = gen_expr(g, expr->ternary.else_);
    return str8f(g->arena, "%.*s ? %.*s : %.*s", 
      str8_varg(cond), str8_varg(then), str8_varg(else_));
  }

  case EXPR_COMPOUND:
  {
    // Compound literals need to be assigned field-by-field
    // Create a temp, assign fields, return temp name
    String8 temp = gen_temp(g, expr->type);
    String8 type_name = cdecl_name(g->arena, expr->type);
    
    gen_pushlnf(g, "%.*s %.*s = {0};", str8_varg(type_name), str8_varg(temp));
    
    for each_index(i, expr->compound.args.count)
    {
      Compound_Field *field = expr->compound.args.v[i];
      String8 value = gen_expr(g, field->init);
      
      switch (field->kind)
      {
      case COMPOUND_FIELD_NAME:
        gen_pushlnf(g, "%.*s.%.*s = %.*s;", 
          str8_varg(temp), str8_varg(field->name), str8_varg(value));
        break;
        
      case COMPOUND_FIELD_INDEX:
      {
        String8 index = gen_expr(g, field->index);
        gen_pushlnf(g, "%.*s.data[%.*s] = %.*s;", 
          str8_varg(temp), str8_varg(index), str8_varg(value));
        break;
      }
        
      case COMPOUND_FIELD_NONE:
        if (expr->type->kind == TYPE_ARRAY)
        {
          gen_pushlnf(g, "%.*s.data[%llu] = %.*s;", 
            str8_varg(temp), i, str8_varg(value));
        }
        else
        {
          Type_Field struct_field = expr->type->aggregate.fields.v[i];
          gen_pushlnf(g, "%.*s.%.*s = %.*s;", 
            str8_varg(temp), str8_varg(struct_field.name), str8_varg(value));
        }
        break;
      }
    }
    
    return temp;
  }

  case EXPR_SIZE_OF:
  {
    Type *target_type = expr->size_of.is_expr ? expr->size_of.expr->type : expr->type;
    return str8f(g->arena, "/*size_of*/%d", target_type->size);
    // String8 type_name = cdecl_name(g->arena, target_type);
    // return str8f(g->arena, "sizeof(%.*s)", str8_varg(type_name));
  }

  default:
    return str8_lit("/* TODO: expr */");
  }
}

internal void
gen_stmt(Codegen *g, Stmt *stmt, Type *ret_type)
{
  switch (stmt->kind)
  {
  case STMT_DECL:
  {
    Decl *decl = stmt->decl;
    Type *type = decl->sym->type;
    
    String8 var_name = decl->name;
    String8 type_name = cdecl_name(g->arena, type);
    
    if (decl->init_expr)
    {
      String8 init_val = gen_expr(g, decl->init_expr);
      gen_pushlnf(g, "%.*s %.*s = %.*s;", 
        str8_varg(type_name), str8_varg(var_name), str8_varg(init_val));
    }
    else
    {
      gen_pushlnf(g, "%.*s %.*s = {0};", str8_varg(type_name), str8_varg(var_name));
    }
    break;
  }

  case STMT_EXPR:
  {
    // Just evaluate the expression (might have side effects like assignments or calls)
    String8 expr_val = gen_expr(g, stmt->expr);
    // For pure expression statements (not assignments which gen_expr handles),
    // we would emit it, but assignments already emit themselves
    if (stmt->expr->kind != EXPR_BINARY || stmt->expr->binary.op.kind != '=')
    {
      // For non-assignment expressions (like function calls), emit as statement
      gen_pushlnf(g, "%.*s;", str8_varg(expr_val));
    }
    break;
  }
  
  case STMT_RETURN:
  {
    if (stmt->return_expr)
    {
      String8 ret_val = gen_expr(g, stmt->return_expr);
      gen_pushlnf(g, "return %.*s;", str8_varg(ret_val));
    }
    else
    {
      gen_pushlnf(g, "return;");
    }
    break;
  }
  
  case STMT_IF:
  {
    String8 cond = gen_expr(g, stmt->if0.cond);
    gen_pushlnf(g, "if (%.*s) {", str8_varg(cond));
    g->indent++;
    gen_stmt_block(g, stmt->if0.then_block->block, ret_type);
    g->indent--;

    Stmt *else_part = stmt->if0.else_stmt;
    while (else_part != NULL)
    {
      if (else_part->kind == STMT_IF)
      {
        // else if - generate on same line as closing brace
        String8 else_cond = gen_expr(g, else_part->if0.cond);
        gen_pushlnf(g, "} else if (%.*s) {", str8_varg(else_cond));
        g->indent++;
        gen_stmt_block(g, else_part->if0.then_block->block, ret_type);
        g->indent--;
        else_part = else_part->if0.else_stmt;
      }
      else
      {
        // final else block
        assert(else_part->kind == STMT_BLOCK);
        gen_pushlnf(g, "} else {");
        g->indent++;
        gen_stmt_block(g, else_part->block, ret_type);
        g->indent--;
        else_part = NULL;
      }
    }
    
    gen_pushlnf(g, "}");
    break;
  }
  
  case STMT_WHILE:
  {
    String8 cond = gen_expr(g, stmt->while0.cond);
    gen_pushlnf(g, "while (%.*s) {", str8_varg(cond));
    g->indent++;
    // gen_pushlnf(g, "if (!%.*s) break;", str8_varg(cond));
    gen_stmt_block(g, stmt->while0.body->block, ret_type);
    g->indent--;
    gen_pushlnf(g, "}");
    break;
  }
  
  case STMT_DO_WHILE:
  {
    gen_pushlnf(g, "do {");
    g->indent++;
    gen_stmt_block(g, stmt->while0.body->block, ret_type);
    g->indent--;
    gen_pushlnf(g, "}");
    String8 cond = gen_expr(g, stmt->while0.cond);
    gen_pushf(g, " while (%.*s);", str8_varg(cond));
    break;
  }
  
  case STMT_FOR:
  {
    gen_pushlnf(g, "// TODO: for loop");
    // TODO: need to make gen_stmt return a string8 like gen_expr with an option to disable semicolon
    // gen_pushlnf(g, "");
    // gen_pushf(g, "for (");
    // gen_stmt(g, stmt->for0.init, NULL);
    // String8 cond = gen_expr(g, stmt->for0.cond);
    // gen_pushf(g, " %.*s;", str8_varg(cond));
    // gen_stmt(g, stmt->for0.loop, NULL); // TODO this generates an extra ; which causes an error,
    // gen_pushf(g, ") {");
    // g->indent++;
    // gen_stmt_block(g, stmt->for0.body->block, NULL);
    // g->indent--;
    // gen_pushlnf(g, "}");
    break;
  }

  case STMT_FOR_IN:
  {
    gen_pushlnf(g, "// TODO: for in loop");
    break;
  }
  
  case STMT_SWITCH:
  {
    gen_pushlnf(g, "// TODO: switch statement");
    break;
  }
  
  case STMT_BLOCK:
  {
    gen_pushlnf(g, "{");
    g->indent++;
    gen_stmt_block(g, stmt->block, ret_type);
    g->indent--;
    gen_pushlnf(g, "}");
    break;
  }
  
  case STMT_BREAK:
    gen_pushlnf(g, "break;");
    break;
    
  case STMT_CONTINUE:
    gen_pushlnf(g, "continue;");
    break;
  
  default:
    gen_pushlnf(g, "// TODO: stmt kind %d", stmt->kind);
    break;
  }
}

internal void
gen_stmt_block(Codegen *g, Stmt_Array block, Type *ret_type)
{
  for each_index(i, block.count)
  {
    gen_stmt(g, block.v[i], ret_type);
  }
}
