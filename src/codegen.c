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
};

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

internal String8 gen_array_type_name(Arena *arena, Type *base, u64 length);
internal void gen_expr_simple(Codegen *g, Expr *expr, String8 dest);

internal String8
cdecl_name(Arena *arena, Type *type)
{
  switch (type->kind)
  {
  case TYPE_VOID:  return str8_lit("void");
  case TYPE_CHAR:  return str8_lit("char");
  case TYPE_INT:   return str8_lit("int");
  case TYPE_FLOAT: return str8_lit("float");

  case TYPE_ARRAY: return gen_array_type_name(arena, type, type->array.length);

  case TYPE_STRUCT:
  case TYPE_UNION:
    return type->sym->name;

  default:
    assert(0);
    return (String8){0};
  }
}

// [type-specifiers] [prefix] IDENT [suffix]
internal void
cdecl_from_type(Arena *arena, CDecl_Builder *b, Type *type)
{
  switch (type->kind)
  {
  case TYPE_VOID:
  case TYPE_CHAR:
  case TYPE_INT:
  case TYPE_FLOAT:
  case TYPE_STRUCT:
  case TYPE_UNION:
    str8_list_push_frontf(arena, &b->type, "%.*s ", str8_varg(cdecl_name(arena, type)));
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

    // wrap
    str8_list_push_frontf(arena, &b->prefix, "(*");
    str8_list_pushf(arena, &b->suffix, ")");

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
  String8 base_name;
  if (base->kind == TYPE_ARRAY)
  {
    base_name = gen_array_type_name(arena, base->array.base, base->array.length);
  }
  else
  {
    base_name = cdecl_name(arena, base);
  }
  
  return str8f(arena, "Array_%llu_%.*s", length, str8_varg(base_name));
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
  
  // Generate body - parameters are already named variables!
  if (decl->init_type->proc.body)
  {
    gen_stmt_block(g, decl->init_type->proc.body->block, type->proc.ret);
  }
  
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
//- Expression Lowering (SSA-style with temporaries)
//=============================================================================

internal String8
gen_temp(Codegen *g, Type *type)
{
  // Generate unique temporary variable name
  return str8f(g->arena, "__t%d", g->temp_counter++);
}

internal String8
gen_binary_op_name(Codegen *g, Token_Kind op, Type *type)
{
  char *base_name = NULL;
  switch (op)
  {
  case '=':           base_name = "(TODO:=)"; break;
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
  
  char *type_suffix = NULL;
  switch (type->kind)
  {
  case TYPE_INT:   type_suffix = "s32"; break;
  case TYPE_CHAR:  type_suffix = "s8";  break;
  case TYPE_FLOAT: type_suffix = "f32"; break;
  // TODO: Extend for more types (s64, u32, etc.)
  default: type_suffix = "s32"; break;
  }
  
  return str8f(g->arena, "%s_%s", base_name, type_suffix);
}

internal void
gen_expr(Codegen *g, Expr *expr, String8 dest)
{
  switch (expr->kind)
  {
  case EXPR_INTEGER_LITERAL:
    gen_pushlnf(g, "%.*s = %lld;", str8_varg(dest), expr->literal.integer);
    break;
    
  case EXPR_FLOAT_LITERAL:
    gen_pushlnf(g, "%.*s = %f;", str8_varg(dest), expr->literal.floating);
    break;
    
  case EXPR_STRING_LITERAL:
    gen_pushlnf(g, "%.*s = \"%.*s\";", str8_varg(dest), str8_varg(expr->literal.string));
    break;
    
  case EXPR_IDENT:
  {
    // Use the variable name directly - preserves debug info!
    // No symbol lookup needed - just use the identifier name
    gen_pushlnf(g, "%.*s = %.*s;", str8_varg(dest), str8_varg(expr->ident));
    break;
  }
  
  case EXPR_UNARY:
  {
    String8 operand_temp = gen_temp(g, expr->unary.right->type);
    String8 type_name = cdecl_name(g->arena, expr->type);
    
    if (expr_is_simple(expr->unary.right))
    {
      gen_pushlnf(g, "%.*s %.*s = ", str8_varg(type_name), str8_varg(operand_temp));
      gen_expr_simple(g, expr->unary.right, operand_temp);
      gen_pushf(g, ";");
    }
    else
    {
      gen_pushlnf(g, "%.*s %.*s;", str8_varg(type_name), str8_varg(operand_temp));
      gen_expr(g, expr->unary.right, operand_temp);
    }
    
    switch (expr->unary.op.kind)
    {
    case '-':
    {
      String8 op_func = str8f(g->arena, "neg_%s", 
        expr->type->kind == TYPE_INT ? "s32" : "f32");
      gen_pushlnf(g, "%.*s = %.*s(%.*s);", 
        str8_varg(dest), str8_varg(op_func), str8_varg(operand_temp));
      break;
    }
    case '!':
      gen_pushlnf(g, "%.*s = !%.*s;", str8_varg(dest), str8_varg(operand_temp));
      break;
    case '~':
    {
      String8 op_func = str8f(g->arena, "not_%s", 
        expr->type->kind == TYPE_INT ? "s32" : "s32");
      gen_pushlnf(g, "%.*s = %.*s(%.*s);", 
        str8_varg(dest), str8_varg(op_func), str8_varg(operand_temp));
      break;
    }
    case '+':
      gen_pushlnf(g, "%.*s = %.*s;", str8_varg(dest), str8_varg(operand_temp));
      break;
    default:
      assert(0);
      break;
    }
    break;
  }
    
  case EXPR_BINARY:
  {
    if (expr->binary.op.kind == '=')
    {
      // Assignment: evaluate rhs and assign to lhs
      Expr *lhs = expr->binary.left;
      Expr *rhs = expr->binary.right;
      
      if (expr_is_simple(rhs) && lhs->kind == EXPR_IDENT)
      {
        // Simple assignment: x = 5 or x = y
        gen_pushlnf(g, "%.*s = ", str8_varg(lhs->ident));
        gen_expr_simple(g, rhs, lhs->ident);
        gen_pushf(g, ";");
        // Assignment expression evaluates to the assigned value
        gen_pushlnf(g, "%.*s = %.*s;", str8_varg(dest), str8_varg(lhs->ident));
      }
      else
      {
        // Complex assignment - use temporary for rhs
        String8 rhs_temp = gen_temp(g, rhs->type);
        String8 type_name = cdecl_name(g->arena, rhs->type);
        
        gen_pushlnf(g, "%.*s %.*s;", str8_varg(type_name), str8_varg(rhs_temp));
        gen_expr(g, rhs, rhs_temp);
        
        if (lhs->kind == EXPR_IDENT)
        {
          gen_pushlnf(g, "%.*s = %.*s;", str8_varg(lhs->ident), str8_varg(rhs_temp));
          gen_pushlnf(g, "%.*s = %.*s;", str8_varg(dest), str8_varg(rhs_temp));
        }
        else
        {
          // TODO: Handle complex lvalue like arr[i], v.field, *ptr
          gen_pushlnf(g, "/* TODO: complex lvalue assignment */");
          gen_pushlnf(g, "%.*s = %.*s;", str8_varg(dest), str8_varg(rhs_temp));
        }
      }
    }
    else
    {
      // Regular binary operation
      // Generate temporaries for operands
      String8 left_temp = gen_temp(g, expr->binary.left->type);
      String8 right_temp = gen_temp(g, expr->binary.right->type);
      
      String8 type_name = cdecl_name(g->arena, expr->type);
      
      if (expr_is_simple(expr->binary.left))
      {
        gen_pushlnf(g, "%.*s %.*s = ", str8_varg(type_name), str8_varg(left_temp));
        gen_expr_simple(g, expr->binary.left, left_temp);
        gen_pushf(g, ";");
      }
      else
      {
        gen_pushlnf(g, "%.*s %.*s;", str8_varg(type_name), str8_varg(left_temp));
        gen_expr(g, expr->binary.left, left_temp);
      }
      
      if (expr_is_simple(expr->binary.right))
      {
        gen_pushlnf(g, "%.*s %.*s = ", str8_varg(type_name), str8_varg(right_temp));
        gen_expr_simple(g, expr->binary.right, right_temp);
        gen_pushf(g, ";");
      }
      else
      {
        gen_pushlnf(g, "%.*s %.*s;", str8_varg(type_name), str8_varg(right_temp));
        gen_expr(g, expr->binary.right, right_temp);
      }
      
      // Use safe operation
      String8 op_func = gen_binary_op_name(g, expr->binary.op.kind, expr->type);
      gen_pushlnf(g, "%.*s = %.*s(%.*s, %.*s);", 
        str8_varg(dest), str8_varg(op_func),
        str8_varg(left_temp), str8_varg(right_temp));
    }
    break;
  }
  
  case EXPR_CALL:
  {
    // Evaluate all arguments into temporaries first (fixes param eval order UB)
    String8 *arg_temps = push_array(g->arena, String8, expr->call.args.count);
    
    for each_index(i, expr->call.args.count)
    {
      Expr *arg = expr->call.args.v[i];
      arg_temps[i] = gen_temp(g, arg->type);
      
      String8 type_name = cdecl_name(g->arena, arg->type);
      if (expr_is_simple(arg))
      {
        gen_pushlnf(g, "%.*s %.*s = ", str8_varg(type_name), str8_varg(arg_temps[i]));
        gen_expr_simple(g, arg, arg_temps[i]);
        gen_pushf(g, ";");
      }
      else
      {
        gen_pushlnf(g, "%.*s %.*s;", str8_varg(type_name), str8_varg(arg_temps[i]));
        gen_expr(g, arg, arg_temps[i]);
      }
    }
    
    // Now call with all arguments evaluated
    // Just use the function name from the expression
    String8 callee_name = expr->call.expr->ident;
    gen_pushlnf(g, "%.*s = %.*s(", str8_varg(dest), str8_varg(callee_name));
    
    for each_index(i, expr->call.args.count)
    {
      if (i > 0) gen_pushf(g, ", ");
      gen_pushf(g, "%.*s", str8_varg(arg_temps[i]));
    }
    
    gen_pushf(g, ");");
    break;
  }
  
  case EXPR_INDEX:
  {
    // Array indexing: a[i] becomes a.data[i]
    String8 array_temp = gen_temp(g, expr->index.expr->type);
    String8 index_temp = gen_temp(g, expr->index.index->type);
    
    String8 array_type = cdecl_name(g->arena, expr->index.expr->type);
    gen_pushlnf(g, "%.*s %.*s;", str8_varg(array_type), str8_varg(array_temp));
    gen_pushlnf(g, "int %.*s;", str8_varg(index_temp));
    
    gen_expr(g, expr->index.expr, array_temp);
    gen_expr(g, expr->index.index, index_temp);
    
    gen_pushlnf(g, "%.*s = %.*s.data[%.*s];", 
      str8_varg(dest), str8_varg(array_temp), str8_varg(index_temp));
    break;
  }
  
  case EXPR_FIELD:
  {
    // Struct field access: v.x
    String8 operand_temp = gen_temp(g, expr->field.expr->type);
    String8 operand_type = cdecl_name(g->arena, expr->field.expr->type);
    
    gen_pushlnf(g, "%.*s %.*s;", str8_varg(operand_type), str8_varg(operand_temp));
    gen_expr(g, expr->field.expr, operand_temp);
    
    gen_pushlnf(g, "%.*s = %.*s.%.*s;", 
      str8_varg(dest), str8_varg(operand_temp), str8_varg(expr->field.name));
    break;
  }

  //TODO
  // case EXPR_ADDR_OF:
  // {
  //   String8 operand_temp = gen_temp(g, expr->unary.operand->type);
  //   String8 operand_type = cdecl_name(g->arena, expr->unary.operand->type);
    
  //   gen_pushlnf(g, "%.*s %.*s;", str8_varg(operand_type), str8_varg(operand_temp));
  //   gen_expr(g, expr->unary.operand, operand_temp);
    
  //   gen_pushlnf(g, "%.*s = &%.*s;", str8_varg(dest), str8_varg(operand_temp));
  //   break;
  // }
  
  // case EXPR_DEREF:
  // {
  //   String8 operand_temp = gen_temp(g, expr->unary.operand->type);
  //   String8 operand_type = cdecl_name(g->arena, expr->unary.operand->type);
    
  //   gen_pushlnf(g, "%.*s %.*s;", str8_varg(operand_type), str8_varg(operand_temp));
  //   gen_expr(g, expr->unary.operand, operand_temp);
    
  //   gen_pushlnf(g, "%.*s = *%.*s;", str8_varg(dest), str8_varg(operand_temp));
  //   break;
  // }
  
  case EXPR_CAST:
  {
    String8 operand_temp = gen_temp(g, expr->cast.expr->type);
    String8 operand_type = cdecl_name(g->arena, expr->cast.expr->type);
    String8 target_type = cdecl_name(g->arena, expr->type);
    
    gen_pushlnf(g, "%.*s %.*s;", str8_varg(operand_type), str8_varg(operand_temp));
    gen_expr(g, expr->cast.expr, operand_temp);
    
    gen_pushlnf(g, "%.*s = (%.*s)%.*s;", 
      str8_varg(dest), str8_varg(target_type), str8_varg(operand_temp));
    break;
  }
  
  case EXPR_TERNARY:
  {
    String8 cond_temp = gen_temp(g, expr->ternary.cond->type);
    String8 then_temp = gen_temp(g, expr->type);
    String8 else_temp = gen_temp(g, expr->type);
    
    String8 type_name = cdecl_name(g->arena, expr->type);
    
    gen_pushlnf(g, "bool %.*s;", str8_varg(cond_temp));
    gen_expr(g, expr->ternary.cond, cond_temp);
    
    gen_pushlnf(g, "%.*s %.*s;", str8_varg(type_name), str8_varg(then_temp));
    gen_pushlnf(g, "%.*s %.*s;", str8_varg(type_name), str8_varg(else_temp));
    
    gen_expr(g, expr->ternary.then, then_temp);
    gen_expr(g, expr->ternary.else_, else_temp);
    
    gen_pushlnf(g, "%.*s = %.*s ? %.*s : %.*s;", 
      str8_varg(dest), str8_varg(cond_temp), 
      str8_varg(then_temp), str8_varg(else_temp));
    break;
  }

  case EXPR_COMPOUND:
  {
    // Build compound literal by assigning each field individually
    String8 type_name = cdecl_name(g->arena, expr->type);
    
    // First, zero-initialize the destination
    gen_pushlnf(g, "%.*s = (%.*s){0};", str8_varg(dest), str8_varg(type_name));
    
    // Then assign each field
    for each_index(i, expr->compound.args.count)
    {
      Compound_Field *field = expr->compound.args.v[i];
      
      // Evaluate the init expression into a temporary
      String8 value_temp = gen_temp(g, field->init->type);
      String8 value_type = cdecl_name(g->arena, field->init->type);
      gen_pushlnf(g, "%.*s %.*s;", str8_varg(value_type), str8_varg(value_temp));
      gen_expr(g, field->init, value_temp);
      
      // Now assign based on field kind
      switch (field->kind)
      {
      case COMPOUND_FIELD_NAME:
        // Struct field: dest.fieldname = value
        gen_pushlnf(g, "%.*s.%.*s = %.*s;", 
          str8_varg(dest), str8_varg(field->name), str8_varg(value_temp));
        break;
        
      case COMPOUND_FIELD_INDEX:
      {
        // Array with designator: dest.data[index] = value
        String8 index_temp = gen_temp(g, field->index->type);
        gen_pushlnf(g, "int %.*s;", str8_varg(index_temp));
        gen_expr(g, field->index, index_temp);
        gen_pushlnf(g, "%.*s.data[%.*s] = %.*s;", 
          str8_varg(dest), str8_varg(index_temp), str8_varg(value_temp));
        break;
      }
        
      case COMPOUND_FIELD_NONE:
        // Positional init - determine if array or struct
        if (expr->type->kind == TYPE_ARRAY)
        {
          // Array: dest.data[i] = value
          gen_pushlnf(g, "%.*s.data[%llu] = %.*s;", 
            str8_varg(dest), i, str8_varg(value_temp));
        }
        else
        {
          // Struct: assign to i-th field (need field name from type)
          Type_Field struct_field = expr->type->aggregate.fields.v[i];
          gen_pushlnf(g, "%.*s.%.*s = %.*s;", 
            str8_varg(dest), str8_varg(struct_field.name), str8_varg(value_temp));
        }
        break;
      }
    }
    break;
  }

  default:
    gen_pushlnf(g, "%.*s = {0}; // TODO: expr kind %d", str8_varg(dest), expr->kind);
    break;
  }
}

// Helper: Check if an expression is simple enough to inline (no temporaries needed)
internal bool
expr_is_simple(Expr *expr)
{
  if (!expr) return false;
  
  switch (expr->kind)
  {
  case EXPR_INTEGER_LITERAL:
  case EXPR_FLOAT_LITERAL:
  case EXPR_STRING_LITERAL:
  case EXPR_IDENT:
    return true;
  default:
    return false;
  }
}

// Helper: Generate simple expression directly (for literals and identifiers)
internal void
gen_expr_simple(Codegen *g, Expr *expr, String8 dest)
{
  switch (expr->kind)
  {
  case EXPR_INTEGER_LITERAL:
    gen_pushf(g, "%lld", expr->literal.integer);
    break;
  case EXPR_FLOAT_LITERAL:
    gen_pushf(g, "%f", expr->literal.floating);
    break;
  case EXPR_STRING_LITERAL:
    gen_pushf(g, "\"%.*s\"", str8_varg(expr->literal.string));
    break;
  case EXPR_IDENT:
    gen_pushf(g, "%.*s", str8_varg(expr->ident));
    break;
  default:
    assert(0 && "Not a simple expression");
    break;
  }
}

internal void
gen_stmt(Codegen *g, Stmt *stmt, Type *ret_type)
{
  /*
  
STMT_BLOCK
STMT_IF
STMT_DO_WHILE
STMT_WHILE
STMT_FOR
STMT_FOR_IN
STMT_SWITCH
STMT_RETURN
STMT_DEFER
STMT_BREAK
STMT_CONTINUE
STMT_EXPR
STMT_DECL
  */

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
      // If initializer is simple (literal or identifier), inline it
      if (expr_is_simple(decl->init_expr))
      {
        gen_pushlnf(g, "%.*s %.*s = ", str8_varg(type_name), str8_varg(var_name));
        gen_expr_simple(g, decl->init_expr, var_name);
        gen_pushf(g, ";");
      }
      else
      {
        // Complex expression - use temporary
        gen_pushlnf(g, "%.*s %.*s;", str8_varg(type_name), str8_varg(var_name));
        
        String8 temp = gen_temp(g, type);
        gen_pushlnf(g, "%.*s %.*s;", str8_varg(type_name), str8_varg(temp));
        gen_expr(g, decl->init_expr, temp);
        
        gen_pushlnf(g, "%.*s = %.*s;", str8_varg(var_name), str8_varg(temp));
      }
    }
    else
    {
      // Zero-initialize
      gen_pushlnf(g, "%.*s %.*s = {0};", str8_varg(type_name), str8_varg(var_name));
    }
    break;
  }

  case STMT_EXPR:
  {
    // Standalone expression statement
    // Special case: if it's an assignment, just execute it without capturing the result
    if (stmt->expr->kind == EXPR_BINARY && stmt->expr->binary.op.kind == '=')
    {
      Expr *lhs = stmt->expr->binary.left;
      Expr *rhs = stmt->expr->binary.right;
      
      if (expr_is_simple(rhs) && lhs->kind == EXPR_IDENT)
      {
        // Simple assignment: a = 5
        gen_pushlnf(g, "%.*s = ", str8_varg(lhs->ident));
        gen_expr_simple(g, rhs, lhs->ident);
        gen_pushf(g, ";");
      }
      else
      {
        // Complex assignment - use temporary for rhs
        String8 rhs_temp = gen_temp(g, rhs->type);
        String8 type_name = cdecl_name(g->arena, rhs->type);
        
        gen_pushlnf(g, "%.*s %.*s;", str8_varg(type_name), str8_varg(rhs_temp));
        gen_expr(g, rhs, rhs_temp);
        
        if (lhs->kind == EXPR_IDENT)
        {
          gen_pushlnf(g, "%.*s = %.*s;", str8_varg(lhs->ident), str8_varg(rhs_temp));
        }
        else
        {
          // TODO: Handle complex lvalue like arr[i], v.field, *ptr
          gen_pushlnf(g, "/* TODO: complex lvalue assignment */");
        }
      }
    }
    else
    {
      // Other expressions - need to evaluate (e.g., function calls for side effects)
      String8 temp = gen_temp(g, stmt->expr->type);
      String8 type_name = cdecl_name(g->arena, stmt->expr->type);
      gen_pushlnf(g, "%.*s %.*s;", str8_varg(type_name), str8_varg(temp));
      gen_expr(g, stmt->expr, temp);
    }
    break;
  }
  
  case STMT_RETURN:
  {
    if (stmt->return_expr)
    {
      String8 temp = gen_temp(g, ret_type);
      String8 type_name = cdecl_name(g->arena, ret_type);
      gen_pushlnf(g, "%.*s %.*s;", str8_varg(type_name), str8_varg(temp));
      gen_expr(g, stmt->return_expr, temp);
      gen_pushlnf(g, "return %.*s;", str8_varg(temp));
    }
    else
    {
      gen_pushlnf(g, "return;");
    }
    break;
  }
  
  case STMT_IF:
  {
    // Evaluate condition into temporary
    String8 cond_temp = gen_temp(g, type_int);
    gen_pushlnf(g, "bool %.*s;", str8_varg(cond_temp));
    gen_expr(g, stmt->if0.cond, cond_temp);
    
    gen_pushlnf(g, "if (%.*s) {", str8_varg(cond_temp));
    g->indent++;
    gen_stmt_block(g, stmt->if0.then_block->block, ret_type);
    g->indent--;
    
    if (stmt->if0.else_stmt)
    {
      gen_pushlnf(g, "} else {");
      g->indent++;
      gen_stmt(g, stmt->if0.else_stmt, ret_type);
      g->indent--;
    }
    
    gen_pushlnf(g, "}");
    break;
  }
  
  case STMT_WHILE:
  {
    String8 cond_temp = gen_temp(g, type_int);
    gen_pushlnf(g, "while (1) {");
    g->indent++;
    
    // Evaluate condition
    gen_pushlnf(g, "bool %.*s;", str8_varg(cond_temp));
    gen_expr(g, stmt->while0.cond, cond_temp);
    gen_pushlnf(g, "if (!%.*s) break;", str8_varg(cond_temp));
    
    // Body
    gen_stmt_block(g, stmt->while0.body->block, ret_type);
    
    g->indent--;
    gen_pushlnf(g, "}");
    break;
  }
  
  case STMT_DO_WHILE:
  {
    String8 cond_temp = gen_temp(g, type_int);
    gen_pushlnf(g, "do {");
    g->indent++;
    
    gen_stmt_block(g, stmt->while0.body->block, ret_type);
    
    g->indent--;
    gen_pushlnf(g, "} while (");
    
    gen_pushlnf(g, "bool %.*s;", str8_varg(cond_temp));
    gen_expr(g, stmt->while0.cond, cond_temp);
    gen_pushf(g, "%.*s);", str8_varg(cond_temp));
    break;
  }
  
  case STMT_FOR:
  {
    // String8 operand_temp = gen_temp(g, stmt->if0.cond->type);
    // String8 type_name = cdecl_name(g->arena, stmt->if0.cond->type);
    
    // gen_pushlnf(g, "%.*s %.*s;", str8_varg(type_name), str8_varg(operand_temp));
    // gen_expr(g, expr->unary.right, operand_temp);

    gen_pushlnf(g, "// TODO: for loop");
    break;
  }
  
  case STMT_FOR_IN:
  {
    // TODO: Implement for-in loop properly
    gen_pushlnf(g, "// TODO: for loop");
    break;
  }
  
  case STMT_SWITCH:
  {
    // TODO: Implement switch statement
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
