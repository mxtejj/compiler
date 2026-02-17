#include "base.h"
#include "arena.h"
#include "lexer.h"
#include "strings.h"
#include "parser.h"
#include <stdarg.h>

typedef struct CDeclBuilder CDeclBuilder;
typedef struct Codegen Codegen;

struct CDeclBuilder {
  String8List type;   // "int ", "float ", ...
  String8List prefix; // "*", "("
  String8List suffix; // ")", "[3]"
};

struct Codegen {
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
gen_pushf(Codegen *g, char *fmt, ...) {
  va_list args;
  va_start(args, fmt);
  String8 result = str8fv(g->arena, fmt, args);
  va_end(args);
  str8_list_push(g->arena, &g->list, result);
}

internal void
gen_pushlnf(Codegen *g, char *fmt, ...) {
  str8_list_pushf(g->arena, &g->list, "\n%.*s", 2*g->indent, "                                                  ");
  va_list args;
  va_start(args, fmt);
  String8 result = str8fv(g->arena, fmt, args);
  va_end(args);
  str8_list_push(g->arena, &g->list, result);
}

internal void
gen_preamble(Codegen *g) {
  // TODO: inline this ??
  gen_pushf(g, "#include \"preamble.h\"\n\n");
}

internal String8 cdecl_make(Arena *arena, Type *type, String8 ident);
internal String8 gen_array_type_name(Arena *arena, Type *base, u64 length);

internal void gen_stmt(Codegen *g, Stmt *stmt, Type *ret_type);
internal void gen_stmt_block(Codegen *g, StmtArray block, Type *ret_type);

internal String8
cdecl_name(Arena *arena, Type *type) {
  switch (type->kind) {
  case TypeKind_Void:   return str8_lit("void");
  case TypeKind_I8:     return str8_lit("i8");
  case TypeKind_I16:    return str8_lit("i16");
  case TypeKind_I32:    return str8_lit("i32");
  case TypeKind_I64:    return str8_lit("i64");
  case TypeKind_U8:     return str8_lit("u8");
  case TypeKind_U16:    return str8_lit("u16");
  case TypeKind_U32:    return str8_lit("u32");
  case TypeKind_U64:    return str8_lit("u64");
  case TypeKind_Int:    return str8_lit("i64"); // TODO: use isize (platform-sized   signed integer)
  case TypeKind_Uint:   return str8_lit("u64"); // TODO: use usize (platform-sized unsigned integer)
  case TypeKind_F32:    return str8_lit("f32");
  case TypeKind_F64:    return str8_lit("f64");
  case TypeKind_String: return str8_lit("string");

  case TypeKind_Array: return gen_array_type_name(arena, type->array.base, type->array.length);

  case TypeKind_Struct:
  case TypeKind_Union:
    return type->sym->name;

  case TypeKind_Ptr: {
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
cdecl_from_type(Arena *arena, CDeclBuilder *b, Type *type) {
  Temp scratch = arena_scratch_get(&arena, 1);

  switch (type->kind) {
  case TypeKind_Void:
  case TypeKind_I8:
  case TypeKind_I16:
  case TypeKind_I32:
  case TypeKind_I64:
  case TypeKind_U8:
  case TypeKind_U16:
  case TypeKind_U32:
  case TypeKind_U64:
  case TypeKind_Int:
  case TypeKind_Uint:
  case TypeKind_F32:
  case TypeKind_F64:
  case TypeKind_String:
  case TypeKind_Struct:
  case TypeKind_Union:
    str8_list_push_frontf(arena, &b->type, "%.*s ", str8_varg(cdecl_name(scratch.arena, type)));
    break;
  case TypeKind_Ptr:
    cdecl_from_type(arena, b, type->ptr.base);

    if (type->ptr.base->kind == TypeKind_Array || type->ptr.base->kind == TypeKind_Proc) {
      str8_list_push_frontf(arena, &b->prefix, "*(");
      str8_list_pushf(arena, &b->suffix, ")");
    } else {
      str8_list_push_frontf(arena, &b->prefix, "*");
    }
    break;
  case TypeKind_Array: {
    // Use struct wrapper to prevent array decay
    String8 struct_name = gen_array_type_name(arena, type->array.base, type->array.length);
    str8_list_push_frontf(arena, &b->type, "%.*s ", str8_varg(struct_name));
    break;
  }
  case TypeKind_Proc:
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

    if (type->proc.params.count == 0) {
      str8_list_pushf(arena, &b->suffix, "void");
    } else {
      for each_index(i, type->proc.params.count) {
        if (i != 0)
        {
          str8_list_pushf(arena, &b->suffix, ", ");
        }

        CDeclBuilder param_builder = {0};
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
cdecl_make(Arena *arena, Type *type, String8 ident) {
  CDeclBuilder b = {0};
  cdecl_from_type(arena, &b, type);

  String8List out = {0};
  str8_list_concat(&out, &b.type);
  str8_list_concat(&out, &b.prefix);
  str8_list_push(arena, &out, ident);
  str8_list_concat(&out, &b.suffix);

  return str8_list_join(arena, &out, NULL);
}

internal String8
gen_array_type_name(Arena *arena, Type *base, u64 length) {
  // Recursively build array type name
  Temp scratch = arena_scratch_get(&arena, 1);

  String8 base_name;
  if (base->kind == TypeKind_Array) {
    base_name = gen_array_type_name(arena, base->array.base, base->array.length);
  } else {
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
gen_array_typedefs(Codegen *g) {
  gen_pushf(g, "//- Array Type Definitions\n");
  
  // Iterate through all cached array types from resolver
  for each_node(it, CachedArrayType, cached_array_types.first) {
    Type *array_type = &it->v;
    String8 struct_name = gen_array_type_name(g->arena, array_type->array.base, array_type->array.length);
    String8 elem_type_name;
    
    // Get element type name (might also be an array struct)
    if (array_type->array.base->kind == TypeKind_Array) {
      elem_type_name = gen_array_type_name(
        g->arena, 
        array_type->array.base->array.base, 
        array_type->array.base->array.length
      );
    } else {
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
gen_forward_decls(Codegen *g) {
  Temp scratch = arena_scratch_get(&g->arena, 1);

  gen_pushf(g, "//- Forward Declarations\n");

  for each_node(it, SymNode, global_syms.first) {
    Sym *sym = it->v;
    Decl *decl = sym->decl;

    if (!decl)                        continue;
    if (decl->kind != DeclKind_Const) continue;
    if (!decl->init_type)             continue;
    if (decl->is_foreign)             continue;

    switch (decl->init_type->kind) {
    case TypeSpecKind_Struct:
      gen_pushlnf(g, "typedef struct %.*s %.*s;", str8_varg(sym->name), str8_varg(sym->name));
      break;
    case TypeSpecKind_Union:
      gen_pushlnf(g, "typedef union %.*s %.*s;", str8_varg(sym->name), str8_varg(sym->name));
      break;
    case TypeSpecKind_Enum:
      gen_pushlnf(g, "typedef enum %.*s %.*s;", str8_varg(sym->name), str8_varg(sym->name));
      break;
    case TypeSpecKind_Proc:
      gen_pushlnf(g, "%.*s;", str8_varg(cdecl_make(scratch.arena, sym->type, sym->name)));
      break;
    }
  }

  gen_pushf(g, "\n");
  arena_scratch_release(scratch);
}

internal void
gen_aggregate(Codegen *g, Sym *sym) {
  assert(sym->type->kind == TypeKind_Struct || sym->type->kind == TypeKind_Union);
  Decl *decl = sym->decl;
  Type *type = sym->type;

  if (decl->is_foreign) {
    return;
  }

  gen_pushf(g, "%s %.*s {", 
    (sym->type->kind == TypeKind_Struct ? "struct" : "union"), 
    str8_varg(decl->name));

  g->indent++;
  for each_index(i, type->aggregate.fields.count) {
    TypeField it = type->aggregate.fields.v[i];
    String8 result = cdecl_make(g->arena, it.type, it.name);
    gen_pushlnf(g, "%.*s;", str8_varg(result));
  }
  g->indent--;

  gen_pushlnf(g, "};\n");
}

internal void
gen_proc(Codegen *g, Sym *sym) {
  assert(sym->type->kind == TypeKind_Proc);
  Decl *decl = sym->decl;
  Type *type = sym->type;

  if (decl->is_foreign) {
    // Don't generate foreign proc bodies.
    return;
  }

  // Generate function signature WITH NAMED PARAMETERS (preserves debug info!)
  gen_pushf(g, "%.*s %.*s(", 
    str8_varg(cdecl_name(g->arena, type->proc.ret)), 
    str8_varg(sym->name));
  
  // Parameters keep their original names for debugging!
  DeclArray params = decl->init_type->proc.params;
  if (params.count == 0) {
    gen_pushf(g, "void");
  } else {
    for each_index(i, params.count) {
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
  if (params.count > 0) {
    g->param_names = push_array(g->arena, String8, params.count);
    g->param_temps = push_array(g->arena, String8, params.count);
    
    for each_index(i, params.count) {
      Decl *param = params.v[i];
      g->param_names[i] = param->name;
      g->param_temps[i] = gen_temp(g, type->proc.params.v[i]);
      
      String8 type_name = cdecl_name(g->arena, type->proc.params.v[i]);
      gen_pushlnf(g, "%.*s %.*s = %.*s;", 
        str8_varg(type_name),
        str8_varg(g->param_temps[i]),
        str8_varg(param->name)
      );
    }
  }
  
  // Generate body - references to parameters will use temps
  if (decl->init_type->proc.body) {
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
gen_sym(Codegen *g, Sym *sym) {
  if (!sym->decl) return;
  Decl *decl = sym->decl;

  switch (decl->kind) {
  case DeclKind_Var: {
    String8 result = cdecl_make(g->arena, sym->type, sym->name);
    gen_pushf(g, "%.*s = {0};\n", str8_varg(result));
    break;
  }
  case DeclKind_Const: {
    // TODO: dont generate !!!:
    // N :: 40            -> int N = {0};
    // Alias :: My_Struct -> My_Struct Alias = {0};
    if (decl->init_type) {
      switch (decl->init_type->kind) {
      case TypeSpecKind_Struct:
      case TypeSpecKind_Union:
        gen_aggregate(g, sym);
        break;
      case TypeSpecKind_Proc:
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
gen_ordered_syms(Codegen *g) {
  gen_pushf(g, "//- Ordered Declarations\n");
  
  for each_node(it, SymNode, ordered_global_syms.first) {
    Sym *sym = it->v;
    gen_sym(g, sym);
  }
}

internal String8
gen_all(Arena *arena) {
  Codegen g = {0};
  g.arena = arena;
  
  gen_preamble(&g);
  gen_array_typedefs(&g);
  gen_forward_decls(&g);
  gen_ordered_syms(&g);
  
  return str8_list_join(arena, &g.list, NULL);
}

//=============================================================================
//- Expression Generation (inline with temps only for function args)
//=============================================================================

internal String8
gen_temp(Codegen *g, Type *type) {
  // Generate unique temporary variable name
  return str8f(g->arena, "__t%d", g->temp_counter++);
}

internal String8 gen_expr(Codegen *g, Expr *expr);

internal String8
gen_binary_op_name(Codegen *g, TokenKind op, Type *type) {
  char *base_name = NULL;
  switch (op) {
  case TokenKind_Plus:      base_name = "add"; break;
  case TokenKind_Minus:     base_name = "sub"; break;
  case TokenKind_Star:      base_name = "mul"; break;
  case TokenKind_Slash:     base_name = "div"; break;
  case TokenKind_Percent:   base_name = "mod"; break;
  case TokenKind_LShift:    base_name = "shl"; break;
  case TokenKind_RShift:    base_name = "shr"; break;
  case TokenKind_Ampersand: base_name = "and"; break;
  case TokenKind_Pipe:      base_name = "or";  break;
  case TokenKind_Caret:     base_name = "xor"; break;
  case TokenKind_CmpEq:     base_name = "eq";  break;
  case TokenKind_CmpNeq:    base_name = "neq"; break;
  case TokenKind_CmpLt:     base_name = "lt";  break;
  case TokenKind_CmpGt:     base_name = "gt";  break;
  case TokenKind_CmpLtEq:   base_name = "lte"; break;
  case TokenKind_CmpGtEq:   base_name = "gte"; break;
  default: assert(0); return (String8){0};
  }
  
  char *type_suffix = NULL; // use cdecl name function here TODO
  switch (type->kind) {
  case TypeKind_Void:   type_suffix = "void";   break;
  case TypeKind_I8:     type_suffix = "i8";     break;
  case TypeKind_I16:    type_suffix = "i16";    break;
  case TypeKind_I32:    type_suffix = "i32";    break;
  case TypeKind_I64:    type_suffix = "i64";    break;
  case TypeKind_U8:     type_suffix = "u8";     break;
  case TypeKind_U16:    type_suffix = "u16";    break;
  case TypeKind_U32:    type_suffix = "u32";    break;
  case TypeKind_U64:    type_suffix = "u64";    break;
  case TypeKind_Int:    type_suffix = "i64";    break; // TODO: use isize (platform-sized   signed integer)
  case TypeKind_Uint:   type_suffix = "u64";    break; // TODO: use usize (platform-sized unsigned integer)
  case TypeKind_F32:    type_suffix = "f32";    break;
  case TypeKind_F64:    type_suffix = "f64";    break;
  case TypeKind_String: type_suffix = "string"; break;
  default: assert(0); break;
  }
  
  return str8f(g->arena, "%s_%s", base_name, type_suffix);
}

// Generate expression as a C string - only uses temps for function arguments
internal String8
gen_expr(Codegen *g, Expr *expr) {
  switch (expr->kind) {
  case ExprKind_IntegerLiteral:
    return str8f(g->arena, "%lld", expr->literal.integer);
    
  case ExprKind_FloatLiteral:
    return str8f(g->arena, "%f", expr->literal.floating);
    
  case ExprKind_StringLiteral:
    return str8f(g->arena, "STR(\"%.*s\")", str8_varg(expr->literal.string));
    
  case ExprKind_Ident: {
    // Check if this identifier is a parameter - if so, use the temp
    for each_index(i, g->param_count) {
      if (str8_equal(expr->ident, g->param_names[i])) {
        return g->param_temps[i];
      }
    }
    // Not a parameter, use the identifier directly
    return expr->ident;
  }
  
  case ExprKind_Unary: {
    String8 operand = gen_expr(g, expr->unary.right);
    
    switch (expr->unary.op.kind) {
    case TokenKind_Minus: {
      // !TODO: handle all types
      String8 op_func = str8f(g->arena, "neg_%s", expr->type->kind == TypeKind_I32 ? "i32" : "f32");
      return str8f(g->arena, "%.*s(%.*s)", str8_varg(op_func), str8_varg(operand));
    }
    case TokenKind_Exclamation:
      return str8f(g->arena, "!%.*s", str8_varg(operand));
    case TokenKind_Tilde: {
      String8 op_func = str8f(g->arena, "not_%s", expr->type->kind == TypeKind_I32 ? "i32" : "i32");
      return str8f(g->arena, "%.*s(%.*s)", str8_varg(op_func), str8_varg(operand));
    }
    case TokenKind_Plus:
      return operand;
    case TokenKind_Deref:
      return str8f(g->arena, "*%.*s", str8_varg(operand));
    case TokenKind_Ampersand:
      return str8f(g->arena, "&%.*s", str8_varg(operand));
    default:
      assert(0);
      return str8_lit("0");
    }
  }
    
  case ExprKind_Binary: {
    if (expr->binary.op.kind == TokenKind_Equal) {
      // Assignment - emit the assignment statement and return lhs
      String8 lhs = gen_expr(g, expr->binary.left);
      String8 rhs = gen_expr(g, expr->binary.right);
      gen_pushlnf(g, "%.*s = %.*s;", str8_varg(lhs), str8_varg(rhs));
      return lhs;
    } else {
      // Regular binary operation
      String8 left = gen_expr(g, expr->binary.left);
      String8 right = gen_expr(g, expr->binary.right);
      String8 op_func = {0};
      bool is_compound_assign = false;
      switch (expr->binary.op.kind) {
      case TokenKind_LogicalAnd: return str8f(g->arena, "%.*s && %.*s", str8_varg(left), str8_varg(right)); break;
      case TokenKind_LogicalOr:  return str8f(g->arena, "%.*s || %.*s", str8_varg(left), str8_varg(right)); break;
      case TokenKind_LShiftAssign: op_func = gen_binary_op_name(g, TokenKind_LShift, expr->type);    is_compound_assign = true; break;
      case TokenKind_RShiftAssign: op_func = gen_binary_op_name(g, TokenKind_RShift, expr->type);    is_compound_assign = true; break;
      case TokenKind_AddAssign:    op_func = gen_binary_op_name(g, TokenKind_Plus, expr->type);      is_compound_assign = true; break;
      case TokenKind_SubAssign:    op_func = gen_binary_op_name(g, TokenKind_Minus, expr->type);     is_compound_assign = true; break;
      case TokenKind_DivAssign:    op_func = gen_binary_op_name(g, TokenKind_Slash, expr->type);     is_compound_assign = true; break;
      case TokenKind_MulAssign:    op_func = gen_binary_op_name(g, TokenKind_Star, expr->type);      is_compound_assign = true; break;
      case TokenKind_AndAssign:    op_func = gen_binary_op_name(g, TokenKind_Ampersand, expr->type); is_compound_assign = true; break;
      case TokenKind_OrAssign:     op_func = gen_binary_op_name(g, TokenKind_Pipe, expr->type);      is_compound_assign = true; break;
      case TokenKind_XorAssign:    op_func = gen_binary_op_name(g, TokenKind_Caret, expr->type);     is_compound_assign = true; break;
      default: op_func = gen_binary_op_name(g, expr->binary.op.kind, expr->type); break;
      }

      if (is_compound_assign) {
        return str8f(g->arena, "%.*s = %.*s(%.*s, %.*s)", str8_varg(left), str8_varg(op_func), str8_varg(left), str8_varg(right));
      }
      return str8f(g->arena, "%.*s(%.*s, %.*s)", str8_varg(op_func), str8_varg(left), str8_varg(right));
    }
  }
  
  case ExprKind_Call: {
    // Just generate the call directly - temporaries handled in function body
    String8 callee_name = expr->call.expr->ident;
    String8List call_parts = {0};
    str8_list_pushf(g->arena, &call_parts, "%.*s(", str8_varg(callee_name));
    
    for each_index(i, expr->call.args.count) {
      if (i > 0) str8_list_push(g->arena, &call_parts, str8_lit(", "));
      Expr *arg = expr->call.args.v[i];
      String8 arg_expr = gen_expr(g, arg);
      str8_list_pushf(g->arena, &call_parts, "%.*s", str8_varg(arg_expr));
    }
    
    str8_list_push(g->arena, &call_parts, str8_lit(")"));
    return str8_list_join(g->arena, &call_parts, NULL);
  }
  
  case ExprKind_Index: {
    // Array indexing: a[i] becomes a.data[i]
    String8 array = gen_expr(g, expr->index.expr);
    String8 index = gen_expr(g, expr->index.index);
    return str8f(g->arena, "%.*s.data[%.*s]", str8_varg(array), str8_varg(index));
  }
  
  case ExprKind_Field: {
    // Struct field access: v.x
    String8 operand = gen_expr(g, expr->field.expr);
    return str8f(g->arena, "%.*s.%.*s", str8_varg(operand), str8_varg(expr->field.name));
  }
  
  case ExprKind_Cast: {
    String8 operand = gen_expr(g, expr->cast.expr);
    String8 target_type = cdecl_name(g->arena, expr->type);
    return str8f(g->arena, "(%.*s)%.*s", str8_varg(target_type), str8_varg(operand));
  }
  
  case ExprKind_Ternary: {
    String8 cond = gen_expr(g, expr->ternary.cond);
    String8 then = gen_expr(g, expr->ternary.then);
    String8 else_ = gen_expr(g, expr->ternary.else_);
    return str8f(g->arena, "%.*s ? %.*s : %.*s", 
      str8_varg(cond), str8_varg(then), str8_varg(else_));
  }

  case ExprKind_Compound: {
    // Compound literals need to be assigned field-by-field
    // Create a temp, assign fields, return temp name
    String8 temp = gen_temp(g, expr->type);
    String8 type_name = cdecl_name(g->arena, expr->type);
    
    gen_pushlnf(g, "%.*s %.*s = {0};", str8_varg(type_name), str8_varg(temp));
    
    for each_index(i, expr->compound.args.count) {
      CompoundField *field = expr->compound.args.v[i];
      String8 value = gen_expr(g, field->init);
      
      switch (field->kind) {
      case CompoundFieldKind_Name:
        gen_pushlnf(g, "%.*s.%.*s = %.*s;", 
          str8_varg(temp), str8_varg(field->name), str8_varg(value));
        break;
        
      case CompoundFieldKind_Index: {
        String8 index = gen_expr(g, field->index);
        gen_pushlnf(g, "%.*s.data[%.*s] = %.*s;", 
          str8_varg(temp), str8_varg(index), str8_varg(value));
        break;
      }
        
      case CompoundFieldKind_None:
        if (expr->type->kind == TypeKind_Array)
        {
          gen_pushlnf(g, "%.*s.data[%llu] = %.*s;", 
            str8_varg(temp), i, str8_varg(value));
        }
        else
        {
          TypeField struct_field = expr->type->aggregate.fields.v[i];
          gen_pushlnf(g, "%.*s.%.*s = %.*s;", 
            str8_varg(temp), str8_varg(struct_field.name), str8_varg(value));
        }
        break;
      }
    }
    
    return temp;
  }

  case ExprKind_SizeOf: {
    Type *target_type = expr->size_of.is_expr ? expr->size_of.expr->type : expr->type;
    return str8f(g->arena, "/*size_of*/%d", target_type->size);
    // String8 type_name = cdecl_name(g->arena, target_type);
    // return str8f(g->arena, "sizeof(%.*s)", str8_varg(type_name));
  }

  case ExprKind_Group: {
    String8 e = gen_expr(g, expr->group.expr);
    return str8f(g->arena, "(%.*s)", str8_varg(e));
  }

  case ExprKind_CharLiteral: {
    return str8f(g->arena, "/*char*/%d", expr->literal.character);
  }

  default:
    return str8_lit("/* TODO: expr */");
  }
}

internal void
gen_stmt(Codegen *g, Stmt *stmt, Type *ret_type) {
  switch (stmt->kind) {
  case StmtKind_Decl: {
    Decl *decl = stmt->decl;
    Type *type = decl->sym->type;
    
    String8 var_name = decl->name;
    String8 type_name = cdecl_name(g->arena, type);
    
    if (decl->init_expr) {
      String8 init_val = gen_expr(g, decl->init_expr);
      gen_pushlnf(g, "%.*s %.*s = %.*s;", 
        str8_varg(type_name), str8_varg(var_name), str8_varg(init_val));
    } else {
      gen_pushlnf(g, "%.*s %.*s = {0};", str8_varg(type_name), str8_varg(var_name));
    }
    break;
  }

  case StmtKind_Expr: {
    // Just evaluate the expression (might have side effects like assignments or calls)
    String8 expr_val = gen_expr(g, stmt->expr);
    // For pure expression statements (not assignments which gen_expr handles),
    // we would emit it, but assignments already emit themselves
    if (stmt->expr->kind != ExprKind_Binary || stmt->expr->binary.op.kind != TokenKind_Equal) {
      // For non-assignment expressions (like function calls), emit as statement
      gen_pushlnf(g, "%.*s;", str8_varg(expr_val));
    }
    break;
  }
  
  case StmtKind_Return: {
    if (stmt->return_expr) {
      String8 ret_val = gen_expr(g, stmt->return_expr);
      gen_pushlnf(g, "return %.*s;", str8_varg(ret_val));
    } else {
      gen_pushlnf(g, "return;");
    }
    break;
  }
  
  case StmtKind_If: {
    String8 cond = gen_expr(g, stmt->if0.cond);
    gen_pushlnf(g, "if (%.*s) {", str8_varg(cond));
    g->indent++;
    gen_stmt_block(g, stmt->if0.then_block->block, ret_type);
    g->indent--;

    Stmt *else_part = stmt->if0.else_stmt;
    while (else_part != NULL) {
      if (else_part->kind == StmtKind_If) {
        // else if - generate on same line as closing brace
        String8 else_cond = gen_expr(g, else_part->if0.cond);
        gen_pushlnf(g, "} else if (%.*s) {", str8_varg(else_cond));
        g->indent++;
        gen_stmt_block(g, else_part->if0.then_block->block, ret_type);
        g->indent--;
        else_part = else_part->if0.else_stmt;
      }
      else {
        // final else block
        assert(else_part->kind == StmtKind_Block);
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
  
  case StmtKind_While: {
    String8 cond = gen_expr(g, stmt->while0.cond);
    gen_pushlnf(g, "while (%.*s) {", str8_varg(cond));
    g->indent++;
    // gen_pushlnf(g, "if (!%.*s) break;", str8_varg(cond));
    gen_stmt_block(g, stmt->while0.body->block, ret_type);
    g->indent--;
    gen_pushlnf(g, "}");
    break;
  }
  
  case StmtKind_DoWhile: {
    gen_pushlnf(g, "do {");
    g->indent++;
    gen_stmt_block(g, stmt->while0.body->block, ret_type);
    g->indent--;
    gen_pushlnf(g, "}");
    String8 cond = gen_expr(g, stmt->while0.cond);
    gen_pushf(g, " while (%.*s);", str8_varg(cond));
    break;
  }
  
  case StmtKind_For: {
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

  case StmtKind_ForIn: {
    String8 elem_type_name = cdecl_name(g->arena, stmt->for_in.item->type);
    String8 elem_name = stmt->for_in.item->ident;

    // for now, handle only arrays
    assert(stmt->for_in.iter->type->kind == TypeKind_Array);

    // TODO: when we have: `for name, index in names`, support `, index`
    // TODO: support not just int for index ^^ (for example we can have a array whose index is an enum)

    // TODO: support this:
    // for n in [6]int.{1, 2, 3, 4, 5, 6} {
    //   print_int(n)
    //   print("\n")
    // }

    // for (int i = 0; i < 100; i += 1)
    gen_pushlnf(g, "for (i64 __i = 0; __i < %d; __i += 1) {", stmt->for_in.iter->type->array.length);
    g->indent++;
    gen_pushlnf(g, "%.*s %.*s = %.*s.data[__i];", str8_varg(elem_type_name), str8_varg(elem_name), str8_varg(stmt->for_in.iter->ident));
    gen_stmt_block(g, stmt->for_in.body->block, ret_type);
    g->indent--;
    gen_pushlnf(g, "}");
    break;
  }
  
  case StmtKind_Switch: {
    gen_pushlnf(g, "// TODO: switch statement");
    break;
  }
  
  case StmtKind_Block: {
    gen_pushlnf(g, "{");
    g->indent++;
    gen_stmt_block(g, stmt->block, ret_type);
    g->indent--;
    gen_pushlnf(g, "}");
    break;
  }
  
  case StmtKind_Break:
    gen_pushlnf(g, "break;");
    break;
    
  case StmtKind_Continue:
    gen_pushlnf(g, "continue;");
    break;
  
  default:
    gen_pushlnf(g, "// TODO: stmt kind %d", stmt->kind);
    break;
  }
}

internal void
gen_stmt_block(Codegen *g, StmtArray block, Type *ret_type) {
  for each_index(i, block.count) {
    gen_stmt(g, block.v[i], ret_type);
  }
}
