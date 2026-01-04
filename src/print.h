#pragma once

#include "base.h"
#include "arena.h"
#include "strings.h"
#include "parser.h"

internal void print_ln(Arena *arena, String8List *list, int *indent);
internal void print_decl(Arena *arena, String8List *list, int *indent, Decl *d);
internal void print_expr(Arena *arena, String8List *list, int *indent, Expr *e);
internal void print_type(Arena *arena, String8List *list, int *indent, Type_Spec *t);
internal void print_stmt(Arena *arena, String8List *list, int *indent, Stmt *s);
