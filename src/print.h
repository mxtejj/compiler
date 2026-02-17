#pragma once

#include "base.h"
#include "arena.h"
#include "strings.h"
#include "parser.h"

function void print_ln  (Arena *arena, String8List *list, int *indent);
function void print_decl(Arena *arena, String8List *list, int *indent, Decl *d);
function void print_expr(Arena *arena, String8List *list, int *indent, Expr *e);
function void print_type(Arena *arena, String8List *list, int *indent, TypeSpec *t);
function void print_stmt(Arena *arena, String8List *list, int *indent, Stmt *s);
