#pragma once

#include "arena.h"
#include "lexer.h"
#include "ast.h"

typedef struct Parser Parser;
struct Parser
{
  Arena *arena;
  Lexer *lexer;
  Token curr;
  Token prev;
};

Parser parser_init(Lexer *l);
AST *ast_alloc(Parser *p);
