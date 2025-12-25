#include "lexer.h"
#include <ctype.h>
#include <stdio.h>

// TODO:
// Integer literal
// Floating literal
// Character literal

String
str_from_token_kind(Token_Kind kind)
{
  local_persist u8 buf[64];
  u64 n = 0;

  switch (kind)
  {
  case TOKEN_EOF: return str_comp("TOKEN_EOF");

  #define X(name) case TOKEN_##name: return str_comp(stringify(glue(TOKEN_, name)));
    TOKEN_LIST(X)
  #undef X

  default:
    if (kind < 128 && isprint(kind))
    {
      n = snprintf(buf, sizeof(buf), "%c", kind);
    }
    else
    {
      n = snprintf(buf, sizeof(buf), "<ASCII %c>", kind);
    }
  }
  return (String){ .data = buf, .count = n };
}

internal Lexer
lexer_init(String source)
{
  Lexer l = {0};
  l.source = source;
  return l;
}

internal char
lexer_peek(Lexer *l)
{
  assert(lexer_can_peek(l));
  return l->source.data[l->cursor];
}

internal void
lexer_eat(Lexer *l)
{
  assert(lexer_can_peek(l));

  char x = lexer_peek(l);
  l->cursor += 1;

  if (x == '\r' && (lexer_can_peek(l) && lexer_peek(l) == '\n'))
  {
    // Consume \r
    l->cursor += 1;
    x = '\n';
  }
  if (x == '\n')
  {
    l->line += 1;
    l->bol = l->cursor;
  }
}

internal bool
lexer_can_peek(Lexer *l)
{
  return l->cursor < l->source.count;
}

internal void
lexer_eat_whitespace(Lexer *l)
{
  while (lexer_can_peek(l) && isspace(lexer_peek(l)))
  {
    lexer_eat(l);
  }
}

internal bool
is_symbol(Lexer *l)
{
  assert(lexer_can_peek(l));
  char c = lexer_peek(l);
  return c == '_' || isalnum(c);
}

internal Token
lexer_make_token(Lexer *l, Token_Kind kind)
{
  return (Token)
  {
    .kind = kind,
    .lexeme = str_view(l->source, l->start, l->cursor),
  };
}

internal Token
lexer_next(Lexer *l)
{
  lexer_eat_whitespace(l);
  l->start = l->cursor;

  Token t = lexer_make_token(l, TOKEN_EOF);
  if (!lexer_can_peek(l))
    return t;

  switch (lexer_peek(l))
  {
  case 'A': case 'B': case 'C': case 'D': case 'E': case 'F': case 'G': case 'H': case 'I': case 'J':
  case 'K': case 'L': case 'M': case 'N': case 'O': case 'P': case 'Q': case 'R': case 'S': case 'T':
  case 'U': case 'V': case 'W': case 'X': case 'Y': case 'Z':
  case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g': case 'h': case 'i': case 'j':
  case 'k': case 'l': case 'm': case 'n': case 'o': case 'p': case 'q': case 'r': case 's': case 't':
  case 'u': case 'v': case 'w': case 'x': case 'y': case 'z':
  case '_':
    while (is_symbol(l))
    {
      lexer_eat(l);
    }
    t = lexer_make_token(l, TOKEN_IDENT);
    break;

  case '"':
    lexer_eat(l); // eat first quote
    while (lexer_can_peek(l) && lexer_peek(l) != '"')
    {
      char c = lexer_peek(l);
      if (c == '\n')
      {
        // TODO: Check \r
        assert(!"String literal cannot contain newline");
      }
      else if (c == '\\')
      {
        lexer_eat(l);
        lexer_eat(l);
      }
      lexer_eat(l);
    }
    assert(lexer_peek(l) == '"');
    lexer_eat(l);

    t = lexer_make_token(l, TOKEN_STRING);
    break;

  default:
    // ASCII Code token
    char c = lexer_peek(l);
    lexer_eat(l);
    t = lexer_make_token(l, c);
    break;
  }

  return t;
}
