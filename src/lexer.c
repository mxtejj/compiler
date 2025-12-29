#include "lexer.h"
#include <ctype.h>
#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <math.h>
#include "globals.h"

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

internal u64
lexer_row(Lexer *l)
{
  return l->line;
}

internal u64
lexer_col(Lexer *l)
{
  return l->cursor - l->bol;
}

internal void
lexer_syntax_error(Lexer *l, const char *fmt, ...)
{
  va_list args;
  va_start(args, fmt);

  // printf(WHT "%.*s(%llu:%llu) ", strf(l->file_path), lexer_row(l), lexer_col(l));
  printf(CLR_WHT "(%llu:%llu) ", lexer_row(l), lexer_col(l));
  printf(CLR_RED "Syntax Error: " CLR_RESET);
  vprintf(fmt, args);
  printf("\n");

  u64 line_start = l->bol;
  u64 line_end = line_start;
  while (line_end < l->source.count && l->source.data[line_end] != '\n' && l->source.data[line_end] != '\r')
    line_end++;

  printf(CLR_CYN "%.*s\n", (int)(line_end - line_start), &l->source.data[line_start]);
  u64 col = lexer_col(l);
  printf("%*s" CLR_GRN "^\n" CLR_RESET, (int)col, "");

  va_end(args);
}

Lexer
lexer_init(String source)
{
  Lexer l = {0};
  l.arena  = arena_alloc(GB(1), MB(1), 0);
  l.source = source;
  return l;
}

void
lexer_fini(Lexer *l)
{
  *l = (Lexer){0};
  arena_delete(l->arena);
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
lexer_parse_string(Lexer *l)
{
  assert(lexer_peek(l) == '"');

  local_persist char escape_to_char[256] =
  {
    ['n'] = '\n',
    ['r'] = '\r',
    ['t'] = '\t',
    ['v'] = '\v',
    ['b'] = '\b',
    ['a'] = '\a',
    ['0'] = '\0',
  };

  Arena *string_arena = arena_alloc(GB(1), MB(8), 0);
  char *string_buf = push_array_align(string_arena, char, 0, 1);
  u64 string_len = 0;

  lexer_eat(l);
  while (lexer_can_peek(l) && lexer_peek(l) != '"')
  {
    char c = lexer_peek(l);
    if (c == '\n')
    {
      // TODO: Check \r
      lexer_syntax_error(l, "String literal cannot contain newline");
    }
    else if (c == '\\')
    {
      lexer_eat(l);
      if (lexer_peek(l) == '"')
      {
        // append "
        char *a = push_array_align(string_arena, char, 1, 1);
        *a = lexer_peek(l);
        string_len++;

        lexer_eat(l);
        continue;
      }
      c = escape_to_char[lexer_peek(l)];
      if (c == 0 && lexer_peek(l) != '0')
      {
        lexer_syntax_error(l, "Invalid string literal escape '\\%c'", lexer_peek(l));
      }
    }
    // append c
    char *a = push_array_align(string_arena, char, 1, 1);
    *a = c;
    string_len++;

    lexer_eat(l);
  }

  if (lexer_can_peek(l))
  {
    assert(lexer_peek(l) == '"');
    lexer_eat(l);
  }
  else
  {
    lexer_syntax_error(l, "Unexpected end of file within string literal");
  }

  Token t = lexer_make_token(l, TOKEN_STRING_LITERAL);
  t.value.string = str_init(g_arena, string_len);
  mem_copy(t.value.string.data, string_buf, string_len);

  arena_delete(string_arena);

  return t;
}

internal Token
lexer_parse_float(Lexer *l)
{
  /*

  (
    digits '.' digits?
    | '.' digits       // <== TODO
    | digits
  )
  (
    [eE] [+-]? digits
  )?

  */

  while (lexer_can_peek(l) && isdigit(lexer_peek(l)))
  {
    lexer_eat(l);
  }

  if (lexer_can_peek(l) && (lexer_peek(l)) == '.')
  {
    lexer_eat(l); // .
    while (lexer_can_peek(l) && isdigit(lexer_peek(l)))
    {
      lexer_eat(l);
    }
  }

  if (lexer_can_peek(l) && tolower(lexer_peek(l)) == 'e')
  {
    lexer_eat(l);
    if (lexer_can_peek(l) && (lexer_peek(l) == '+' || lexer_peek(l) == '-'))
    {
      lexer_eat(l);
    }
    if (!isdigit(lexer_peek(l)))
    {
      lexer_syntax_error(l, "Expected digit after float literal exponent, got '%c'", lexer_peek(l));
      // assert(0);
    }
    while (lexer_can_peek(l) && isdigit(lexer_peek(l)))
    {
      lexer_eat(l);
    }
  }

  Token t = lexer_make_token(l, TOKEN_FLOAT_LITERAL);

  Arena_Temp scratch = arena_scratch_get(0, 0);

  u8 *buf = push_array(scratch.arena, u8, t.lexeme.count); // +1 ?
  mem_copy(buf, t.lexeme.data, t.lexeme.count);
  buf[t.lexeme.count] = '\0';

  f64 value = strtod(buf, NULL);
  if (value == HUGE_VAL || value == -HUGE_VAL)
  {
    lexer_syntax_error(l, "Float literal overflow");
  }

  t.value.floating = value;

  arena_scratch_release(scratch);

  return t;
}

internal Token
lexer_parse_integer(Lexer *l)
{
  u64 value = 0;
  u64 base = 10;

  if (lexer_can_peek(l) && lexer_peek(l) == '0')
  {
    lexer_eat(l);
    if (lexer_can_peek(l) && tolower(lexer_peek(l)) == 'x')
    {
      // HEX
      lexer_eat(l);
      base = 16;
    }
    else if (lexer_can_peek(l) && tolower(lexer_peek(l)) == 'b')
    {
      // BINARY
      lexer_eat(l);
      base = 2;
    }
    else if (lexer_can_peek(l) && tolower(lexer_peek(l)) == 'o')
    {
      // OCTAL
      lexer_eat(l);
      base = 8;
    }
  }

  local_persist u8 char_to_digit[256] =
  {
    ['0'] = 0,
    ['1'] = 1,
    ['2'] = 2,
    ['3'] = 3,
    ['4'] = 4,
    ['5'] = 5,
    ['6'] = 6,
    ['7'] = 7,
    ['8'] = 8,
    ['9'] = 9,
    ['a'] = 10, ['A'] = 10,
    ['b'] = 11, ['B'] = 11,
    ['c'] = 12, ['C'] = 12,
    ['d'] = 13, ['D'] = 13,
    ['e'] = 14, ['E'] = 14,
    ['f'] = 15, ['F'] = 15,
  };

  // for (;;)
  while (lexer_can_peek(l))
  {
    u64 digit = char_to_digit[lexer_peek(l)];
    if (digit == 0 && lexer_peek(l) != '0')
    {
      break;
    }
    if (digit >= base)
    {
      lexer_syntax_error(l, "Digit '%c' out of range for base %llu", lexer_peek(l), base);
      break;
    }
    if (value > (UINT64_MAX - digit) / base)
    {
      lexer_syntax_error(l, "Integer literal overflow");
      while (lexer_can_peek(l) && isdigit(lexer_peek(l)))
      {
        lexer_eat(l);
      }
      value = 0;
    }
    else
    {
      lexer_eat(l);
      value *= base;
      value += digit;
    }
  }

  Token t = lexer_make_token(l, TOKEN_INTEGER_LITERAL);
  t.value.integer = value;

  return t;
}

internal Token
lexer_parse_ident_or_keyword(Lexer *l)
{
  typedef struct Keyword Keyword;
  struct Keyword
  {
    Token_Kind kind;
    String     value;
  };

  local_persist Keyword keywords[] =
  {
    { .kind = TOKEN_NIL,       .value = S("nil") },

    // integer types
    { .kind = TOKEN_S8,        .value = S("s8") },
    { .kind = TOKEN_S16,       .value = S("s16") },
    { .kind = TOKEN_S32,       .value = S("s32") },
    { .kind = TOKEN_S64,       .value = S("s64") },
    { .kind = TOKEN_U8,        .value = S("u8") },
    { .kind = TOKEN_U16,       .value = S("u16") },
    { .kind = TOKEN_U32,       .value = S("u32") },
    { .kind = TOKEN_U64,       .value = S("u64") },
    { .kind = TOKEN_UINTPTR,   .value = S("uintptr") },
    { .kind = TOKEN_INT,       .value = S("int") },
    { .kind = TOKEN_UINT,      .value = S("uint") },

    // floating-point types
    { .kind = TOKEN_F32,       .value = S("f32") },
    { .kind = TOKEN_F64,       .value = S("f64") },

    // boolean
    { .kind = TOKEN_BOOL,      .value = S("bool") },
    { .kind = TOKEN_TRUE,      .value = S("true") },
    { .kind = TOKEN_FALSE,     .value = S("false") },

    // control flow statements
    { .kind = TOKEN_IF,        .value = S("if") },
    { .kind = TOKEN_ELSE,      .value = S("else") },
    { .kind = TOKEN_FOR,       .value = S("for") },
    { .kind = TOKEN_DO,        .value = S("do") },
    { .kind = TOKEN_WHILE,     .value = S("while") },
    { .kind = TOKEN_SWITCH,    .value = S("switch") },
    { .kind = TOKEN_CASE,      .value = S("case") },
    { .kind = TOKEN_BREAK,     .value = S("break") },
    { .kind = TOKEN_CONTINUE,  .value = S("continue") },
    { .kind = TOKEN_RETURN,    .value = S("return") },

    { .kind = TOKEN_STRUCT,    .value = S("struct") },
    { .kind = TOKEN_UNION,     .value = S("union") },
    { .kind = TOKEN_ENUM,      .value = S("enum") },
    { .kind = TOKEN_STRING,    .value = S("string") },

    { .kind = TOKEN_PROC,      .value = S("proc") },
    { .kind = TOKEN_VAR,       .value = S("var") },
    { .kind = TOKEN_CONST,     .value = S("const") },

    { .kind = TOKEN_SIZE_OF,   .value = S("size_of") },
    { .kind = TOKEN_CAST,      .value = S("cast") },
    { .kind = TOKEN_TRANSMUTE, .value = S("transmute") },
  };
  static_assert(array_count(keywords) == (TOKEN_KEYWORD_END - TOKEN_KEYWORD_BEGIN - 1));

  while (lexer_can_peek(l) && is_symbol(l))
  {
    lexer_eat(l);
  }

  Token t = lexer_make_token(l, TOKEN_IDENT);

  // TODO: String interning for faster string checks
  for (u32 i = 0; i < array_count(keywords); i++)
  {
    if (str_equal(t.lexeme, keywords[i].value))
    {
      t.kind = keywords[i].kind;
      break;
    }
  }

  return t;
}

Token
lexer_next(Lexer *l)
{
  lexer_eat_whitespace(l);
  l->start = l->cursor;

  if (!lexer_can_peek(l))
    return lexer_make_token(l, TOKEN_EOF);

  Token t = {0};

  switch (lexer_peek(l))
  {
  case 'A': case 'B': case 'C': case 'D': case 'E': case 'F': case 'G': case 'H': case 'I': case 'J':
  case 'K': case 'L': case 'M': case 'N': case 'O': case 'P': case 'Q': case 'R': case 'S': case 'T':
  case 'U': case 'V': case 'W': case 'X': case 'Y': case 'Z':
  case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g': case 'h': case 'i': case 'j':
  case 'k': case 'l': case 'm': case 'n': case 'o': case 'p': case 'q': case 'r': case 's': case 't':
  case 'u': case 'v': case 'w': case 'x': case 'y': case 'z':
  case '_':
    t = lexer_parse_ident_or_keyword(l);
    break;

  case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7': case '8': case '9':
    while (lexer_can_peek(l) && isdigit(lexer_peek(l)))
    {
      lexer_eat(l);
    }

    if (lexer_can_peek(l) &&
       (lexer_peek(l) == '.' || tolower(lexer_peek(l)) == 'e'))
    {
      l->cursor = l->start;
      t = lexer_parse_float(l);
    }
    else
    {
      l->cursor = l->start;
      t = lexer_parse_integer(l);
    }
    break;

  case '"':
    t = lexer_parse_string(l);
    break;

  case '\'':
    assert(!"TODO: Character literal");
    break;

  case '*':
    lexer_eat(l);
    if (lexer_can_peek(l) && lexer_peek(l) == '=')
    {
      lexer_eat(l);
      t = lexer_make_token(l, TOKEN_MUL_ASSIGN);
    }
    else
    {
      t = lexer_make_token(l, '*');
    }
    break;

  case '/':
    lexer_eat(l);
    if (lexer_can_peek(l) && lexer_peek(l) == '=')
    {
      lexer_eat(l);
      t = lexer_make_token(l, TOKEN_DIV_ASSIGN);
    }
    else
    {
      t = lexer_make_token(l, '/');
    }
    break;

  case '-':
    lexer_eat(l);
    // if (lexer_can_peek(l) && lexer_peek(l) == '-')
    // {
    //   lexer_eat(l);
    //   t = lexer_make_token(l, TOKEN_DECREMENT);
    // }
    // else
    if (lexer_can_peek(l) && lexer_peek(l) == '=')
    {
      lexer_eat(l);
      t = lexer_make_token(l, TOKEN_SUB_ASSIGN);
    }
    else if (lexer_can_peek(l) && lexer_peek(l) == '>')
    {
      lexer_eat(l);
      t = lexer_make_token(l, TOKEN_ARROW);
    }
    else
    {
      t = lexer_make_token(l, '-');
    }
    break;

  case '+':
    lexer_eat(l);
    // if (lexer_can_peek(l) && lexer_peek(l) == '+')
    // {
    //   lexer_eat(l);
    //   t = lexer_make_token(l, TOKEN_INCREMENT);
    // }
    // else
    if (lexer_can_peek(l) && lexer_peek(l) == '=')
    {
      lexer_eat(l);
      t = lexer_make_token(l, TOKEN_ADD_ASSIGN);
    }
    else
    {
      t = lexer_make_token(l, '+');
    }
    break;

  case '&':
    lexer_eat(l);
    if (lexer_can_peek(l) && lexer_peek(l) == '&')
    {
      lexer_eat(l);
      t = lexer_make_token(l, TOKEN_LOGICAL_AND);
    }
    else if (lexer_can_peek(l) && lexer_peek(l) == '=')
    {
      lexer_eat(l);
      t = lexer_make_token(l, TOKEN_AND_ASSIGN);
    }
    else
    {
      t = lexer_make_token(l, '&');
    }
    break;

  case '|':
    lexer_eat(l);
    if (lexer_can_peek(l) && lexer_peek(l) == '|')
    {
      lexer_eat(l);
      t = lexer_make_token(l, TOKEN_LOGICAL_OR);
    }
    else if (lexer_can_peek(l) && lexer_peek(l) == '=')
    {
      lexer_eat(l);
      t = lexer_make_token(l, TOKEN_OR_ASSIGN);
    }
    else
    {
      t = lexer_make_token(l, '|');
    }
    break;

  case '^':
    lexer_eat(l);
    if (lexer_can_peek(l) && lexer_peek(l) == '=')
    {
      lexer_eat(l);
      t = lexer_make_token(l, TOKEN_XOR_ASSIGN);
    }
    else
    {
      t = lexer_make_token(l, '^');
    }
    break;

  case '=':
    lexer_eat(l);
    if (lexer_can_peek(l) && lexer_peek(l) == '=')
    {
      lexer_eat(l);
      t = lexer_make_token(l, TOKEN_EQ);
    }
    else
    {
      t = lexer_make_token(l, '=');
    }
    break;

  case '!':
    lexer_eat(l);
    if (lexer_can_peek(l) && lexer_peek(l) == '=')
    {
      lexer_eat(l);
      t = lexer_make_token(l, TOKEN_NEQ);
    }
    else
    {
      t = lexer_make_token(l, '!');
    }
    break;

  case '>':
    lexer_eat(l);
    if (lexer_can_peek(l) && lexer_peek(l) == '=')
    {
      lexer_eat(l);
      t = lexer_make_token(l, TOKEN_GTEQ);
    }
    else if (lexer_can_peek(l) && lexer_peek(l) == '>')
    {
      lexer_eat(l);
      if (lexer_can_peek(l) && lexer_peek(l) == '=')
      {
        lexer_eat(l);
        t = lexer_make_token(l, TOKEN_RSHIFT_ASSIGN);
      }
      else
      {
        t = lexer_make_token(l, TOKEN_RSHIFT);
      }
    }
    else
    {
      t = lexer_make_token(l, '>');
    }
    break;

  case '<':
    lexer_eat(l);
    if (lexer_can_peek(l) && lexer_peek(l) == '=')
    {
      lexer_eat(l);
      t = lexer_make_token(l, TOKEN_LTEQ);
    }
    else if (lexer_can_peek(l) && lexer_peek(l) == '<')
    {
      lexer_eat(l);
      if (lexer_can_peek(l) && lexer_peek(l) == '=')
      {
        lexer_eat(l);
        t = lexer_make_token(l, TOKEN_LSHIFT_ASSIGN);
      }
      else
      {
        t = lexer_make_token(l, TOKEN_LSHIFT);
      }
    }
    else
    {
      t = lexer_make_token(l, '<');
    }
    break;

  default:
    // ASCII Code token
    char c = lexer_peek(l);
    lexer_eat(l);
    // t = lexer_make_token(l, lexer_peek(l));
    t = lexer_make_token(l, c);
    break;
  }

  return t;
}
