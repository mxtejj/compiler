#include "lexer.h"
#include <ctype.h>
#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <math.h>
#include "globals.h"

//
// TODO:
// [ ] handle each token explicitly
// [ ] add comments
//   [ ] single line
//   [ ] multi line
//

internal String8
str_from_token_kind(Arena *arena, Token_Kind kind)
{
  switch (kind)
  {
  case TokenKind_EOF:            return str8_lit("end of file");
  case TokenKind_LParen:         return str8_lit("(");
  case TokenKind_RParen:         return str8_lit(")");
  case TokenKind_LBracket:       return str8_lit("[");
  case TokenKind_RBracket:       return str8_lit("]");
  case TokenKind_LBrace:         return str8_lit("{");
  case TokenKind_RBrace:         return str8_lit("}");
  case TokenKind_Plus:           return str8_lit("+");
  case TokenKind_Minus:          return str8_lit("-");
  case TokenKind_Caret:          return str8_lit("^");
  case TokenKind_Ampersand:      return str8_lit("&");
  case TokenKind_Pipe:           return str8_lit("|");
  case TokenKind_Star:           return str8_lit("*");
  case TokenKind_Slash:          return str8_lit("/");
  case TokenKind_Percent:        return str8_lit("%");
  case TokenKind_Exclamation:    return str8_lit("!");
  case TokenKind_Question:       return str8_lit("?");
  case TokenKind_Equal:          return str8_lit("=");
  case TokenKind_Dot:            return str8_lit(".");
  case TokenKind_Comma:          return str8_lit(",");
  case TokenKind_Semicolon:      return str8_lit(";");
  case TokenKind_Colon:          return str8_lit(":");
  case TokenKind_Tilde:          return str8_lit("~");
  case TokenKind_Ident:          return str8_lit("identifier");
  case TokenKind_Arrow:          return str8_lit("->");
  case TokenKind_Deref:          return str8_lit(".*");
  case TokenKind_CmpGt:          return str8_lit(">");
  case TokenKind_CmpLt:          return str8_lit("<");
  case TokenKind_CmpEq:          return str8_lit("==");
  case TokenKind_CmpNeq:         return str8_lit("!=");
  case TokenKind_CmpGtEq:        return str8_lit(">=");
  case TokenKind_CmpLtEq:        return str8_lit("<=");
  case TokenKind_LogicalOr:      return str8_lit("||");
  case TokenKind_LogicalAnd:     return str8_lit("&&");
  case TokenKind_LShift:         return str8_lit("<<");
  case TokenKind_RShift:         return str8_lit(">>");
  case TokenKind_LShiftAssign:   return str8_lit("<<=");
  case TokenKind_RShiftAssign:   return str8_lit(">>=");
  case TokenKind_AddAssign:      return str8_lit("+=");
  case TokenKind_SubAssign:      return str8_lit("-=");
  case TokenKind_DivAssign:      return str8_lit("/=");
  case TokenKind_MulAssign:      return str8_lit("*=");
  case TokenKind_AndAssign:      return str8_lit("&=");
  case TokenKind_OrAssign:       return str8_lit("|=");
  case TokenKind_XorAssign:      return str8_lit("^=");
  case TokenKind_Increment:      return str8_lit("++");
  case TokenKind_Decrement:      return str8_lit("--");
  case TokenKind_RangeIncl:      return str8_lit("..=");
  case TokenKind_RangeExcl:      return str8_lit("..<");
  case TokenKind_StringLiteral:  return str8_lit("string literal");
  case TokenKind_IntegerLiteral: return str8_lit("integer literal");
  case TokenKind_FloatLiteral:   return str8_lit("float literal");
  case TokenKind_CharLiteral:    return str8_lit("char literal");
  case TokenKind_Nil:            return str8_lit("nil");
  case TokenKind_S8:             return str8_lit("s8");
  case TokenKind_S16:            return str8_lit("s16");
  case TokenKind_S32:            return str8_lit("s32");
  case TokenKind_S64:            return str8_lit("s64");
  case TokenKind_U8:             return str8_lit("u8");
  case TokenKind_U16:            return str8_lit("u16");
  case TokenKind_U32:            return str8_lit("u32");
  case TokenKind_U64:            return str8_lit("u64");
  case TokenKind_Uintptr:        return str8_lit("uintptr");
  case TokenKind_Int:            return str8_lit("int");
  case TokenKind_Uint:           return str8_lit("uint");
  case TokenKind_F32:            return str8_lit("f32");
  case TokenKind_F64:            return str8_lit("f64");
  case TokenKind_Bool:           return str8_lit("bool");
  case TokenKind_String:         return str8_lit("string");
  case TokenKind_True:           return str8_lit("true");
  case TokenKind_False:          return str8_lit("false");
  case TokenKind_If:             return str8_lit("if");
  case TokenKind_Else:           return str8_lit("else");
  case TokenKind_For:            return str8_lit("for");
  case TokenKind_Do:             return str8_lit("do");
  case TokenKind_While:          return str8_lit("while");
  case TokenKind_Switch:         return str8_lit("switch");
  case TokenKind_Case:           return str8_lit("case");
  case TokenKind_Defer:          return str8_lit("defer");
  case TokenKind_Break:          return str8_lit("break");
  case TokenKind_Fallthrough:    return str8_lit("fallthrough");
  case TokenKind_Continue:       return str8_lit("continue");
  case TokenKind_Return:         return str8_lit("return");
  case TokenKind_Struct:         return str8_lit("struct");
  case TokenKind_Union:          return str8_lit("union");
  case TokenKind_Enum:           return str8_lit("enum");
  case TokenKind_Proc:           return str8_lit("proc");
  case TokenKind_SizeOf:         return str8_lit("size_of");
  case TokenKind_Cast:           return str8_lit("cast");
  case TokenKind_Transmute:      return str8_lit("transmute");
  case TokenKind_In:             return str8_lit("in");
  case TokenKind_Then:           return str8_lit("then");
  case TokenKind_Foreign:        return str8_lit("foreign");

  default:
    assert(!"Invalid token kind");
    break;
  }

  assert(!"Unreachable");
  return (String8){0};
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
  u64 col = lexer_col(l) - 1;
  printf("%*s" CLR_GRN "^\n" CLR_RESET, (int)col, "");

  va_end(args);
}

internal Lexer
lexer_init(String8 source)
{
  Lexer l = {0};
  l.arena  = arena_alloc(GB(1), MB(1), 0);
  l.source = source;
  return l;
}

internal void
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

internal bool
is_symbol_char(char c)
{
  return c == '_' || isalnum(c);
}

internal Token
lexer_make_token(Lexer *l, Token_Kind kind)
{
  return (Token)
  {
    .kind = kind,
    .lexeme = str8_substr(l->source, l->start, l->cursor),
  };
}

global read_only char escape_to_char[256] =
{
  ['n'] = '\n',
  ['r'] = '\r',
  ['t'] = '\t',
  ['v'] = '\v',
  ['b'] = '\b',
  ['a'] = '\a',
  ['0'] = '\0',
};

internal Token
lexer_parse_string(Lexer *l)
{
  assert(lexer_peek(l) == '"');

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

  Token t = lexer_make_token(l, TokenKind_StringLiteral);
  // t.value.string = str8_copy(g_arena, str8(string_buf, string_len));

  // @HACK since we cant emit escape codes in C codegen,
  // for now we just take the original literal text with the quotes chopped off
  t.value.string = t.lexeme;
  t.value.string = str8_skip(t.value.string, 1);
  t.value.string = str8_chop(t.value.string, 1);

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

  Token t = lexer_make_token(l, TokenKind_FloatLiteral);

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

  Token t = lexer_make_token(l, TokenKind_IntegerLiteral);
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
    String8    value;
  };

  local_persist Keyword keywords[] =
  {
    { .kind = TokenKind_Nil,       .value = S("nil") },

    // integer types
    { .kind = TokenKind_S8,        .value = S("s8") },
    { .kind = TokenKind_S16,       .value = S("s16") },
    { .kind = TokenKind_S32,       .value = S("s32") },
    { .kind = TokenKind_S64,       .value = S("s64") },
    { .kind = TokenKind_U8,        .value = S("u8") },
    { .kind = TokenKind_U16,       .value = S("u16") },
    { .kind = TokenKind_U32,       .value = S("u32") },
    { .kind = TokenKind_U64,       .value = S("u64") },
    { .kind = TokenKind_Uintptr,   .value = S("uintptr") },
    { .kind = TokenKind_Int,       .value = S("int") },
    { .kind = TokenKind_Uint,      .value = S("uint") },

    // floating-point types
    { .kind = TokenKind_F32,       .value = S("f32") },
    { .kind = TokenKind_F64,       .value = S("f64") },

    // boolean
    { .kind = TokenKind_Bool,      .value = S("bool") },
    { .kind = TokenKind_True,      .value = S("true") },
    { .kind = TokenKind_False,     .value = S("false") },

    // control flow statements
    { .kind = TokenKind_If,          .value = S("if") },
    { .kind = TokenKind_Else,        .value = S("else") },
    { .kind = TokenKind_For,         .value = S("for") },
    { .kind = TokenKind_Do,          .value = S("do") },
    { .kind = TokenKind_While,       .value = S("while") },
    { .kind = TokenKind_Switch,      .value = S("switch") },
    { .kind = TokenKind_Case,        .value = S("case") },
    { .kind = TokenKind_Defer,       .value = S("defer") },
    { .kind = TokenKind_Break,       .value = S("break") },
    { .kind = TokenKind_Fallthrough, .value = S("fallthrough") },
    { .kind = TokenKind_Continue,    .value = S("continue") },
    { .kind = TokenKind_Return,      .value = S("return") },

    { .kind = TokenKind_Struct,    .value = S("struct") },
    { .kind = TokenKind_Union,     .value = S("union") },
    { .kind = TokenKind_Enum,      .value = S("enum") },
    { .kind = TokenKind_String,    .value = S("string") },

    // declarations
    { .kind = TokenKind_Proc,      .value = S("proc") },

    { .kind = TokenKind_SizeOf,    .value = S("size_of") },
    { .kind = TokenKind_Cast,      .value = S("cast") },
    { .kind = TokenKind_Transmute, .value = S("transmute") },
    { .kind = TokenKind_In,        .value = S("in") },
    { .kind = TokenKind_Then,      .value = S("then") },
    { .kind = TokenKind_Foreign,   .value = S("foreign") },
  };
  static_assert(array_count(keywords) == (TokenKind_Keyword_End - TokenKind_KeywordBegin));

  while (lexer_can_peek(l) && is_symbol(l))
  {
    lexer_eat(l);
  }

  Token t = lexer_make_token(l, TokenKind_Ident);

  // TODO(#5): String interning for faster string checks
  for (u32 i = 0; i < array_count(keywords); i++)
  {
    if (str8_equal(t.lexeme, keywords[i].value))
    {
      t.kind = keywords[i].kind;
      break;
    }
  }

  return t;
}

internal Token
lexer_next(Lexer *l)
{
  // lexer_eat_whitespace(l);

  for (;;)
  {
    if (!lexer_can_peek(l)) break;

    char c = lexer_peek(l);
    if (c == ' ' || c == '\t')
    {
      lexer_eat(l);
    }
    else if (c == '\r' || c == '\n')
    {
      // if the previous token was a terminator, we stop
      // here and return a semicolon instead of eating the newline
      if (l->insert_semicolon)
      {
        // PEEK AHEAD: Skip whitespaces/newlines to see what's coming
        u64 temp_cursor = l->cursor + 1;
        while (temp_cursor < l->source.count && isspace(l->source.data[temp_cursor]))
        {
          temp_cursor++;
        }

        // If the next actual token is a '{' or 'else', don't insert a semicolon!
        if (temp_cursor < l->source.count && l->source.data[temp_cursor] == '{')
        {
          l->insert_semicolon = false;
          lexer_eat(l); // Just eat the newline
          continue;
        }
        
        // Check for "else" keyword
        if (temp_cursor + 4 <= l->source.count &&
            l->source.data[temp_cursor + 0] == 'e' &&
            l->source.data[temp_cursor + 1] == 'l' &&
            l->source.data[temp_cursor + 2] == 's' &&
            l->source.data[temp_cursor + 3] == 'e' &&
            (temp_cursor + 4 >= l->source.count || !is_symbol_char(l->source.data[temp_cursor + 4])))
        {
          l->insert_semicolon = false;
          lexer_eat(l); // Just eat the newline
          continue;
        }

        l->insert_semicolon = false; // reset

        Token t = {0};
        t.pos.row = lexer_row(l) + 1;
        t.pos.col = l->cursor - l->bol + 1;
        t.pos.length = 1;
        t.kind = TokenKind_Semicolon;
        t.lexeme = str8_lit(";");
        return t;
      }
      lexer_eat(l);
    }
    else
    {
      break;
    }
  }
  l->start = l->cursor;

  if (!lexer_can_peek(l))
  {
    if (l->insert_semicolon)
    {
      Token t = {0};
      t.pos.row = lexer_row(l) + 1;
      t.pos.col = l->start - l->bol + 1;
      t.pos.length = 1;
      t.kind = TokenKind_Semicolon;
      t.lexeme = str8_lit(";");

      l->insert_semicolon = false;
      return t;
    }
    return lexer_make_token(l, TokenKind_EOF);
  }

  // TODO: make lexer_row, lexer_col return with +1
  Source_Pos pos = {0};
  pos.row = lexer_row(l) + 1;
  pos.col = l->start - l->bol + 1;

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

    b32 looks_like_float = false;
    if (lexer_can_peek(l))
    {
      if (tolower(lexer_peek(l)) == 'e')
      {
        looks_like_float = true;
      }
      else if (lexer_peek(l) == '.')
      {
        lexer_eat(l);
        if (lexer_can_peek(l) && lexer_peek(l) != '.')
        {
          looks_like_float = true;
        }
      }
    }

    l->cursor = l->start;
    // if (lexer_can_peek(l) &&
    //    (lexer_peek(l) == '.' || tolower(lexer_peek(l)) == 'e'))
    if (looks_like_float)
    {
      t = lexer_parse_float(l);
    }
    else
    {
      t = lexer_parse_integer(l);
    }
    break;

  case '(':
    lexer_eat(l);
    t = lexer_make_token(l, TokenKind_LParen);
    break;
  case ')':
    lexer_eat(l);
    t = lexer_make_token(l, TokenKind_RParen);
    break;
  case '[':
    lexer_eat(l);
    t = lexer_make_token(l, TokenKind_LBracket);
    break;
  case ']':
    lexer_eat(l);
    t = lexer_make_token(l, TokenKind_RBracket);
    break;
  case '{':
    lexer_eat(l);
    t = lexer_make_token(l, TokenKind_LBrace);
    break;
  case '}':
    lexer_eat(l);
    t = lexer_make_token(l, TokenKind_RBrace);
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
      t = lexer_make_token(l, TokenKind_AddAssign);
    }
    else
    {
      t = lexer_make_token(l, TokenKind_Plus);
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
      t = lexer_make_token(l, TokenKind_SubAssign);
    }
    else if (lexer_can_peek(l) && lexer_peek(l) == '>')
    {
      lexer_eat(l);
      t = lexer_make_token(l, TokenKind_Arrow);
    }
    else
    {
      t = lexer_make_token(l, TokenKind_Minus);
    }
    break;
  case '^':
    lexer_eat(l);
    if (lexer_can_peek(l) && lexer_peek(l) == '=')
    {
      lexer_eat(l);
      t = lexer_make_token(l, TokenKind_XorAssign);
    }
    else
    {
      t = lexer_make_token(l, TokenKind_Caret);
    }
    break;
  case '&':
    lexer_eat(l);
    if (lexer_can_peek(l) && lexer_peek(l) == '&')
    {
      lexer_eat(l);
      t = lexer_make_token(l, TokenKind_LogicalAnd);
    }
    else if (lexer_can_peek(l) && lexer_peek(l) == '=')
    {
      lexer_eat(l);
      t = lexer_make_token(l, TokenKind_AndAssign);
    }
    else
    {
      t = lexer_make_token(l, TokenKind_Ampersand);
    }
    break;
  case '|':
    lexer_eat(l);
    if (lexer_can_peek(l) && lexer_peek(l) == '|')
    {
      lexer_eat(l);
      t = lexer_make_token(l, TokenKind_LogicalOr);
    }
    else if (lexer_can_peek(l) && lexer_peek(l) == '=')
    {
      lexer_eat(l);
      t = lexer_make_token(l, TokenKind_OrAssign);
    }
    else
    {
      t = lexer_make_token(l, TokenKind_Pipe);
    }
    break;
  case '*':
    lexer_eat(l);
    if (lexer_can_peek(l) && lexer_peek(l) == '=')
    {
      lexer_eat(l);
      t = lexer_make_token(l, TokenKind_MulAssign);
    }
    else
    {
      t = lexer_make_token(l, TokenKind_Star);
    }
    break;
  case '/':
    lexer_eat(l);
    if (lexer_can_peek(l) && lexer_peek(l) == '=')
    {
      lexer_eat(l);
      t = lexer_make_token(l, TokenKind_DivAssign);
    }
    else
    {
      t = lexer_make_token(l, TokenKind_Slash);
    }
    break;
  case '%':
    lexer_eat(l);
    t = lexer_make_token(l, TokenKind_Percent);
    break;
  case '!':
    lexer_eat(l);
    if (lexer_can_peek(l) && lexer_peek(l) == '=')
    {
      lexer_eat(l);
      t = lexer_make_token(l, TokenKind_CmpNeq);
    }
    else
    {
      t = lexer_make_token(l, TokenKind_Exclamation);
    }
    break;
  case '?':
    lexer_eat(l);
    t = lexer_make_token(l, TokenKind_Question);
    break;
  case '=':
    lexer_eat(l);
    if (lexer_can_peek(l) && lexer_peek(l) == '=')
    {
      lexer_eat(l);
      t = lexer_make_token(l, TokenKind_CmpEq);
    }
    else
    {
      t = lexer_make_token(l, TokenKind_Equal);
    }
    break;
  case '.':
    lexer_eat(l);
    if (lexer_can_peek(l) && lexer_peek(l) == '*')
    {
      lexer_eat(l);
      t = lexer_make_token(l, TokenKind_Deref);
    }
    else if (lexer_can_peek(l) && lexer_peek(l) == '.')
    {
      lexer_eat(l);
      if (lexer_can_peek(l) && lexer_peek(l) == '<')
      {
        lexer_eat(l);
        t = lexer_make_token(l, TokenKind_RangeExcl);
      }
      else if (lexer_can_peek(l) && lexer_peek(l) == '=')
      {
        lexer_eat(l);
        t = lexer_make_token(l, TokenKind_RangeIncl);
      }
      else
      {
        lexer_syntax_error(l, "Range operator must explicitly be inclusive '..=' or exclusive '..<'");
      }
    }
    else
    {
      t = lexer_make_token(l, TokenKind_Dot);
    }
    break;
  case ',':
    lexer_eat(l);
    t = lexer_make_token(l, TokenKind_Comma);
    break;
  case ';':
    lexer_eat(l);
    t = lexer_make_token(l, TokenKind_Semicolon);
    break;
  case ':':
    lexer_eat(l);
    t = lexer_make_token(l, TokenKind_Colon);
    break;
  case '~':
    lexer_eat(l);
    t = lexer_make_token(l, TokenKind_Tilde);
    break;

  case '"':
    t = lexer_parse_string(l);
    break;

  case '\'':
  {
    char val = 0;
    lexer_eat(l);
    if (!lexer_can_peek(l))
    {
      lexer_syntax_error(l, "Unexpected end of file in character literal");
    }
    if (lexer_peek(l) == '\'')
    {
      lexer_syntax_error(l, "Character literal must not be empty");
    }
    else
    {
      if (lexer_peek(l) == '\n')
      {
        lexer_syntax_error(l, "Character literal must not contain newline");
      }
      else if (lexer_peek(l) == '\\')
      {
        lexer_eat(l);
        val = escape_to_char[lexer_peek(l)];
        if (val == 0 && lexer_peek(l) != '0')
        {
          lexer_syntax_error(l, "Invalid character literal escape '\\%c'", lexer_peek(l));
        }
        lexer_eat(l);
      }
      else
      {
        val = lexer_peek(l);
        lexer_eat(l);
      }

      if (lexer_peek(l) != '\'')
      {
        lexer_syntax_error(l, "Expected closing character single quote", lexer_peek(l));
      }
      else
      {
        lexer_eat(l);
      }
    }
    t = lexer_make_token(l, TokenKind_CharLiteral);
    t.value.character = val;
    break;
  }

  case '>':
    lexer_eat(l);
    if (lexer_can_peek(l) && lexer_peek(l) == '=')
    {
      lexer_eat(l);
      t = lexer_make_token(l, TokenKind_CmpGtEq);
    }
    else if (lexer_can_peek(l) && lexer_peek(l) == '>')
    {
      lexer_eat(l);
      if (lexer_can_peek(l) && lexer_peek(l) == '=')
      {
        lexer_eat(l);
        t = lexer_make_token(l, TokenKind_RShiftAssign);
      }
      else
      {
        t = lexer_make_token(l, TokenKind_RShift);
      }
    }
    else
    {
      t = lexer_make_token(l, TokenKind_CmpGt);
    }
    break;

  case '<':
    lexer_eat(l);
    if (lexer_can_peek(l) && lexer_peek(l) == '=')
    {
      lexer_eat(l);
      t = lexer_make_token(l, TokenKind_CmpLtEq);
    }
    else if (lexer_can_peek(l) && lexer_peek(l) == '<')
    {
      lexer_eat(l);
      if (lexer_can_peek(l) && lexer_peek(l) == '=')
      {
        lexer_eat(l);
        t = lexer_make_token(l, TokenKind_LShiftAssign);
      }
      else
      {
        t = lexer_make_token(l, TokenKind_LShift);
      }
    }
    else
    {
      t = lexer_make_token(l, TokenKind_CmpLt);
    }
    break;
  default:
    fprintf(stderr, "Unexpected token '%c'", lexer_peek(l));
    trap();
    break;
  }

  t.pos = pos;
  t.pos.length = l->cursor - l->start; // Set token length

  switch (t.kind)
  {
  case TokenKind_Ident:
  case TokenKind_Nil:
  case TokenKind_S8:
  case TokenKind_S16:
  case TokenKind_S32:
  case TokenKind_S64:
  case TokenKind_U8:
  case TokenKind_U16:
  case TokenKind_U32:
  case TokenKind_U64:
  case TokenKind_Uintptr:
  case TokenKind_Int:
  case TokenKind_Uint:
  case TokenKind_F32:
  case TokenKind_F64:
  case TokenKind_Bool:
  case TokenKind_String:
  case TokenKind_True:
  case TokenKind_False:
  case TokenKind_IntegerLiteral:
  case TokenKind_FloatLiteral:
  case TokenKind_StringLiteral:
  case TokenKind_CharLiteral:
  case TokenKind_RParen:
  case TokenKind_RBracket:
  case TokenKind_RBrace:
  // case TokenKind_Return:
  case TokenKind_Break:
  case TokenKind_Continue:
  case TokenKind_Deref:
  case TokenKind_Fallthrough:
    l->insert_semicolon = true;
    break;
  default:
    l->insert_semicolon = false;
    break;
  }

  return t;
}
