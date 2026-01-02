#pragma once

#include <stdarg.h>
#include "base.h"
#include "arena.h"
#include "raddbg_markup.h"

typedef struct String8 String8;
struct String8
{
  u8 *data;
  u64 count;
};
raddbg_type_view(String8, array(data, count));

typedef struct String8Node String8Node;
struct String8Node
{
  String8Node *next;
  String8 string;
};

typedef struct String8List String8List;
struct String8List
{
  String8Node *first;
  String8Node *last;
  u64 node_count;
  u64 strings_count;
};

typedef struct String16 String16;
struct String16
{
  u16 *data;
  u64  count;
};
raddbg_type_view(String16, array(data, count));

typedef struct String32 String32;
struct String32
{
  u32 *data;
  u64  count;
};
raddbg_type_view(String32, array(data, count));

typedef struct StringJoin StringJoin;
struct StringJoin
{
  String8 pre;
  String8 mid;
  String8 post;
};

typedef struct Unicode Unicode;
struct Unicode
{
  u32 codepoint;
  u32 size; // how many bytes
};

// #define str8_lit(s) str8((u8 *)(s), sizeof((s)) - 1)
#define str8_lit(s) str8((u8 *)(s), sizeof((s)) - 1)
#define S(s) { (u8 *)(s), sizeof((s)) - 1 }
#define str8_varg(s) (int)((s).count), ((s).data)

internal String8 str8(u8 *data, u64 count);
internal String8 str8_init(Arena *arena, u64 count);
internal String8 str8_range(u8 *first, u8 *one_past_last);
internal String8 str8_cstr(u8 *cstr);
internal String8 str8_prefix(String8 str, u64 n);
internal String8 str8_suffix(String8 str, u64 n);
internal String8 str8_chop(String8 str, u64 n);
internal String8 str8_skip(String8 str, u64 n);
internal String8 str8_substr(String8 str, u64 first, u64 one_past_last);
internal String8 str8_copy(Arena *arena, String8 str);
internal String8 str8fv(Arena *arena, char *fmt, va_list args);
internal String8 str8f(Arena *arena, char *fmt, ...);
internal void    str8_list_push_explicit(String8List *list, String8 string, String8Node *node_memory);
internal void    str8_list_push(Arena *arena, String8List *list, String8 string);
internal void    str8_list_pushf(Arena *arena, String8List *list, char *fmt, ...);
internal String8 str8_list_join(Arena *arena, String8List *list, StringJoin *optional_join);
internal String8List str8_split_chars(Arena *arena, String8 string, u8 *split_characters, u32 count);
internal String8List str8_split(Arena *arena, String8 string, String8 split);

internal b32 str8_equal(String8 a, String8 b);

internal String16 str16(u16 *data, u64 count);
internal String32 str32(u32 *data, u64 count);

internal String16 str16_cstr(u8 *cstr);
internal String32 str32_cstr(u8 *cstr);

// TODO(mxtej): Cleanup
internal Unicode str_decode_utf8(u8 *data, u64 count);
internal u32     str_encode_utf8(u8 *data, u32 codepoint);
internal Unicode str_decode_utf16(u16 *data, u64 count);
internal u32     str_encode_utf16(u16 *data, u32 codepoint);

internal String8  str8_from_32(Arena *arena, String32 string);
internal String8  str8_from_16(Arena *arena, String16 string);
internal String16 str16_from_8(Arena *arena, String8  string);
internal String32 str32_from_8(Arena *arena, String8  string);
