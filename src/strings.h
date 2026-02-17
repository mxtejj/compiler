#pragma once

#include <stdarg.h>
#include "base.h"
#include "arena.h"
#include "raddbg_markup.h"

typedef struct String8 String8;
typedef struct String8Node String8Node;
typedef struct String8List String8List;
typedef struct String16 String16;
typedef struct String32 String32;
typedef struct StringJoin StringJoin;
typedef struct Unicode Unicode;

struct String8 {
  u8 *data;
  u64 count;
};
raddbg_type_view(String8, array(data, count));

struct String8Node {
  String8Node *next;
  String8 string;
};

struct String8List {
  String8Node *first;
  String8Node *last;
  u64 node_count;
  u64 strings_count;
};

struct String16 {
  u16 *data;
  u64  count;
};
raddbg_type_view(String16, array(data, count));

struct String32 {
  u32 *data;
  u64  count;
};
raddbg_type_view(String32, array(data, count));

struct StringJoin {
  String8 pre;
  String8 mid;
  String8 post;
};

struct Unicode {
  u32 codepoint;
  u32 size; // how many bytes
};

// #define str8_lit(s) str8((u8 *)(s), sizeof((s)) - 1)
#define str8_lit(s) str8((u8 *)(s), sizeof((s)) - 1)
#define S(s) { (u8 *)(s), sizeof((s)) - 1 }
#define str8_varg(s) (int)((s).count), ((s).data)

function String8 str8(u8 *data, u64 count);
function String8 str8_init(Arena *arena, u64 count);
function String8 str8_range(u8 *first, u8 *one_past_last);
function String8 str8_cstr(u8 *cstr);
function String8 str8_prefix(String8 str, u64 n);
function String8 str8_suffix(String8 str, u64 n);
function String8 str8_chop(String8 str, u64 n);
function String8 str8_skip(String8 str, u64 n);
function String8 str8_substr(String8 str, u64 first, u64 one_past_last);
function String8 str8_copy(Arena *arena, String8 str);
function String8 str8fv(Arena *arena, char *fmt, va_list args);
function String8 str8f(Arena *arena, char *fmt, ...);
function void    str8_list_push_explicit(String8List *list, String8 string, String8Node *node_memory);
function void    str8_list_push(Arena *arena, String8List *list, String8 string);
function void    str8_list_pushf(Arena *arena, String8List *list, char *fmt, ...);
function void    str8_list_push_front_explicit(String8List *list, String8 string, String8Node *node_memory);
function void    str8_list_push_front(Arena *arena, String8List *list, String8 string);
function void    str8_list_push_frontf(Arena *arena, String8List *list, char *fmt, ...);
function void    str8_list_concat(String8List *dst, String8List *src);
function String8 str8_list_join(Arena *arena, String8List *list, StringJoin *optional_join);
function String8List str8_split_chars(Arena *arena, String8 string, u8 *split_characters, u32 count);
function String8List str8_split(Arena *arena, String8 string, String8 split);

function b32 str8_equal(String8 a, String8 b);

function String16 str16(u16 *data, u64 count);
function String32 str32(u32 *data, u64 count);

function String16 str16_cstr(u8 *cstr);
function String32 str32_cstr(u8 *cstr);

// TODO: Cleanup
function Unicode str_decode_utf8(u8 *data, u64 count);
function u32     str_encode_utf8(u8 *data, u32 codepoint);
function Unicode str_decode_utf16(u16 *data, u64 count);
function u32     str_encode_utf16(u16 *data, u32 codepoint);

function String8  str8_from_32(Arena *arena, String32 string);
function String8  str8_from_16(Arena *arena, String16 string);
function String16 str16_from_8(Arena *arena, String8  string);
function String32 str32_from_8(Arena *arena, String8  string);
