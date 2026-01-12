#include "strings.h"

internal String8
str8(u8 *data, u64 count)
{
  String8 result = {0};
  result.data  = data;
  result.count = count;
  return result;
}

internal String8
str8_init(Arena *arena, u64 count)
{
  String8 result = {0};
  result.data = push_array(arena, u8, count + 1);
  result.data[count] = '\0';
  result.count = count;
  return result;
}

internal String8
str8_range(u8 *first, u8 *one_past_last)
{
  String8 result = {0};
  if (first != 0 && one_past_last != 0 && first <= one_past_last)
  {
    result.data  = first;
    result.count = (u64)(one_past_last - first);
  }
  return result;
}

internal String8
str8_cstr(u8 *cstr)
{
  assert(false);
}

internal String8
str8_prefix(String8 str, u64 n)
{
  return (String8){ str.data, clamp_top(n, str.count) };
}

internal String8
str8_suffix(String8 str, u64 n)
{
  u64 size_clamped = clamp_top(n, str.count);
  u64 skip_to = str.count - size_clamped;
  return (String8){ str.data + skip_to, size_clamped };
}

internal String8
str8_chop(String8 str, u64 n)
{
  return (String8){ str.data, str.count - clamp_top(n, str.count) };
}

internal String8
str8_skip(String8 str, u64 n)
{
  u64 amount_clamped = clamp_top(n, str.count);
  u64 remaining_size = str.count - amount_clamped;
  return (String8){ str.data + amount_clamped, remaining_size };
}

internal String8
str8_substr(String8 str, u64 first, u64 one_past_last)
{
  first         = clamp_top(first, str.count);
  one_past_last = clamp_top(one_past_last, str.count);
  if (one_past_last < first)
  {
    one_past_last = first;
  }
  String8 result = { str.data + first, one_past_last - first };
  return result;
}

internal String8
str8_copy(Arena *arena, String8 str)
{
  String8 result = str8_init(arena, str.count);
  mem_copy(result.data, str.data, str.count);
  return result;
}

internal String8
str8fv(Arena *arena, char *fmt, va_list args)
{
  String8 result = {0};

  va_list args_copy;
  va_copy(args_copy, args);
  int bytes_needed = stbsp_vsnprintf(0, 0, fmt, args_copy);
  va_end(args_copy);

  if (bytes_needed > 0)
  {
    result.data = push_array_nz(arena, u8, bytes_needed + 1);
    result.count = stbsp_vsnprintf((char *)result.data, bytes_needed + 1, fmt, args);
    result.data[result.count] = '\0';
  }

  return result;
}

internal String8
str8f(Arena *arena, char *fmt, ...)
{
  va_list args;
  va_start(args, fmt);
  String8 result = str8fv(arena, fmt, args);
  va_end(args);
  return result;
}

internal void
str8_list_push_explicit(String8List *list, String8 string, String8Node *node_memory)
{
  node_memory->string = string;
  sll_queue_push(list->first, list->last, node_memory);
  list->node_count  += 1;
  list->strings_count += string.count;
}

internal void
str8_list_push(Arena *arena, String8List *list, String8 string)
{
  String8Node *node = push_array(arena, String8Node, 1);
  str8_list_push_explicit(list, string, node);
}

internal void
str8_list_pushf(Arena *arena, String8List *list, char *fmt, ...)
{
  va_list args;
  va_start(args, fmt);
  String8 string = str8fv(arena, fmt, args);
  va_end(args);
  str8_list_push(arena, list, string);
}

internal void
str8_list_push_front_explicit(String8List *list, String8 string, String8Node *node_memory)
{
  node_memory->string = string;
  sll_queue_push_front(list->first, list->last, node_memory);
  list->node_count  += 1;
  list->strings_count += string.count;
}

internal void
str8_list_push_front(Arena *arena, String8List *list, String8 string)
{
  String8Node *node = push_array(arena, String8Node, 1);
  str8_list_push_front_explicit(list, string, node);
}

internal void
str8_list_push_frontf(Arena *arena, String8List *list, char *fmt, ...)
{
  va_list args;
  va_start(args, fmt);
  String8 string = str8fv(arena, fmt, args);
  va_end(args);
  str8_list_push_front(arena, list, string);
}

internal void
str8_list_concat(String8List *dst, String8List *src)
{
  if (src->first == 0) return;

  if (dst->first == 0)
  {
    // dst is empty -> take src wholesale
    *dst = *src;
  }
  else
  {
    // link them together
    dst->last->next = src->first;
    dst->last = src->last;

    dst->node_count += src->node_count;
    dst->strings_count += src->strings_count;
  }
}

internal String8
str8_list_join(Arena *arena, String8List *list, StringJoin *optional_join)
{
  // setup join params
  local_persist StringJoin nil_join = {0};
  StringJoin *join = optional_join;
  if (join == NULL)
  {
    join = &nil_join;
  }

  String8 result = {0};

  // compute total size
  result.count = (join->pre.count +
                  join->post.count +
                  join->mid.count * (list->node_count - 1) +
                  list->strings_count);

  // begin string build
  result.data = push_array_nz(arena, u8, result.count + 1);
  u8 *ptr = result.data;

  // write pre
  mem_copy(ptr, join->pre.data, join->pre.count);
  ptr += join->pre.count;

  for (String8Node *node = list->first; node != 0; node = node->next)
  {
    // write node string
    mem_copy(ptr, node->string.data, node->string.count);
    ptr += node->string.count;

    if (node->next != 0)
    {
      mem_copy(ptr, join->mid.data, join->mid.count);
      ptr += join->mid.count;
    }
  }

  mem_copy(ptr, join->post.data, join->post.count);
  ptr += join->post.count;
  *ptr = 0;

  return result;
}

internal String8List
str8_split_chars(Arena *arena, String8 string, u8 *split_characters, u32 count)
{
  String8List result = {0};

  u8 *ptr = string.data;
  u8 *word_first = ptr;
  u8 *one_past_last = string.data + string.count;

  for (;ptr < one_past_last; ptr += 1)
  {
    // is this a split
    u8 byte = *ptr;
    b32 is_split_byte = false;
    for (u32 i = 0; i < count; i += 1)
    {
      if (byte == split_characters[i])
      {
        is_split_byte = true;
        break;
      }
    }

    if (is_split_byte)
    {
      // try to emit word, advance word first pointer
      if (word_first < ptr)
      {
        str8_list_push(arena, &result, str8_range(word_first, ptr));
      }
      word_first = ptr + 1;
    }
  }

  // try to emit final word
  if (word_first < ptr)
  {
    str8_list_push(arena, &result, str8_range(word_first, ptr));
  }

  return result;
}

internal String8List
str8_split(Arena *arena, String8 string, String8 split)
{
  return str8_split_chars(arena, string, split.data, split.count);
}

internal b32
str8_equal(String8 a, String8 b)
{
  if (a.count == b.count)
  {
    return mem_equal(a.data, b.data, a.count);
  }
  return false;
}

////////////////////////////////
//- Unicode Encode/Decode
internal Unicode
str_decode_utf8(u8 *data, u64 count)
{
  local_persist u32 replacement_char = 0xFFFD; // "�" (U+FFFD)

  // TODO: better implementation
  Unicode result = {0};
  b32 err = false;

  u8 byte0 = data[0];

  if ((byte0 >> 7) == 0)
  {
    // ASCII Code
    result.codepoint = byte0;
    result.size = 1;
  }
  else if ((byte0 >> 5) == 0b110)
  {
    // 2 byte codepoint
    if (count < 2)
    {
      err = true;
      goto error;
    }

    u8 byte1 = data[1];

    // byte1 must be continuation byte
    if ((byte1 >> 6) != 0b10)
    {
      err = true;
      goto error;
    }

    result.codepoint = (byte1 & 0b00111111) |
                      ((byte0 & 0b00011111) << 6);
    result.size = 2;
  }
  else if ((byte0 >> 4) == 0b1110)
  {
    // 3 byte
    if (count < 3)
    {
      err = true;
      goto error;
    }

    u8 byte1 = data[1];
    if ((byte1 >> 6) != 0b10)
    {
      err = true;
      goto error;
    }
    u8 byte2 = data[2];
    if ((byte2 >> 6) != 0b10)
    {
      err = true;
      goto error;
    }

    // byte0    | byte1    | byte2    | byte 4
    // 1110wwww   10xxxxyy   10yyzzzz   --------
    // 11100010   10000010   10101100   --------

    result.codepoint = (byte2 & 0b00111111)       |
                      ((byte1 & 0b00111111) << 6) |
                      ((byte0 & 0b00001111) << 12);
    result.size = 3;
  }
  else if ((byte0 >> 3) == 0b11110)
  {
    // 4 byte
    if (count < 4)
    {
      err = true;
      goto error;
    }

    u8 byte1 = data[1];
    if ((byte1 >> 6) != 0b10)
    {
      err = true;
      goto error;
    }
    u8 byte2 = data[2];
    if ((byte2 >> 6) != 0b10)
    {
      err = true;
      goto error;
    }
    u8 byte3 = data[3];
    if ((byte3 >> 6) != 0b10)
    {
      err = true;
      goto error;
    }

    result.codepoint = (byte3 & 0b00111111)        |
                      ((byte2 & 0b00111111) << 6)  |
                      ((byte1 & 0b00111111) << 12) |
                      ((byte0 & 0b00001111) << 18);
    result.size = 4;
  }

  error:
  if (err)
  {
    result.size = 1;
    result.codepoint = replacement_char;
  }

  return result;
}

internal u32
str_encode_utf8(u8 *data, u32 codepoint)
{
  u32 size = 0;
  if (codepoint < (1 << 7))
  {
    // ASCII
    data[0] = codepoint;
    size = 1;
  }
  else if (codepoint < (1 << 11))
  {
    // 2 byte
    // assert(data_len >= 2);

    // -xxxyyyyzzzz
    // 110xxxyy	10yyzzzz
    data[0] = 0b110 | (codepoint >> 6);
    data[1] = 0b10  | (codepoint & 0b111111);
    size = 2;
  }
  else if (codepoint < (1 << 16))
  {
    // 3 byte
    // assert(data_len >= 3);

    // wwwwxxxxyyyyzzzz
    // 1110wwww	10xxxxyy	10yyzzzz

    data[0] = 0b1110 | (codepoint >> 12);
    data[1] = 0b10   | ((codepoint >> 6) & 0b111111);
    data[2] = 0b10   | (codepoint & 0b111111);
    size = 3;
  }
  else if (codepoint < (1 << 21))
  {
    // 4 byte
    // assert(data_len >= 4);

    // ---uvvvvwwwwxxxxyyyyzzzz
    // 11110uvv	10vvwwww	10xxxxyy	10yyzzzz

    data[0] = 0b11110 | (codepoint >> 18);
    data[1] = 0b10    | ((codepoint >> 12) & 0b111111);
    data[2] = 0b10    | ((codepoint >> 6) & 0b111111);
    data[3] = 0b10    | (codepoint & 0b111111);
    size = 4;
  }

  return size;
}

internal Unicode
str_decode_utf16(u16 *data, u64 count)
{
  Unicode result = { '#', 1 };

  u16 x = data[0];
  if (x < 0xD800 || 0xDFFF < x)
  {
    result.codepoint = x;
  }
  else if (count >= 2)
  {
    u16 y = data[1];
    if (0xD800 <= x && x < 0xDC00 &&
        0xDC00 <= y && y < 0xE000)
    {
      u16 xj = x - 0xD800;
      u16 yj = y - 0xDC00;
      u32 xy = (xj << 10) | yj;
      result.codepoint = xy + 0x10000;
      result.size = 2;
    }
  }

  return result;
}

internal u32
str_encode_utf16(u16 *data, u32 codepoint)
{
  u32 size = 0;

  if (codepoint < 0x10000)
  {
    data[0] = codepoint;
    size = 1;
  }
  else// if (data_len >= 2)
  {
    u32 cpj = codepoint - 0x10000;
    data[0] = (cpj >> 10) + 0xD800;
    data[1] = (cpj & 0x3FF) + 0xDC00;
    size = 2;
  }

  return size;
}

////////////////////////////////
//- String Conversions
internal String8
str8_from_32(Arena *arena, String32 string)
{
  u64 alloc_count = string.count*4 + 1;
  u8 *memory = push_array_nz(arena, u8, alloc_count);

  u8 *dptr = memory;
  u32 *ptr = string.data;
  u32 *opl = string.data + string.count;
  for (; ptr < opl;)
  {
    // Unicode decode = str_decode_utf8(ptr, (u64)(opl - ptr));
    u32 size = str_encode_utf8(dptr, *ptr);
    // *dptr = decode.codepoint;
    ptr += 1;
    dptr += size;
  }

  *dptr = 0;

  u64 string_count = (u64)(dptr - memory);
  u64 unused_count = alloc_count - string_count - 1;

  arena_pop(arena, unused_count * sizeof(*memory));

  String8 result = { memory, string_count };
  return result;
}

internal String8
str8_from_16(Arena *arena, String16 string)
{
  u64 alloc_count = string.count*3 + 1;
  u8 *memory = push_array_nz(arena, u8, alloc_count);

  u8 *dptr = memory;
  u16 *ptr = string.data;
  u16 *opl = string.data + string.count;
  for (; ptr < opl;)
  {
    Unicode decode = str_decode_utf16(ptr, (u64)(opl - ptr));
    u16 encode_size = str_encode_utf8(dptr, decode.codepoint);
    // *dptr = decode.codepoint;
    ptr += decode.size;
    dptr += encode_size;
  }

  *dptr = 0;

  u64 string_count = (u64)(dptr - memory);
  u64 unused_count = alloc_count - string_count - 1;

  arena_pop(arena, unused_count * sizeof(*memory));

  String8 result = { memory, string_count };
  return result;
}

internal String16
str16_from_8(Arena *arena, String8 string)
{
  u64 alloc_count = string.count*2 + 1;
  u16 *memory = push_array_nz(arena, u16, alloc_count);

  u16 *dptr = memory;
  u8 *ptr = string.data;
  u8 *opl = string.data + string.count;
  for (; ptr < opl;)
  {
    Unicode decode = str_decode_utf8(ptr, (u64)(opl - ptr));
    u32 encode_size = str_encode_utf16(dptr, decode.codepoint);
    ptr += decode.size;
    dptr += encode_size;
  }

  *dptr = 0;

  u64 string_count = (u64)(dptr - memory);
  u64 unused_count = alloc_count - string_count - 1;

  arena_pop(arena, unused_count * sizeof(*memory));

  String16 result = { memory, string_count };
  return result;
}

internal String32
str32_from_8(Arena *arena, String8 string)
{
  u64 alloc_count = string.count + 1;
  u32 *memory = push_array_nz(arena, u32, alloc_count);

  u32 *dptr = memory;
  u8 *ptr = string.data;
  u8 *opl = string.data + string.count;
  for (; ptr < opl;)
  {
    Unicode decode = str_decode_utf8(ptr, (u64)(opl - ptr));
    *dptr = decode.codepoint;
    ptr += decode.size;
    dptr += 1;
  }

  *dptr = 0;

  u64 string_count = (u64)(dptr - memory);
  u64 unused_count = alloc_count - string_count - 1;

  arena_pop(arena, unused_count * sizeof(*memory));

  String32 result = { memory, string_count };
  return result;
}
