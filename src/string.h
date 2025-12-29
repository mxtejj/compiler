#pragma once

#include "base.h"
#include "raddbg_markup.h"

typedef struct String String;
struct String
{
  u8 *data;
  u64 count;
};
raddbg_type_view(String, array(data, count));

#define S(x) { .data = (u8 *)(x), .count = sizeof((x)) - 1 }
#define str_comp(x) (String){ .data = (u8 *)(x), .count = sizeof((x)) - 1 }
#define strf(s) (int)(s.count), (char *)(s.data)

String str_init(Arena *a, u64 count);
String str_view(String s, u64 start, u64 end);
String str_copy(Arena *a, String s);
bool str_equal(String a, String b);
