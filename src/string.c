#include "string.h"

String
str_init(Arena *a, u64 count)
{
  String s = {0};
  s.data = push_array(a, u8, count + 1);
  s.data[count] = '\0';
  s.count = count;
  return s;
}

String
str_view(String s, u64 start, u64 end)
{
  return (String){ .data = &s.data[start], .count = end - start };
}

String str_copy(Arena *a, String s)
{
  String result = str_init(a, s.count);
  mem_copy(result.data, s.data, s.count);
  return result;
}

bool
str_equal(String a, String b)
{
  // if (a.data == NULL || b.data == NULL) return false;
  if (a.count != b.count) return false;
  return mem_equal(a.data, b.data, a.count);

  // for (u64 i = 0; i < a.count; i++)
  // {
  //   if (a.data[i] != b.data[i])
  //   {
  //     return false;
  //   }
  // }

  // return true;
}
