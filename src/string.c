#include "string.h"

String
str_view(String s, u64 start, u64 end)
{
  return (String){ .data = &s.data[start], .count = end - start };
}
