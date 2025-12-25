#include "os.h"
#include <windows.h>

u32
os_page_size()
{
  SYSTEM_INFO info = {0};
  GetSystemInfo(&info);
  return info.dwPageSize;
}

void *
os_reserve(usize size)
{
  return VirtualAlloc(NULL, size, MEM_RESERVE, PAGE_READWRITE);
}

bool
os_commit(void *ptr, usize size)
{
  void *result = VirtualAlloc(ptr, size, MEM_COMMIT, PAGE_READWRITE);
  return result != NULL;
}

bool
os_decommit(void *ptr, usize size)
{
  return VirtualFree(ptr, size, MEM_DECOMMIT);
}

bool
os_release(void *ptr, usize size)
{
  return VirtualFree(ptr, size, MEM_RELEASE);
}
