#include "os.h"

#pragma push_macro("function")
#undef function
#define WIN32_LEAN_AND_MEAN
#define NO_MIN_MAX
#include <windows.h>
#pragma pop_macro("function")

function u32
os_page_size() {
  SYSTEM_INFO info = {0};
  GetSystemInfo(&info);
  return info.dwPageSize;
}

function void *
os_reserve(usize size) {
  return VirtualAlloc(NULL, size, MEM_RESERVE, PAGE_READWRITE);
}

function b32
os_commit(void *ptr, usize size) {
  void *result = VirtualAlloc(ptr, size, MEM_COMMIT, PAGE_READWRITE);
  return result != NULL;
}

function b32
os_decommit(void *ptr, usize size) {
  return VirtualFree(ptr, size, MEM_DECOMMIT);
}

function b32
os_release(void *ptr, usize size) {
  return VirtualFree(ptr, size, MEM_RELEASE);
}

function void
os_exit(u32 code) {
  ExitProcess(code);
}

function String8
os_read_entire_file(Arena *arena, String8 path) {
  Temp scratch = arena_scratch_get(&arena, 1);

  String8 result = {0};
  String16 path16 = str16_from_8(scratch.arena, path);

  HANDLE handle = CreateFileW(path16.data, GENERIC_READ, 0, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
  if (handle == INVALID_HANDLE_VALUE) {
    return result;
  }

  LARGE_INTEGER file_size = {0};
  if (!GetFileSizeEx(handle, &file_size)) {
    CloseHandle(handle);
    return result;
  }
  assert(file_size.QuadPart > 0);

  u8 *data = push_array(arena, u8, file_size.QuadPart + 1);

  DWORD bytes_read = 0;
  b32 ok = ReadFile(handle, data, (DWORD)file_size.QuadPart, &bytes_read, NULL);
  assert(ok && bytes_read == file_size.QuadPart);

  data[bytes_read] = '\0';

  CloseHandle(handle);

  result.data  = data;
  result.count = bytes_read;

  arena_scratch_release(scratch);
  return result;
}

function b32
os_write_entire_file(String8 path, String8 contents) {
  Temp scratch = arena_scratch_get(NULL, 0);

  String16 path16 = str16_from_8(scratch.arena, path);

  HANDLE handle = CreateFileW(path16.data, GENERIC_WRITE, 0, NULL, TRUNCATE_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
  if (handle == INVALID_HANDLE_VALUE) {
    return false;
  }

  assert(!"Write file");

  arena_scratch_release(scratch);
  return false;
}
