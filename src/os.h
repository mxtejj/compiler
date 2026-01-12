#pragma once

#include "base.h"

//- Memory
internal u32  os_page_size();
internal void *os_reserve(usize size);
internal b32 os_commit(void *ptr, usize size);
internal b32 os_decommit(void *ptr, usize size);
internal b32 os_release(void *ptr, usize size);
internal void os_exit(u32 code);

//- File IO
internal String8 os_read_entire_file(Arena *arena, String8 path);
internal b32 os_write_entire_file(String8 path, String8 contents);
