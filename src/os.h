#pragma once

#include "base.h"

//- Memory
function u32  os_page_size();
function void *os_reserve(usize size);
function b32 os_commit(void *ptr, usize size);
function b32 os_decommit(void *ptr, usize size);
function b32 os_release(void *ptr, usize size);
function void os_exit(u32 code);

//- File IO
function String8 os_read_entire_file(Arena *arena, String8 path);
function b32 os_write_entire_file(String8 path, String8 contents);
