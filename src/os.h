#pragma once

#include "base.h"

u32  os_page_size();
void *os_reserve(usize size);
bool os_commit(void *ptr, usize size);
bool os_decommit(void *ptr, usize size);
bool os_release(void *ptr, usize size);
