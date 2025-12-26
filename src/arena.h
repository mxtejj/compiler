#pragma once

#include "base.h"

#define ARENA_HEADER_SIZE sizeof(Arena)
#define ARENA_ALIGN       sizeof(void *)

#define ARENA_NUM_SCRATCH     2
#define ARENA_SCRATCH_RESERVE MB(64)
#define ARENA_SCRATCH_COMMIT  KB(64)

typedef u32 Arena_Flags;
enum
{
  Arena_Flag_Null     = 0,
  Arena_Flag_Growable = (1 << 0),
  Arena_Flag_Decommit = (1 << 1),
};

typedef struct Arena Arena;
struct Arena
{
  Arena *curr;
  Arena *prev;

  u64 reserve_size;
  u64 commit_size;

  u32 flags;

  u64 base_pos;
  u64 pos;
  u64 commit_pos;
};

typedef struct Arena_Temp Arena_Temp;
struct Arena_Temp
{
  Arena *arena;
  u64   pos;
};

#define push_struct(arena, T)                (T *)arena_push_((arena), sizeof(T), false, 0)
#define push_struct_align(arena, T, align)   (T *)arena_push_((arena), sizeof(T), false, align)
#define push_struct_nz(arena, T)             (T *)arena_push_((arena), sizeof(T), true, 0)
#define push_array(arena, T, n)              (T *)arena_push_((arena), sizeof(T) * (n), false, 0)
#define push_array_align(arena, T, n, align) (T *)arena_push_((arena), sizeof(T) * (n), false, align)
#define push_array_nz(arena, T, n)           (T *)arena_push_((arena), sizeof(T) * (n), true, 0)

Arena *arena_alloc(u64 reserve_size, u64 commit_size, Arena_Flags flags);
void   arena_delete(Arena *arena);
u64    arena_pos(Arena *arena);

#define arena_push(arena, size)    arena_push_((arena), (size), false)
#define arena_push_nz(arena, size) arena_push_((arena), (size), true)
void  *arena_push_(Arena *arena, u64 size, b32 non_zero, u64 align);

void   arena_pop(Arena *arena, u64 size);
void   arena_pop_to(Arena *arena, u64 pos);
void   arena_clear(Arena *arena);

Arena_Temp arena_temp_begin(Arena *arena);
void arena_temp_end(Arena_Temp temp);

Arena_Temp arena_scratch_get(Arena **conflicts, u32 num_conflicts);
void arena_scratch_release(Arena_Temp scratch);
