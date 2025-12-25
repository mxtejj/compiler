#include "arena.h"
#include "os.h"

Arena *
arena_alloc(u64 reserve_size, u64 commit_size, Arena_Flags flags)
{
  u32 page_size = os_page_size();

  reserve_size = align_up_pow2(reserve_size, page_size);
  commit_size  = align_up_pow2(commit_size, page_size);

  Arena *arena = os_reserve(reserve_size);

  if (!os_commit(arena, commit_size))
  {
    arena = NULL;
  }

  // TODO: Graphical assert
  assert(arena != NULL && "Failed to commit arena memory");

  // if (arena == NULL)
  // {
  //   plat_fatal_error("Fatal error: unable to commit memory for arena", 1);
  // }

  // TODO: ASAN

  arena->curr = arena;
  arena->prev = NULL;

  arena->reserve_size = reserve_size;
  arena->commit_size  = commit_size;

  arena->flags = flags;

  arena->base_pos = 0;
  arena->pos = ARENA_HEADER_SIZE;
  arena->commit_pos = commit_size;

  return arena;
}

void
arena_delete(Arena *arena)
{
  Arena *curr = arena->curr;

  while (curr != NULL)
  {
    Arena *prev = curr->prev;
    os_release(curr, curr->reserve_size);

    curr = prev;
  }
}

u64
arena_pos(Arena *arena)
{
  return arena->curr->base_pos + arena->curr->pos;
}

void *
arena_push_(Arena *arena, u64 size, b32 non_zero)
{
  void *out = NULL;

  Arena *curr = arena->curr;

  u64 pos_aligned = align_up_pow2(curr->pos, ARENA_ALIGN);
  out = (u8 *)curr + pos_aligned;
  u64 new_pos = pos_aligned + size;

  if (new_pos > curr->reserve_size)
  {
    out = NULL;

    if (arena->flags & Arena_Flag_Growable)
    {
      u64 reserve_size = arena->reserve_size;
      u64 commit_size = arena->commit_size;

      if (size + ARENA_HEADER_SIZE > reserve_size)
      {
        reserve_size = align_up_pow2(size + ARENA_HEADER_SIZE, ARENA_ALIGN);
      }

      Arena *new_arena = arena_alloc(reserve_size, commit_size, arena->flags);
      new_arena->base_pos = curr->base_pos + curr->reserve_size;

      Arena *prev_cur = curr;
      curr = new_arena;
      curr->prev = prev_cur;
      arena->curr = curr;

      pos_aligned = align_up_pow2(curr->pos, ARENA_ALIGN);
      out = (u8 *)curr + pos_aligned;
      new_pos = pos_aligned + size;
    }
  }

  if (new_pos > curr->commit_pos)
  {
    u64 new_commit_pos = new_pos;
    new_commit_pos += curr->commit_size - 1;
    new_commit_pos -= new_commit_pos % curr->commit_size;
    new_commit_pos = MIN(new_commit_pos, curr->reserve_size);

    u64 commit_size = new_commit_pos - curr->commit_pos;
    u8 *commit_ptr  = (u8 *)curr + curr->commit_pos;

    if (!os_commit(commit_ptr, commit_size))
    {
      out = NULL;
    }
    else
    {
      curr->commit_pos = new_commit_pos;
    }
  }

  assert(out != NULL && "Failed to allocate memory on arena");
  curr->pos = new_pos;

  if (!non_zero)
  {
    mem_zero(out, size);
  }

  return out;
}

void
arena_pop(Arena *arena, u64 size)
{
  size = MIN(size, arena_pos(arena));

  Arena *curr = arena->curr;
  while (curr != NULL && size > curr->pos)
  {
    Arena *prev = curr->prev;

    size -= curr->pos;
    os_release(curr, curr->reserve_size);

    curr = prev;
  }

  arena->curr = curr;

  size = MIN(curr->pos - ARENA_HEADER_SIZE, size);
  curr->pos -= size;

  if (arena->flags & Arena_Flag_Decommit)
  {
    u64 required_commit_pos = curr->pos + curr->commit_size - 1;
    required_commit_pos -= required_commit_pos % curr->commit_size;

    if (required_commit_pos < arena->commit_pos)
    {
      u8* commit_ptr = (u8*)curr + required_commit_pos;

      if (!os_decommit(commit_ptr, arena->commit_pos - required_commit_pos))
      {
        assert(!"Failed to decommit arena memory!");
      }

      arena->commit_pos = required_commit_pos;
    }
  }
}

void
arena_pop_to(Arena *arena, u64 pos)
{
  u64 cur_pos = arena_pos(arena);
  pos = MIN(pos, cur_pos);
  arena_pop(arena, cur_pos - pos);
}

void
arena_clear(Arena *arena)
{
  arena_pop_to(arena, ARENA_HEADER_SIZE);
}

Arena_Temp
arena_temp_begin(Arena *arena)
{
  Arena_Temp temp = {0};
  temp.arena = arena;
  temp.pos   = arena_pos(arena);
  return temp;
}

void
arena_temp_end(Arena_Temp temp)
{
  arena_pop_to(temp.arena, temp.pos);
}

global thread_local Arena *scratch_arenas[2];

Arena_Temp
arena_scratch_get(Arena **conflicts, u32 num_conflicts)
{
  s32 scratch_index = -1;

  for (s32 i = 0; i < 2; i++)
  {
    b32 conflict_found = false;

    for (u32 j = 0; j < num_conflicts; j++)
    {
      if (scratch_arenas[i] == conflicts[j])
      {
        conflict_found = true;
        break;
      }
    }

    if (!conflict_found)
    {
      scratch_index = i;
      break;
    }
  }

  if (scratch_index == -1)
  {
    return (Arena_Temp){0};
  }

  if (scratch_arenas[scratch_index] == NULL)
  {
    scratch_arenas[scratch_index] = arena_alloc(
      ARENA_SCRATCH_RESERVE,
      ARENA_SCRATCH_COMMIT,
      0
    );
  }

  return arena_temp_begin(scratch_arenas[scratch_index]);
}

void
arena_scratch_release(Arena_Temp scratch)
{
  arena_temp_end(scratch);
}
