main :: proc() -> s32
{
  print_int(432186)
}

foreign FILE :: struct {}

foreign fwrite :: proc(buffer: *u8, element_size: u64, element_count: u64, stream: *FILE)
foreign __acrt_iob_func :: proc(_Ix: u32) -> *FILE

_stdin :: proc() -> *FILE {
  return __acrt_iob_func(0)
}
_stdout :: proc() -> *FILE {
  return __acrt_iob_func(1)
}
_stderr :: proc() -> *FILE {
  return __acrt_iob_func(2)
}

print :: proc(text: string) {
  fwrite(text.data, 1, text.len, _stdout())
}

print_int :: proc(n: int)
{
  buf: [16]u8
  i := 0
  neg := n < 0

  if neg
  {
    n = -n
  }

  if n == 0
  {
    buf[i] = cast(u8)'0'
    i += 1
  }

  while n > 0
  {
    buf[i] = cast(u8)'0' + cast(u8)(n % 10)
    i += 1
    n /= 10
  }

  if neg
  {
    buf[i] = cast(u8)'-'
    i += 1
  }

  while i > 0
  {
    i -= 1
    fwrite(&buf[i], 1, 1, _stdout())
  }
}

println :: proc(text: string) {
  print(text)
  print("\n")
}
