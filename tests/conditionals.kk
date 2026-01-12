min :: proc(a: int, b: int) -> int {
  if a < b {
    return a
  }
  return b
}

max :: proc(a: int, b: int) -> int {
  if a > b {
    return a
  }
  return b
}

main :: proc() -> int {
  x := min(42, 17)
  y := max(5, 99)
  return x + y
}
