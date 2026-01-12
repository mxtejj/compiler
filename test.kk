add :: proc(a: int, b: int) -> int {
  return a + b
}

main :: proc() -> int {
  b := 15
  a: int
  a = 5

  result := add(a, b)

  sum := 0
  i := 0
  do
  {
    sum = sum * i
    i = i + 1
  } while i < 10
}
