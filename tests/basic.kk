Vec2 :: struct {
  x, y: int,
}

add_v2 :: proc(a: Vec2, b: Vec2) -> Vec2 {
  return .{ a.x + b.x, a.y + b.y }
}

dot_v2 :: proc(a: Vec2, b: Vec2) -> int {
  return a.x * b.x + a.y * b.y
}

add :: proc(a: int, b: int) -> int {
  return a + b
}

factorial :: proc(n: int) -> int {
  if n <= 1 {
    return 1
  }
  return n * factorial(n - 1)
}

main :: proc() -> int {
  x := 5
  y := 10
  sum := add(x, y)

  f := factorial(5)

  v1 := Vec2.{3, 4}
  v2 := Vec2.{5, 6}
  result := dot(v1, v2)

  return result
}
