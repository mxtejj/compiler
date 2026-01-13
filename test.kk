cstring   :: *int
cstring16 :: *int

add :: proc(
  a: int,
  b: int,
) -> int {
  return a + b
}

main :: proc() -> int {
  b := 15
  a: int
  a = 5

  result := add(a, b)

  sum := 0
  i := 0
  do {
    sum += i
    i += 1
  } while i < 10

  A :: size_of(Point)
  arr: [A]int

  modify_int(&b)
}

distance :: proc(p1: Point, p2: Point) -> int {
  dx := p1.x - p2.x
  dy := p1.y - p2.y
  return dx*dx + dy*dy
}

Point :: struct {
  x, y: int
}

hello_world :: proc() {
  a := 5
  b := 15
  c := add(a, b)
}

arithmetic :: proc() {
  1 + 1
  1 - 1
  1 / 1
  1 * 1
  1 % 1
}

comparisons :: proc() {
  1 == 1
  1 != 1
  1 <  1
  1 >  1
  1 <= 1
  1 >= 1
}

variables :: proc() {
  my_int: int
  my_int = 5
  other_int := 10
}

if_else :: proc(n: int) -> int {
  result: int
  if n < 0 {
    result = -1
  } else if n == 0 {
    result = n - 10
  } else if n > 0 && n < 100 {
    result = 20
  } else {
    result = n
  }
  return result
}

literals :: proc() {
  int_literal := 5
  float_literal := 3.14
  string_literal := "hello world"

  string_array: [6]string = .{
    "hello,",
    " world",
    "!",
  }

  inferred_compound_string_array := [2]string.{ "hello", "world" }

  Array2x2 :: [2][2]int
  test_type_alias := Array2x2.{ .{69, 420}, .{69, 420} }
}

modify_int :: proc(n: *int)
{
  n.* = 5
}

pointer_stuff :: proc()
{
  a := 0
  modify_int(&a)
  b := a
}

global_string := "TODO: this string does not get the value set!!!"
