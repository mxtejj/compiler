sum_while :: proc(n: int) -> int {
  total := 0
  i := 0
  while i < n {
    total += + i
    i += 1
  }
  return total
}

sum_for_in :: proc(n: int) -> int {
  total := 0
  for i in 0 ..< n {
    total += i
  }
  return total
}

sum_for :: proc(n: int) -> int {
  total := 0
  for i := 0; i < n; i += 1 {
    total += i
  }
  return total
}

main :: proc() -> int {
  a := sum_while(10)
  b := sum_for_in(10)
  c := sum_for(10)
  return a + b + c
}
