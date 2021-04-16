factorial = fn n {
  case n of
    1 -> 1
  | n -> n * factorial (n - 1)
}