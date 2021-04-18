fizzbuzz = fn {
  for num (range 100) {
    if (zero? (num % 15)) {
      print "FizzBuzz"
    } else: if (zero? (num % 3)) {
      print "Fizz"
    } else: if (zero? (num % 5)) {
      print "Buzz"
    } else: {
      print (string num)
    }
  }
}

fizzbuzz = fn num {
  case (num % 3, num % 5) of
    (0, 0) => "FizzBuzz"
  | (0, _) => "Fizz"
  | (_, 0) => "Buzz"
  | (_, _) => (string num)
}

(range 100)
  |> map fizzbuzz
  |> each println