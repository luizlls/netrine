const { tokenize } = require('./lexer')
const { parse } = require('./parser')
const { analyze } = require('./analysis')
const { compile } = require('./compiler')


const source = `
hello(name) =
  print "Hello, {name}!"

fizzbuzz() =
  (range 0 100)
  |> map (
    fn num ->
      if zero? (num % 15)
        then "FizzBuzz"
      else if zero? (num % 3)
        then "Fizz"
      else if zero? (num % 5)
        then "Buzz"
      else
        string num
    )
  |> each print


factorial(n) =
  if n <= 1 then 1 else n * factorial (n - 1)


counter() = do
  counter = 0;
  div [
    button "+" { click() = counter := counter + 1 },
    p ["total ", strong "{counter}"],
    button "-" { click() = counter := counter - 1 },
  ]

sum = fold (+) 0

prod = fold (*) 1
`

try {
  const pipeline = [
    tokenize,
    parse,
    analyze,
    compile,
  ]

  const output = pipeline.reduce((partial, pass) => pass(partial), source)
  console.log(output)
} catch (e) {
  console.error(e)
}