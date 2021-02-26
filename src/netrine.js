const { tokenize } = require('./lexer')
const { parse } = require('./parser')
const { analyze } = require('./analysis')
const { compile } = require('./compiler')


const source = `
hello = name =>
  print "Hello, {name}!"


fizzbuzz = () =>
  (range 0 100)
  |> map (
      num =>
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


factorial = n => if n <= 1 then 1 else n * factorial (n - 1)

factorial = n => fold (*) 1 (range 1 (n + 1))


counter = () =>
do
  total = mut 0;
  div [
    button "+" { click: () => total := total + 1 }
    p ["total ", strong "{total}"]
    button "-" { click: () => total := total - 1 }
  ]


sum = fold (+) 0

prod = fold (*) 1


numbers = list (range 0 10)
numbers.[0] := random()
numbers.[1] := numbers.[0] + 1
`

try {
  const pipeline = [
    tokenize,
    parse,
    // (nodes) => JSON.stringify(nodes, null, 4),
    analyze,
    compile,
  ]

  const output = pipeline.reduce((partial, pass) => pass(partial), source)
  console.log(output)
} catch (e) {
  console.error(e)
}