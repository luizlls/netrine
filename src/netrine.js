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


counter = () =>
do
  total = mut 0;
  div [
    button "+" { click: () => total := total + 1 },
    p ["total ", strong "{total}"],
    button "-" { click: () => total := total - 1 },
  ]


sum = fold (+) 0

prod = fold (*) 1


optional = Some 10
`

const pretty = (nodes) =>
  JSON.stringify(nodes, null, 4);

try {
  const pipeline = [
    tokenize,
    parse,
    // pretty
    analyze,
    compile,
  ]

  const output = pipeline.reduce((partial, pass) => pass(partial), source)
  console.log(output)
} catch (e) {
  console.error(e)
}