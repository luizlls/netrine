import { tokenize } from './src/lexer'
import { parse } from './src/parser'
import { desugar } from './src/desugar'
import { compile } from './src/compiler'


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


counter() =
  count := 0;
  div [
    button "+" { click() = count := count + 1 },
    p ["total ", strong "{count}"],
    button "-" { click() = count := count - 1 },
  ]
`

const source2 = `
main =
  print (if True and not False then { a = 1 } else { a = 2 }).a
`


try {
  console.log(compile(desugar(parse(tokenize(source2)))))
} catch (e) {
  console.error(e)
}