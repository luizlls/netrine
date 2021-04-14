const { tokenize } = require('./lexer')
const { parse } = require('./parser')
const { analyze } = require('./analysis')
const { compile } = require('./compiler')


const source = `
hello = fn name {
  print "Hello, {name}!"
}


fizzbuzz = fn {
  for num (range 0 100) {
    if zero? (num % 15)
      then: print "FizzBuzz"
    else: if zero? (num % 3)
      then: print "Fizz"
    else: if zero? (num % 5)
      then: print "Buzz"
    else:
      print (string num)
  }
}


fac = fn n {
  if n <= 1 then: 1 else: n * fac (n - 1)
}


counter = fn {
  total = mut 0
  div [
    button "+" [ click: fn total := total + 1 ],
    p "total " (strong total)
    button "-" [ click: fn total := total - 1 ],
  ]
}

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
    pretty,
    // analyze,
    // compile,
  ]

  const output = pipeline.reduce((partial, pass) => pass(partial), source)
  console.log(output)
} catch (e) {
  console.error(e)
}