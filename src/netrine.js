const readline = require('readline')
const filesystem = require('fs')

const { tokenize } = require('./lexer')
const { parse } = require('./parser')
const { analyze } = require('./analysis')
const { compile } = require('./compiler')
const { pretty, sexpr } = require('./pprint')


const file = (path) => {
  filesystem.readFile(path, null, (err, data) => {
    console.log(eval(data.toString('utf8')))
  })
}


const repl = () => {
  const rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout,
    terminal: false,
  })

  process.stdout.write('>>>> ')

  rl.on('line', line => {
    console.log(eval(line))
    process.stdout.write('>>>> ')
  })
}


const pipeline = [
  tokenize,
  parse,
  (module) => module.nodes.map(it => sexpr(it)),
  // pretty,
  // analyze,
  // compile,
]

const eval = (source) => {
  return pipeline.reduce((partial, pass) => pass(partial), source)
}

try {
  const args = process.argv.slice(2)
  if (args.length) {
    file(args.shift())
  } else {
    repl()
  }
} catch (e) {
  console.error(e)
}