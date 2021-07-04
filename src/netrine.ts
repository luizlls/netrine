import * as readline from 'readline'
import * as filesystem from 'fs'

import { tokenize } from './syntax/lexer'

function file(path: string) {
  filesystem.readFile(path, null, (err, data) => {
    console.log(evaluate(data.toString('utf8')))
  })
}

function repl() {
  const rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout,
    terminal: false,
  })

  process.stdout.write('>>>> ')

  let lines: string[] = []

  rl.on('line', line => {
    if (line != null && line != '') {
      process.stdout.write('>>>> ')
      return lines.push(line.trim())
    }
    const source = lines.join('\n')
    console.log(evaluate(source))
    process.stdout.write('>>>> ')
    lines = []
  })
}

function pretty(node: string): string {
  return JSON.stringify(node, null, 4)
}

const pipeline = [
  tokenize,
  pretty,
]

function evaluate(source: string) {
  return pipeline.reduce((partial: any, pass) => pass(partial), source)
}

try {
  const args = process.argv.slice(2)
  if (args.length) {
    file(args[0])
  } else {
    repl()
  }
} catch (e) {
  console.error(e)
}