import * as readline from 'readline'
import * as filesystem from 'fs'

import { tokenize } from './lexer'
import { parse } from './parser'
import { analyze } from './analysis'
import { compile } from './compiler'


const file = (path: string) => {
  filesystem.readFile(path, null, (err, data) => {
    console.log(evaluate(data.toString('utf8')))
  })
}


const repl = () => {
  const rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout,
    terminal: false,
  })

  process.stdout.write('>>>> ')

  let lines: string[] = []

  rl.on('line', line => {
    if (line != null && line != '') {
      return lines.push(line.trim())
    }
    const source = lines.join('\n')
    console.log(evaluate(source))
    process.stdout.write('>>>> ')
    lines = []
  })
}


const pretty = (node: string): string =>
  JSON.stringify(node, null, 4)

const pipeline = [
  tokenize,
  parse,
  analyze,
  compile,
  //pretty,
]

const evaluate = (source: string) => {
  return pipeline.reduce((partial: any, pass) => pass(partial), source)
}

try {
  const args = process.argv.slice(2)
  if (args.length != 0) {
    file(args[0])
  } else {
    repl()
  }
} catch (e) {
  console.error(e)
}