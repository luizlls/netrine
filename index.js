const SYMBOLS = '.%^&|:=~+-*<>!/'

const keywords = new Set([
  'do',
  'if',
  'then',
  'else',
  'and',
  'or',
  'is',
  'not',
  'for',
])

const operators = {
  '.'  : 'dot',
  ':'  : 'colon',
  '='  : 'equal',
  '->' : 'rarrow',
  '=>' : 'farrow',
  '+'  : 'add',
  '-'  : 'sub',
  '*'  : 'mul',
  '/'  : 'div',
  '%'  : 'rem',
  '==' : 'eq',
  '!=' : 'ne',
  '<'  : 'lt',
  '<=' : 'le',
  '>'  : 'gt',
  '>=' : 'ge',
  '++' : 'concat',
  '..' : 'range',
  '..=': 'rangex',
  '|>' : 'rpipe',
  '<|' : 'lpipe',
  '&&&': 'bitand',
  '|||': 'bitor',
  '~~~': 'bitnot',
  '^^^': 'bitxor',
  '>>>': 'bitshr',
  '<<<': 'bitshl',
}

const lexer = (source) => ({
  source,
  line: 1,
  curr: 0,
  prev: 0,
  tokens: [],
})

const token = (lexer, kind, value = null) => {
  lexer.tokens.push({
    kind,
    value,
    line: lexer.line,
    span: {
      start: lexer.prev,
      offset: lexer.curr,
    },
  })
  return lexer
}

const atEnd = (lexer) => {
  return lexer.curr >= lexer.source.length
}

const advance = (lexer) => {
  lexer.curr += 1
  return lexer
}

const newline = (lexer) => {
  lexer.line += 1
  return advance(lexer)
}

const current = (lexer) => {
  return lexer.source[lexer.curr]
}

const peek = (lexer) => {
  return lexer.source[lexer.curr + 1]
}

const slice = (lexer) => {
  return lexer.source.slice(lexer.prev, lexer.curr)
}

const numeric = (char) => {
  return char >= '0' && char <= '9'
}

const lower = (char) => {
  return char >= 'a' && char <= 'z' || char === '_'
}

const upper = (char) => {
  return char >= 'A' && char <= 'Z'
}

const alpha = (char) => {
  return lower(char) || upper(char)
}

const alphanumeric = (char) => {
  return lower(char) || upper(char) || numeric(char)
}

const symbolic = (char) => {
  return SYMBOLS.includes(char)
}

const error = (lexer, msg) => {
  throw `Error [${lexer.line}]: ${msg}`
}

const comment = (lexer) => {
  while (current(lexer) !== '\n' && !atEnd(lexer)) {
    advance(lexer)
  }
  return lexer
}

const number = (lexer) => {
  let float = false
  while (numeric(current(lexer))) {
    advance(lexer)
    if (current(lexer) === '.' && !float) {
      float = true
      advance(lexer)
    }
  }
  return token(lexer, 'number', slice(lexer))
}

const string = (lexer) => {
  advance(lexer)
  while (current(lexer) !== '"') {
    advance(lexer)

    if (current(lexer) === '\n' || atEnd(lexer)) {
      return error('Unterminated end of string')
    }
  }
  advance(lexer)

  return token(lexer, 'string', slice(lexer))
}

const ident = (lexer) => {
  while (alphanumeric(current(lexer))) {
    advance(lexer)
  }

  const identifier = slice(lexer)

  if (keywords.has(identifier)) {
    return token(lexer, identifier)
  } else {
    return token(lexer, upper(identifier[0]) ? 'upper' : 'lower', identifier)
  }
}

const operator = (lexer) => {
  while (symbolic(current(lexer))) {
    advance(lexer)
  }

  const operator = operators[slice(lexer)]

  if (operator) {
    return token(lexer, operator)
  } else {
    return error(lexer, `Invalid operator '${slice(lexer)}'`)
  }

}

const next = (lexer) => {
  const char = current(lexer)

  switch (char) {
    case ' ':
    case '\t':
    case '\r': return advance(lexer)
    case '\n': return newline(lexer)
    case '(': return token(advance(lexer), 'lparen')
    case ')': return token(advance(lexer), 'rparen')
    case '{': return token(advance(lexer), 'lbrace')
    case '}': return token(advance(lexer), 'rbrace')
    case '[': return token(advance(lexer), 'lbracket')
    case ']': return token(advance(lexer), 'rbracket')
    case ',': return token(advance(lexer), 'comma')
    case ';': return token(advance(lexer), 'semicolon')
    case '"': return string(lexer)
    case '/':
      if (peek(lexer) === '/') {
        return comment(lexer)
      } else {
        return operator(lexer)
      }
    case '-':
    case '+':
      if (numeric(peek(lexer))) {
        return number(advance(lexer))
      } else {
        return operator(lexer)
      }
    default:
      if (alpha(char)) {
        return ident(lexer)
      } else if (numeric(char)) {
        return number(lexer)
      } else if (symbolic(char)) {
        return operator(lexer)
      } else {
        return error(lexer, `Invalid character '${slice(advance(lexer))}'`)
      }
  }
}


const lex = lexer(`
hello() =
  print("Hello, World")


greet() = {
  print("Please tell me your name ")
  name = read()
  print("Nice to meet you, $name !")
}


fizzbuzz(num) =
  if zero(num % 15)
  then
    "FizzBuzz"
  else if zero(num % 3)
  then
    "Fizz"
  else if zero(num % 5)
  then
    "Buzz"
  else
    string(num)


mandelbrot() = {
  xsize = 59
  ysize = 21
  minI = -1.0
  maxI =  1.0
  minR = -2.0
  maxR =  1.0
  stepX = (maxR - minR) / xsize
  stepY = (maxI - minI) / ysize

  each(0 .. ysize, y => {
    im = minI + stepY * y
    print(map(0 .. xsize, x => {
      re = minR + stepX * x
      char(62 - loop(0, re, im, re, im, re * re + im * im))
    }))
  })

}

loop(n, r, i, zr, zi, v) =
  if n >= 30
  then
    n
  else if v > 4.0
  then
    n - 1
  else do {
    a = zr * zr
    b = zi * zi
    loop(n + 1, r, i, a - b + r, 2 * zr * zi + i, a + b)
  }
`)

while (!atEnd(lex)) {
  next(lex)
  lex.prev = lex.curr
}

console.log(lex.tokens)
