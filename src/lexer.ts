import { Token, Kind, keywords, operators } from './token'

const SYMBOLS = '.%^&|:=~+-*<>!/'

interface Lexer {
  source: string
  line: number
  prev: number
  curr: number
  tokens: Token[]
}

const lexer = (source: string) => ({
  source,
  line: 1,
  curr: 0,
  prev: 0,
  tokens: [],
}) as Lexer

const token = (lexer: Lexer, kind: Kind, value?: string) => {
  lexer.tokens.push({
    kind,
    value,
    span: {
      lineno: lexer.line,
      start: lexer.prev,
      offset: lexer.curr,
    }
  })
  return lexer
}

const atEnd = (lexer: Lexer) => {
  return lexer.curr >= lexer.source.length
}

const bump = (lexer: Lexer) => {
  lexer.curr += 1
  return lexer
}

const line = (lexer: Lexer) => {
  lexer.line += 1
  return bump(lexer)
}

const current = (lexer: Lexer) => {
  return lexer.source[lexer.curr]
}

const peek = (lexer: Lexer) => {
  return lexer.source[lexer.curr + 1]
}

const slice = (lexer: Lexer) => {
  return lexer.source.slice(lexer.prev, lexer.curr)
}

const numeric = (char: string) => {
  return char >= '0' && char <= '9'
}

const lower = (char: string) => {
  return char >= 'a' && char <= 'z' || char === '_'
}

const upper = (char: string) => {
  return char >= 'A' && char <= 'Z'
}

const alpha = (char: string) => {
  return lower(char) || upper(char)
}

const alphanumeric = (char: string) => {
  return lower(char) || upper(char) || numeric(char)
}

const symbolic = (char: string) => {
  return SYMBOLS.includes(char)
}

const error = (lexer: Lexer, msg: string) => {
  throw `Error [${lexer.line}]: ${msg}`
}

const comment = (lexer: Lexer) => {
  while (current(lexer) !== '\n' && !atEnd(lexer)) {
    bump(lexer)
  }
  return lexer
}

const number = (lexer: Lexer) => {
  let float = false
  while (numeric(current(lexer))) {
    bump(lexer)
    if (current(lexer) === '.' && !float) {
      float = true
      bump(lexer)
    }
  }
  return token(lexer, 'number', slice(lexer))
}

const string = (lexer: Lexer) => {
  bump(lexer)
  while (current(lexer) !== '"') {
    bump(lexer)

    if (current(lexer) === '\n' || atEnd(lexer)) {
      return error(lexer, 'Unterminated string')
    }
  }
  bump(lexer)

  return token(lexer, 'string', slice(lexer))
}

const identifier = (lexer: Lexer) => {
  while (alphanumeric(current(lexer))) {
    bump(lexer)
  }

  switch (current(lexer)) {
    case '?':
    case '!': {
      bump(lexer)
    }
  }

  const ident = slice(lexer)

  const keyword = keywords[ident]

  if (keyword) {
    return token(lexer, keyword)
  } else {
    return token(lexer, upper(ident[0]) ? 'upper' : 'lower', ident)
  }
}

const operator = (lexer: Lexer) => {
  while (symbolic(current(lexer))) {
    bump(lexer)
  }

  const operator = operators[slice(lexer)]

  if (operator) {
    return token(lexer, operator)
  } else {
    return error(lexer, `Invalid operator '${slice(lexer)}'`)
  }

}

const next = (lexer: Lexer) => {
  lexer.prev = lexer.curr

  const char = current(lexer)

  switch (char) {
    case ' ':
    case '\t':
    case '\r': return bump(lexer)
    case '\n': return line(lexer)
    case '(':  return token(bump(lexer), 'lparen')
    case ')':  return token(bump(lexer), 'rparen')
    case '{':  return token(bump(lexer), 'lbrace')
    case '}':  return token(bump(lexer), 'rbrace')
    case '[':  return token(bump(lexer), 'lbracket')
    case ']':  return token(bump(lexer), 'rbracket')
    case ';':  return token(bump(lexer), 'semi')
    case ',':  return token(bump(lexer), 'comma')
    case '"':  return string(lexer)
    case '/':
      if (peek(lexer) === '/') {
        return comment(lexer)
      } else {
        return operator(lexer)
      }
    default:
      if (alpha(char)) {
        return identifier(lexer)
      } else if (numeric(char)) {
        return number(lexer)
      } else if (symbolic(char)) {
        return operator(lexer)
      } else {
        bump(lexer)
        return error(lexer, `Invalid character '${slice(lexer)}'`)
      }
  }
}

export const tokenize = (source: string): Token[] => {
  const tokenizer = lexer(source)

  while (!atEnd(tokenizer)) { next(tokenizer) }

  return tokenizer.tokens
}
