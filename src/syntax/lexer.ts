import { Token, TType, tokens, keywords, symbols } from './token'

const SYMBOLS = '.%^&|:=~+-*<>!/'

interface Lexer {
  source: string
  line: number
  prev: number
  curr: number
  mode: Mode,
  tokens: Token[]
}

type Mode =
  'regular'
| 'string'
| 'template'

function lexer(source: string): Lexer {
  return {
    source,
    line: 1,
    curr: 0,
    prev: 0,
    mode: 'regular',
    tokens: [],
  }
}

function token(lexer: Lexer, type: TType, value?: string): Lexer {
  lexer.tokens.push({
    type,
    value,
    line: lexer.line,
    span: {
      start: lexer.prev,
      offset: lexer.curr - lexer.prev,
    }
  })
  return lexer
}

function bump(lexer: Lexer): Lexer {
  lexer.curr += 1
  return lexer
}

function line(lexer: Lexer): Lexer {
  lexer.line += 1
  return bump(lexer)
}

function char(lexer: Lexer) {
  return lexer.source[lexer.curr]
}

function peek(lexer: Lexer) {
  return lexer.source[lexer.curr + 1]
}

function slice(lexer: Lexer) {
  return lexer.source.slice(lexer.prev, lexer.curr)
}

function done(lexer: Lexer) {
  return lexer.curr >= lexer.source.length
}

function numeric(ch: string) {
  return ch >= '0' && ch <= '9'
}

function lower(ch: string) {
  return ch >= 'a' && ch <= 'z' || ch === '_'
}

function upper(ch: string) {
  return ch >= 'A' && ch <= 'Z'
}

function alpha(ch: string) {
  return lower(ch) || upper(ch)
}

function alphanumeric(ch: string) {
  return lower(ch) || upper(ch) || numeric(ch)
}

function symbolic(ch: string) {
  return SYMBOLS.includes(ch)
}

function space(lexer: Lexer): Lexer {
  let ch = char(lexer)
  while (ch === ' '
      || ch === '\r'
      || ch === '\t') {
    ch = char(bump(lexer))
  }
  return next(lexer)
}

function comment(lexer: Lexer): Lexer {
  while (char(lexer) !== '\n' && !done(lexer)) {
    bump(lexer)
  }
  return lexer
}

function number(lexer: Lexer, prefix = false): Lexer {
  if (prefix) {
    bump(lexer)
  }
  let float = false
  while (numeric(char(lexer))) {
    bump(lexer)
    if (char(lexer) === '.' && !float) {
      float = true
      bump(lexer)
    }
  }
  return token(lexer, tokens.number, slice(lexer))
}

function identifier(lexer: Lexer): Lexer {

  while (alphanumeric(char(lexer))) {
    bump(lexer)
  }

  switch (char(lexer)) {
    case '?':
    case '!': {
      bump(lexer)
    }
    break
    case '\'': {
      while (char(lexer) === '\'') {
        bump(lexer)
      }
    }
    break
  }

  const value = slice(lexer)

  const keyword = keywords[value]

  if (keyword) {
    return token(lexer, keyword)
  } else {
    return token(lexer, upper(value[0]) ? tokens.upper : tokens.lower, value)
  }
}

function wildcard(lexer: Lexer): Lexer {
  bump(lexer)
  return token(lexer, tokens.underscore)
}

function operator(lexer: Lexer): Lexer {
  while (symbolic(char(lexer))) {
    bump(lexer)
  }

  const operator = symbols[slice(lexer)]

  if (operator != undefined) {
    return token(lexer, operator)
  } else {
    return error(lexer, `Invalid operator '${slice(lexer)}'`)
  }
}

// TODO: collect the errors and handle lexer/parser recovery
function error(lexer: Lexer, msg: string): never {
  throw new Error(`${msg} at ${lexer.line}`)
}

function next(lexer: Lexer): Lexer {
  lexer.prev = lexer.curr

  if (lexer.mode === 'string') {
    return template(lexer, false)
  }

  const ch = char(lexer)

  switch (ch) {
    case ' ':
    case '\t':
    case '\r': 
      return space(lexer)
    case '\n':
      return line(lexer)
    case ',':
      return token(bump(lexer), tokens.comma)
    case ';':
      return token(bump(lexer), tokens.semicolon)
    case '(':
      return token(bump(lexer), tokens.lparen)
    case ')':
      return token(bump(lexer), tokens.rparen)
    case '[':
      return token(bump(lexer), tokens.lbracket)
    case ']':
      return token(bump(lexer), tokens.rbracket)
    case '{':
      return token(bump(lexer), tokens.lbrace)
    case '}': {
      if (lexer.mode === 'template') {
          lexer.mode = 'string'
      }
      return token(bump(lexer), tokens.rbrace)
    }
    case '"':
      return template(lexer, true)
    case '+':
    case '-':
      if (numeric(peek(lexer))) {
        return number(lexer, true)
      } else {
        return operator(lexer)
      }
    case '/':
      if (peek(lexer) === '/') {
        return comment(lexer)
      } else {
        return operator(lexer)
      }
    case '_':
      if (alpha(peek(lexer))) {
        return identifier(lexer)
      } else {
        return wildcard(lexer)
      }
    default: break
  }
  if (alpha(ch)) {
    return identifier(lexer)
  } else if (numeric(ch)) {
    return number(lexer)
  } else if (symbolic(ch)) {
    return operator(lexer)
  } else {
    bump(lexer)
    return error(lexer, `Invalid character '${slice(lexer)}'`)
  }
}

function template(lexer: Lexer, start: boolean): Lexer {
  lexer.prev = lexer.curr

  while (!done(lexer)) {
    switch (char(lexer)) {
      case '"': {
        if (lexer.mode === 'regular') {
          bump(lexer)
          lexer.mode = 'string'
        } else {
          lexer.mode = 'regular'
          bump(lexer)
          const kind = start ? tokens.string : tokens.stringFinish
          return token(lexer, kind, slice(lexer))
        }
      }
        break
      case '{': {
        if (peek(lexer) === '{') {
          bump(lexer)
          bump(lexer)
        } else {
          const kind = start ? tokens.stringStart : tokens.stringPart
          lexer.mode = 'template'
          return token(lexer, kind, slice(lexer))
        }
      }
        break
      case '\n':
        return error(lexer, 'Unterminated string')
      case '\\': {
        bump(lexer)
        switch (char(lexer)) {
          case 'n': case 'r':
          case 't': case 'v':
          case 'a': case 'b':
          case '"': case '0':
          case '\\': {
            bump(lexer)
          }
            break
          case 'u':
            return error(lexer, 'TODO Validate unicode escape')
          case 'x':
            return error(lexer, 'TODO Validate binary escape')
          default:
            return error(lexer, `Invalid escape character ${char(lexer)}`)
        }
      }
        break
      default:
        bump(lexer)
    }
  }

  return lexer
}

export function tokenize(source: string) {
  const tokenizer = lexer(source)

  while (!done(tokenizer)) { next(tokenizer) }

  return tokenizer.tokens
}
