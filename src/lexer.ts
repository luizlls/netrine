import { Token, TokenKind, keywords, operators } from './token'

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

const lexer = (source: string): Lexer => ({
  source,
  line: 1,
  curr: 0,
  prev: 0,
  mode: 'regular',
  tokens: [],
})

const token = (lexer: Lexer, kind: TokenKind, value?: string) => {
  lexer.tokens.push({
    kind,
    value,
    meta: {
      line: lexer.line,
      span: {
        start: lexer.prev,
        offset: lexer.curr,
      }
    }
  })
  return lexer
}

const done = (lexer: Lexer) => {
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
  while (current(lexer) !== '\n' && !done(lexer)) {
    bump(lexer)
  }
  return lexer
}

const number = (lexer: Lexer, prefix = false) => {
  if (prefix) {
    bump(lexer)
  }

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

const identifier = (lexer: Lexer) => {
  while (alphanumeric(current(lexer))) {
    bump(lexer)
  }

  switch (current(lexer)) {
    case '?':
    case '!': {
      bump(lexer)
    }
    break
  }

  const ident = slice(lexer)

  const keyword = keywords[ident]

  if (keyword) {
    return token(lexer, keyword)
  } else {
    return token(lexer, upper(ident[0]) ? 'upper' : 'lower', ident)
  }
}

const wildcard = (lexer: Lexer) => {
  bump(lexer)
  return token(lexer, 'any')
}

const operator = (lexer: Lexer) => {
  while (symbolic(current(lexer))) {
    bump(lexer)
  }

  const operator = operators[slice(lexer)]

  if (operator != undefined) {
    return token(lexer, operator)
  } else {
    return error(lexer, `Invalid operator '${slice(lexer)}'`)
  }
}

const next = (lexer: Lexer) => {
  lexer.prev = lexer.curr

  if (lexer.mode === 'string') {
    return template(lexer, false)
  }

  const char = current(lexer)

  switch (char) {
    case ' ':
    case '\t':
    case '\r': return bump(lexer)
    case '\n': return line(lexer)
    case ',':  return token(bump(lexer), 'comma')
    case ';':  return token(bump(lexer), 'semi')
    case '(':  return token(bump(lexer), 'lparen')
    case ')':  return token(bump(lexer), 'rparen')
    case '[':  return token(bump(lexer), 'lbracket')
    case ']':  return token(bump(lexer), 'rbracket')
    case '{':  return token(bump(lexer), 'lbrace')
    case '}':  {
      if (lexer.mode === 'template') {
          lexer.mode = 'string'
      }
      return token(bump(lexer), 'rbrace')
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

const template = (lexer: Lexer, start: boolean) => {
  lexer.prev = lexer.curr

  while (!done(lexer)) {
    switch (current(lexer)) {
      case '"': {
        if (lexer.mode === 'regular') {
          bump(lexer)
          lexer.mode = 'string'
        } else {
          lexer.mode = 'regular'
          bump(lexer)
          const kind = start ? 'string' : 'string finish'
          return token(lexer, kind, slice(lexer))
        }
      }
        break
      case '{': {
        if (peek(lexer) === '{') {
          bump(lexer)
          bump(lexer)
        } else {
          const kind = start ? 'string start' : 'string fragment'
          lexer.mode = 'template'
          return token(lexer, kind, slice(lexer))
        }
      }
        break
      case '\n':
        return error(lexer, 'Unterminated string')
      case '\\': {
        bump(lexer)
        switch (current(lexer)) {
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
            return error(lexer, `Invalid escape character ${current(lexer)}`)
        }
      }
        break
      default:
        bump(lexer)
    }
  }

}

export const tokenize = (source: string) => {
  const tokenizer = lexer(source)

  while (!done(tokenizer)) { next(tokenizer) }

  return tokenizer.tokens
}
