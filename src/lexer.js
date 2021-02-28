const { keywords, operators } = require('./token')

const SYMBOLS = '.%^&|:=~+-*<>!/'

const lexer = (source) => ({
  source,
  line: 1,
  curr: 0,
  prev: 0,
  mode: 'regular',
  tokens: [],
})

const token = (lexer, kind, value = null) => {
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

const done = (lexer) => {
  return lexer.curr >= lexer.source.length
}

const bump = (lexer) => {
  lexer.curr += 1
  return lexer
}

const line = (lexer) => {
  lexer.line += 1
  return bump(lexer)
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
  while (current(lexer) !== '\n' && !done(lexer)) {
    bump(lexer)
  }
  return lexer
}

const number = (lexer, prefix = false) => {
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

const identifier = (lexer) => {
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

const special = (lexer) => {
  bump(lexer)

  while (alphanumeric(current(lexer))) {
    bump(lexer)
  }

  switch (slice(lexer)) {
    case '@js':
      return 'native'
    default:
      return error(lexer, `invalid special form ${slice(lexer)}`)
  }
}

const operator = (lexer) => {
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

const next = (lexer) => {
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
    case '@':
      return special(lexer)
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

const template = (lexer, start) => {
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

exports.tokenize = (source) => {
  const tokenizer = lexer(source)

  while (!done(tokenizer)) { next(tokenizer) }

  return tokenizer.tokens
}
