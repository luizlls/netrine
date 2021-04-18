const { operatorInfo, defaultToken } = require('./token')

const parser = (tokens) => ({
  tokens,
  token: defaultToken(),
  peek : defaultToken(),
  prev : defaultToken(),
  index: 0,
  nodes: [],
})

const node = (kind, value, meta) => {
  return { kind, value, meta }
}

const expr = (kind, args, meta) => {
  args = Array.isArray(args) ? args : [args]
  return { kind, args, meta }
}

const parseIdent = (parser) => {
  const { value, meta } = eat(parser, 'lower')
  return node('name', value, meta)
}

const parseExpr = (parser) => {
  if (tokenIs(parser, 'case')) {
    return parseCase(parser)
  } else if (operatorInfo[parser.token.kind] !== undefined) {
    return parseUnary(parser)
  } else if (startTerm(parser)) {
    const term = parseTerm(parser)
    if (term.kind === 'name') {
      switch (parser.token.kind) {
        case 'equals':
          return parseDef(parser, term)
        case 'walrus':
          return parseSet(parser, term)
        default:
          if (startTerm(parser) && matchLines(parser)) {
            return parseApply(parser, term)
          }
          break
      }
    }
    if (operatorInfo[parser.token.kind] !== undefined) {
      return parseBinary(parser, 0, term)
    } else {
      return term
    }
  } else {
    return error(parser, parser.token.meta, `Unexpected ${parser.token.kind}`)
  }
}

const startTerm = (parser) => {
  switch (parser.token.kind) {
    case 'lower':
    case 'upper':
    case 'lparen':
    case 'lbracket':
    case 'lbrace':
    case 'number':
    case 'string':
    case 'string start':
    case 'kwarg':
    case 'wildcard':
      return true
    default:
      return false
  }
}

const parseTerm = (parser) => {
  let value
  switch (parser.token.kind) {
    case 'lower':
      value = parseIdent(parser); break
    case 'upper':
      value = parseUpper(parser); break
    case 'lparen':
      value = parseParens(parser); break
    case 'lbracket':
      value = parseBrackets(parser); break
    case 'lbrace':
      value = parseBlock(parser); break
    case 'number':
      value = parseNumber(parser); break
    case 'string':
      value = parseString(parser); break
    case 'string start':
      value = parseTemplate(parser); break
    case 'kwarg':
      value = parseKwArg(parser); break
    case 'wildcard':
      value = parseWildcard(parser); break
    default:
      const { kind, meta } = parser.token
      return error(parser, meta, `unexpected ${kind}`)
  }

  if (tokenIs(parser, 'dot')) {
    value = parseGet(parser, value)
  }

  return value
}

const parseDef = (parser, name) => {
  eat(parser, 'equals')
  const mutable = maybeEat(parser, 'mut')
  const value = parseExpr(parser)
  const meta  = { mutable, ...name.meta }
  return expr('def', [name, value], span(parser, meta))
}

const parseSet = (parser, target) => {
  eat(parser, 'walrus')
  const value = parseExpr(parser)
  return expr('set', [target, value], span(parser, target.meta))
}

const parseGet = (parser, expr) => {
  while (maybeEat(parser, 'dot')) {
    let name
    let index
    if (tokenIs(parser, 'lbracket')) {
      eat(parser, 'lbracket')
      index = parseExpr(parser)
      eat(parser, 'rbracket')
    } else {
      name = parseIdent(parser)
    }
    expr = expr('get', [expr, name || index], span(parser, expr.meta))
  }
  return expr
}

const parseUpper = (parser) => {
  const { value, meta } = eat(parser, 'upper')

  const name = node('name', value, meta)

  if (matchLines(parser) && tokenIs(parser, 'dot')) {
    return parseGet(parser, name)
  }

  const values = parseArgs(parser)
  return expr('symbol', [name, values], span(parser, meta))
}

const parseArgs = (parser) => {
  const args = []

  while (startTerm(parser) && matchLines(parser)) {
    const kwarg = prevIs(parser, 'kwarg')
    if (kwarg) {
      args.push(parseExpr(parser))
    } else {
      args.push(parseTerm(parser))
    }
  }

  return args
}

const parseApply = (parser, fn) => {
  const args = parseArgs(parser)
  return expr('apply', [fn, ...args], span(parser, fn.meta))
}

const parseUnary = (parser) => {
  const { kind, meta } = parser.token

  const operator = node('name', kind, meta)

  if (prevIs(parser, 'lparen')
  &&  peekIs(parser, 'rparen')) {
    bump(parser)
    return operator
  }

  const info = operatorInfo[kind]

  if (info === undefined || info.precedence !== undefined) {
    return error(parser, meta, `'${kind}' is not a valid unary (prefix) operator`)
  }

  bump(parser)

  return expr('apply', [operator, parseTerm(parser)], span(parser, meta))
}

const parseBinary = (parser, minimum, lhs) => {
  if (lhs == null) {
      lhs = parseExpr(parser)
  }

  while (!done(parser)) {
    const { kind, meta } = parser.token

    if (operatorInfo[kind] === undefined) {
      break
    }

    const { precedence,
            associativity } = operatorInfo[kind]

    if (precedence == null) {
      return error(parser, meta, `'${kind}' is not a valid binary (infix) operator`)
    }

    if (precedence < minimum) {
        break
    }

    const operator = node('name', kind, meta)

    bump(parser)

    let fix = 0
    switch (associativity) {
        case 'right':
          fix = 0; break
        case 'left':
        case 'none':
          fix = 1; break
    }

    const _rhs = parseBinary(parser, precedence + fix)
    const _lhs = lhs

    lhs = expr('apply', [operator, _lhs, _rhs], span(parser, lhs.meta))
  }

  return lhs
}

const parseNumber = (parser) => {
  const { value, meta } = eat(parser, 'number')
  return node('number', value, meta)
}

const parseTemplate = (parser) => {
  const meta = parser.token.meta

  const parts = []

  loop:
  while (!done(parser)) {
    switch (parser.token.kind) {
      case 'string finish': {
        parts.push(parseString(parser, 'string finish'))
        break loop
      }
      case 'string start': {
        parts.push(parseString(parser, 'string start'))
        break
      }
      case 'string fragment': {
        parts.push(parseString(parser, 'string fragment'))
        break
      }
      case 'lbrace': {
        eat(parser, 'lbrace')
        parts.push(parseExpr(parser))
        eat(parser, 'rbrace')
        break
      }
      default:
        break loop
    }
  }

  const elements = parts.filter(part => {
    if (part.kind === 'String') {
      return part.value.length > 0
    } else {
      return true
    }
  })

  return expr('template', elements, span(parser, meta))
}

const parseString = (parser, kind = 'string') => {
  const { value: raw, meta } = eat(parser, kind)

  let value = raw
  switch (kind) {
    case 'string': {
      value = raw.slice(1, raw.length - 1)
      break
    }
    case 'string start': {
      value = raw.slice(1)
      break
    }
    case 'string finish': {
      value = raw.slice(0, raw.length - 1)
      break
    }
    default:
      value = raw
  }
  return node('string', value, { raw, ...meta })
}

const parseKwArg = (parser) => {
  const { value, meta } = parser.token
  eat(parser, 'kwarg')
  return node('kwarg', value, meta)
}

const parseWildcard = (parser) => {
  const meta = parser.token.meta
  eat(parser, 'wildcard')
  return node('wildcard', '_', meta)
}

const parseParens = (parser) => {
  const meta = parser.token.meta
  const items = blockOf(parser, 'lparen', 'rparen', parseExpr)

  switch (items.length) {
    case 0:
      return node('unit', {}, span(parser, meta))
    case 1:
      return expr('group', items, span(parser, meta))
    default:
      return expr('tuple', items, span(parser, meta))
  }
}

const parseBrackets = (parser) => {
  const meta = parser.token.meta

  let items = []
  let kind

  eat(parser, 'lbracket')

  if (tokenIs(parser, 'arrow') && peekIs(parser, 'rbracket')) {
    kind = 'Dict'
    bump(parser) // :
  }

  while (!done(parser)) {
    if (tokenIs(parser, 'rbracket')) {
      break
    }

    let key = parseExpr(parser)

    if (kind === undefined) {
      if (tokenIs(parser, 'arrow') && matchLines(parser)) {
        kind = 'dict'
      } else {
        kind = 'list'
      }
    }

    let value = key
    if (kind === 'dict') {
      eat(parser, 'arrow')
      value = parseExpr(parser)
    }

    if (kind === 'dict') {
      items.push([key, value])
    } else {
      items.push(value)
    }

    if (!maybeEat(parser, 'comma')) {
      break
    }
  }

  eat(parser, 'rbracket')

  return expr(kind, items, span(parser, meta))
}

const parseBlock = (parser) => {
  const meta = parser.token.meta
  const items = []

  eat(parser, 'lbrace')
  while (!done(parser)) {
    if (tokenIs(parser, 'rbrace')) {
      break
    }
    items.push(parseExpr(parser))
  }
  eat(parser, 'rbrace')

  return expr('block', items, span(parser, meta))
}

const parseCase = (parser) => {
  const meta = parser.token.meta
  eat(parser, 'case')
  const value = parseExpr(parser)
  eat(parser, 'of')
  const cases = []

  while (!done(parser)) {
    const test = parseTerm(parser)
    eat(parser, 'arrow')
    const then = parseExpr(parser)
    cases.push([test, then])
    if (!maybeEat(parser, 'pipe')) {
      break
    }
  }

  return expr('case', [value, ...cases], span(parser, meta))
}

const blockOf = (parser, open, close, fn) => {
  const result = []

  eat(parser, open)

  while (!tokenIs(parser, close)) {
    result.push(fn(parser))
    if (!maybeEat(parser, 'comma')) {
      break
    }
  }

  eat(parser, close)

  return result
}

const span = (parser, meta, end = null) => {
  end = end || parser.prev.meta.span
  return { ...meta, span: { ...meta.span, offset: end.offset } }
}

const matchLines = (parser) => {
  return parser.token.meta.line === parser.prev.meta.line
}

const maybeEat = (parser, kind) => {
  if (tokenIs(parser, kind)) {
    bump(parser)
    return true
  } else {
    return false
  }
}

const eat = (parser, expected) => {
  if (tokenIs(parser, expected)) {
    bump(parser)
    return parser.prev
  } else {
    const { kind, meta } = parser.token
    return error(parser, meta, `expected '${expected}', but found '${kind}'`)
  }
}

const done = (parser) => {
  return parser.token.kind === 'eof'
}

const bump = (parser) => {
  parser.prev  = parser.token
  parser.token = parser.tokens[parser.index]     || defaultToken()
  parser.peek  = parser.tokens[parser.index + 1] || defaultToken()
  parser.index += 1
  return parser
}

const tokenIs = (parser, kind) => {
  return parser.token.kind === kind
}

const prevIs = (parser, kind) => {
  return parser.prev.kind === kind
}

const peekIs = (parser, kind) => {
  return parser.peek.kind === kind
}

const error = (parser, meta, msg) => {
  throw `Error [${meta.line}]: ${msg}`
}

exports.parse = (tokens) => {
  const pp = parser(tokens)
  bump(pp) // initialize

  const nodes = []

  while (!done(pp)) { nodes.push(parseExpr(pp)) }

  return { nodes }
}
