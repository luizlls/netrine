const { operatorInfo, defaultToken } = require('./token')

const parser = (tokens) => ({
  tokens,
  token: defaultToken(),
  peek : defaultToken(),
  prev : defaultToken(),
  index: 0,
  nodes: [],
})

const node = (kind, props, span) => {
  return { kind, ...props, span }
}

const identifier = (parser) => {
  const { value, span } = eat(parser, 'lower')
  return node('Name', { value }, span)
}

const parseExpr = (parser) => {
  switch (parser.token.kind) {
    case 'do':
      return parseDo(parser)
    case 'if':
      return parseIf(parser)
    case 'native':
      return parseNative(parser)
    default:
      return parseBinary(parser, 0)
  }
}

const parseTerm = (parser, apply = true) => {
  let value
  switch (parser.token.kind) {
    case 'lower':
      value = identifier(parser); break
    case 'lparen':
      value = parseParens(parser); break
    case 'lbrace':
      value = parseRecord(parser); break
    case 'lbracket':
      value = parseList(parser); break
    case 'upper':
      value = parseUpper(parser); break
    case 'number':
      value = parseNumber(parser); break
    case 'string':
      value = parseString(parser, 'string'); break
    case 'string start':
      value = parseTemplate(parser); break
    default:
      const { kind, span } = parser.token

      if (operatorInfo[kind]) {
        value = parseUnary(parser); break
      } else {
        return error(parser, span, `unexpected ${kind}`)
      }
  }

  if (!apply) {
    switch (parser.token.kind) {
      case 'dot':
        return parseGet(parser, value)
      default:
        return value
    }
  }

  loop:
  while (!done(parser)) {
    switch (parser.token.kind) {
      case 'equals':
        bump(parser)
        return parseDef(parser, value); break
      case 'walrus':
        bump(parser)
        return parseSet(parser, value); break
      case 'dot':
        value = parseGet(parser, value); break
      default:
        break loop
    }
  }

  return parseApply(parser, value)
}

const parseDef = (parser, patt) => {
  const mutable = maybeEat(parser, 'mut')
  const value   = parseExpr(parser)
  return node('Def', { patt, mutable, value }, span(parser, patt.span))
}

const parseSet = (parser, target) => {
  const value = parseExpr(parser)
  return node('Set', { target, value }, span(parser, target.span))
}

const parseGet = (parser, expr) => {
  while (maybeEat(parser, 'dot')) {
    let name
    let index
    if (matches(parser, 'lbracket')) {
      eat(parser, 'lbracket')
      index = parseExpr(parser)
      eat(parser, 'rbracket')
    } else {
      name = identifier(parser)
    }
    expr = node('Get', { expr, name, index }, span(parser, expr.span))
  }
  return expr
}

const parseFn = (parser, params) => {
  eat(parser, 'arrow')

  params.forEach(param => {
    switch (param.kind) {
      case 'Name':
      case 'Unit':
      case 'Tuple':
      case 'List':
      case 'Record':
      case 'Symbol':
      case 'String':
      case 'Number': break;
      default:
        return error(parser, param.span, 'invalid pattern for a function parameter')
    }
  })

  const value = parseExpr(parser)

  return node('Fn', { params, value }, span(parser, params[0].span))
}

const parseUpper = (parser) => {
  const { value, span } = eat(parser, 'upper')

  const name = node('Name', { value }, span)

  if (matchLines(parser) && matches(parser, 'dot')) {
    return parseGet(parser, name)
  }

  const values = parseTerms(parser)
  return node('Symbol', { name, values }, span(parser, span))
}

const parseTerms = (parser) => {
  const terms = []

  loop:
  while (matchLines(parser)) {
    switch (parser.token.kind) {
      case 'lower':
      case 'upper':
      case 'number':
      case 'string':
      case 'string start':
      case 'lparen':
      case 'lbrace':
      case 'lbracket': {
        terms.push(parseTerm(parser, false))
        break
      }
      default:
        break loop
    }
  }

  return terms
}

const parseApply = (parser, callee) => {
  const args = parseTerms(parser)

  if (matchLines(parser) && matches(parser, 'arrow')) {
    return parseFn(parser, [callee, ...args])
  }

  if (args.length === 0) {
    return callee
  } else {
    return args
      .reduce((fn, arg) => node('Apply', { fn, arg }, arg.span), callee)
  }
}

const parseNumber = (parser) => {
  const { value, span } = eat(parser, 'number')
  return node('Number', { value }, span)
}

const parseTemplate = (parser) => {
  const start = parser.token.span

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

  return node('Template', { elements }, span(parser, start))
}

const parseString = (parser, kind) => {
  const { value: raw, span } = eat(parser, kind)

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
  return node('String', { value, raw }, span)
}

const parseParens = (parser) => {
  const start = parser.token.span
  const items = blockOf(parser, 'lparen', 'rparen', parseExpr)

  if (items.length === 0) {
    return node('Unit', { }, span(parser, start))
  }
  if (items.length === 1) {
    return items.pop()
  }
  return node('Tuple', { items }, span(parser, start))
}

const parseList = (parser) => {
  const start = parser.token.span
  const items = blockOf(parser, 'lbracket', 'rbracket', parseExpr)

  return node('List',  { items }, span(parser, start))
}

const parseRecord = (parser) => {
  const start = parser.token.span
  const properties = blockOf(parser, 'lbrace', 'rbrace', parseProperty)

  return node('Record', { properties }, span(parser, start))
}

const parseProperty = (parser) => {
  const name = identifier(parser)

  if (!matches(parser, 'comma')
   && !matches(parser, 'rbrace')
   && !matchLines(parser)) {
    return error(parser, parser.token.span, 'the property name and value must start in the same line')
  }

  if (matches(parser, 'colon')) {
    bump(parser)
    return { name, value: parseExpr(parser) }
  } else {
    return { name, value: name }
  }
}

const parseUnary = (parser) => {
  const { kind, span: start } = parser.token

  const operator = node('Name', { value: kind }, start)

  if (parser.prev.kind === 'lparen'
  &&  parser.peek.kind === 'rparen') {
    bump(parser)
    return operator
  }

  bump(parser)

  const rhs = parseTerm(parser)
  return node('Unary', { operator, rhs }, span(parser, start))
}

const parseBinary = (parser, minPrecedence) => {
  let expr = parseTerm(parser)

  while (!done(parser)) {
    const { kind, span: start } = parser.token

    const info = operatorInfo[kind]

    if (info == null) {
      break;
    }

    const { precedence, associativity } = info;

    if (precedence == null) {
      return error(parser, start, `'${kind}' is not a valid binary (infix) operator`)
    }

    if (precedence < minPrecedence) {
        break;
    }

    const operator = node('Name', { value: kind }, start)

    bump(parser)

    let fix = 0
    switch (associativity) {
        case 'right':
          fix = 0; break
        case 'left':
        case 'none':
          fix = 1; break
    }

    const rhs = parseBinary(parser, precedence + fix)
    const lhs = expr

    expr = node('Binary', { operator, lhs, rhs }, span(parser, operator.span))
  }

  return expr
}

const parseDo = (parser) => {
  const start = parser.token.span
  const items = []

  eat(parser, 'do')

  while (!done(parser)) {
    items.push(parseExpr(parser))
    if (!maybeEat(parser, 'semi')) {
      break;
    }
  }

  return node('Block', { items }, span(parser, start))
}

const parseIf = (parser) => {
  const start = parser.token.span
  eat(parser, 'if')
  const test = parseExpr(parser)
  eat(parser, 'then')
  const then = parseExpr(parser)
  eat(parser, 'else')
  const otherwise = parseExpr(parser)

  return node('If', { test, then, otherwise }, span(parser, start))
}

const blockOf = (parser, open, close, fn) => {
  const result = []

  eat(parser, open)

  while (!matches(parser, close)) {
    result.push(fn(parser))
    if (!maybeEat(parser, 'comma')) {
      break;
    }
  }

  eat(parser, close)

  return result
}

const span = (parser, start, end = null) => {
  end = end || parser.prev.span
  return { ...start, offset: end.offset }
}

const matchLines = (parser) => {
  return parser.token.span.lineno === parser.prev.span.lineno
}

const maybeEat = (parser, kind) => {
  if (matches(parser, kind)) {
    bump(parser)
    return true
  } else {
    return false
  }
}

const eat = (parser, expected) => {
  if (matches(parser, expected)) {
    bump(parser)
    return parser.prev
  } else {
    const { kind, span } = parser.token
    return error(parser, span, `expected '${expected}', but found '${kind}'`)
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

const matches = (parser, kind) => {
  return parser.token.kind === kind
}

const error = (parser, span, msg) => {
  throw `Error [${span.lineno}]: ${msg}`
}

exports.parse = (tokens) => {
  const pp = parser(tokens)
  bump(pp) // initialize

  const nodes = []

  while (!done(pp)) { nodes.push(parseExpr(pp)) }

  return { nodes }
}
