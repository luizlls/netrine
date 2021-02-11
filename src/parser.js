const AST = require('./ast')
const { operatorInfo, defaultToken } = require('./token')

const parser = (tokens) => ({
  tokens,
  token: defaultToken(),
  prev : defaultToken(),
  index: 0,
  nodes: [],
})

const definition = (parser) => {
  const start = parser.token.span

  const name = identifier(parser)

  let value
  switch (parser.token.kind) {
    case 'lparen':
      value = parseFun(parser); break;
    case 'equals': {
      bump(parser)
      value = parseExpr(parser); break;
    }
    default:
      return error(parser, 'Expected definition')
  }

  return AST.def(name, value, span(parser, start))
}

const identifier = (parser) => {
  const { value, span } = eat(parser, 'lower')
  return AST.name(value, span)
}

const parseFun = (parser) => {
  const start  = parser.token.span
  const params = blockOf(parser, 'lparen', 'rparen', identifier)
  eat(parser, 'equals')
  const value = parseExpr(parser)

  return AST.fn(params, value, span(parser, start))
}

const parseExpr = (parser) => {
  switch (parser.token.kind) {
    case 'fn':
      return parseFn(parser)
    case 'do':
      return parseDo(parser)
    case 'if':
      return parseIf(parser)
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
      if (operatorInfo[parser.token.kind]) {
        value = parseUnary(parser); break
      } else {
        return error(parser, `Unexpected ${parser.token.kind}`)
      }
  }

  switch (parser.token.kind) {
    case 'equals':
      value = parseLet(parser, value); break
    case 'walrus':
      value = parseMut(parser, value); break
    case 'dot':
      value = parseDot(parser, value); break
  }

  if (apply) {
    return parseApply(parser, value)
  } else {
    return value
  }
}

const parseLet = (parser, term) => {
  bump(parser)
  return AST.def(term, parseExpr(parser), span(parser, term.span))
}

const parseMut = (parser, term) => {
  bump(parser)
  return AST.mut(term, parseExpr(parser), span(parser, term.span))
}

const parseDot = (parser, term) => {
  while (maybeEat(parser, 'dot')) {
    term = AST.member(term, identifier(parser), span(parser, term.span))
  }
  return term
}

const parseUpper = (parser) => {
  const { value, span: start } = eat(parser, 'upper')

  const name = AST.name(value, start)

  if (matchLines(parser) && matches(parser, 'dot')) {
    return parseDot(parser, name)
  }

  return AST.symbol(name, parseArgs(parser), span(parser, start))
}

const parseArgs = (parser) => {
  const args = []

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
        args.push(parseTerm(parser, false))
        break
      }
      default:
        break loop
    }
  }

  return args
}

const parseApply = (parser, callee) => {
  const args = parseArgs(parser)

  if (args.length === 0) {
    return callee
  } else {
    return AST.apply(callee, args, span(parser, callee.span))
  }
}

const parseNumber = (parser) => {
  const { value, span } = eat(parser, 'number')
  return AST.number(value, span)
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

  return AST.template(elements, span(parser, start))
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
  return AST.string(value, raw, span)
}

const parseParens = (parser) => {
  const start = parser.token.span
  const items = blockOf(parser, 'lparen', 'rparen', parseExpr)

  if (items.length === 1) {
    const inner = items[0]
    return AST.group(inner, span(parser, start))
  } else {
    return AST.tuple(items, span(parser, start))
  }
}

const parseList = (parser) => {
  const start = parser.token.span
  const items = blockOf(parser, 'lbracket', 'rbracket', parseExpr)

  return AST.list(items, span(parser, start))
}

const parseRecord = (parser) => {
  const start = parser.token.span
  const props = blockOf(parser, 'lbrace', 'rbrace', parseProperty)

  return AST.record(props, span(parser, start))
}

const parseProperty = (parser) => {
  const name = identifier(parser)

  if (!matches(parser, 'comma')
   && !matches(parser, 'rbrace')
   && !matchLines(parser)) {
    return error(parser, 'The property name and value must start in the same line')
  }

  switch (parser.token.kind) {
    case 'equals': {
      bump(parser)
      return { name, value: parseExpr(parser) }
    }
    case 'lbrace':
      return { name, value: parseExpr(parser) }
    case 'lparen':
      return { name, value: parseFun(parser) }
    default:
      return { name }
  }
}

const parseUnary = (parser) => {
  const { kind, span } = parser.token

  const operator = AST.name(kind, span)

  if (parser.prev.kind === 'lparen' && peek(parser).kind === 'rparen') {
    bump(parser)
    return operator
  }

  bump(parser)

  if (matchLines(parser)) {
    return AST.unary(operator, parseTerm(parser, false), span(parser, span))
  } else {
    return operator
  }
}

const parseBinary = (parser, minPrecedence) => {
  let expr = parseTerm(parser)

  while (!done(parser)) {
    const info = operatorInfo[parser.token.kind]

    if (info == null) {
      break;
    }

    const { precedence, associativity } = info;

    if (precedence == null) {
      return error(parser, `'${parser.token.kind}' is not a valid binary (infix) operator`)
    }

    if (precedence < minPrecedence) {
        break;
    }

    const operator = AST.name(parser.token.kind, parser.token.span)

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

    expr = AST.binary(operator, lhs, rhs, span(parser, operator.span))
  }

  return expr
}

const parseFn = (parser) => {
  const start = parser.token.span
  const params = blockOf(parser, 'fn', 'arrow', identifier)
  const value = parseExpr(parser)

  return AST.fn(params, value, span(parser, start))
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

  return AST.block(items, span(parser, start))
}

const parseIf = (parser) => {
  const start = parser.token.span
  eat(parser, 'if')
  const test = parseExpr(parser)
  eat(parser, 'then')
  const then = parseExpr(parser)
  eat(parser, 'else')
  const otherwise = parseExpr(parser)

  return AST.cond(test, then, otherwise, span(parser, start))
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

const eat = (parser, kind) => {
  if (matches(parser, kind)) {
    bump(parser)
    return parser.prev
  } else {
    return error(parser, `Expected '${kind}', but found '${parser.token.kind}'`)
  }
}

const done = (parser) => {
  return parser.token.kind === 'eof'
}

const bump = (parser) => {
  parser.prev  = parser.token
  parser.token = parser.tokens[parser.index] || defaultToken()
  parser.index += 1
}

const matches = (parser, kind) => {
  return parser.token.kind === kind
}

const peek = (parser) => {
  return parser.tokens[parser.index] || defaultToken()
}

const error = (parser, msg) => {
  throw `Error [${parser.token.span.lineno}]: ${msg}`
}

exports.parse = (tokens) => {
  const pp = parser(tokens)
  bump(pp) // initialize

  const nodes = []

  while (!done(pp)) { nodes.push(definition(pp)) }

  return { nodes }
}
