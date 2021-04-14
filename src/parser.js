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

const identifier = (parser) => {
  const { value, meta } = eat(parser, 'lower')
  return node('Name', value, meta)
}

const parseExpr = (parser) => {
  return parseBinary(parser, 0)
}

const parseTerm = (parser, apply = true) => {
  let value
  switch (parser.token.kind) {
    case 'lower':
      value = identifier(parser); break
    case 'lparen':
      value = parseParens(parser); break
    case 'lbracket':
      value = parseBrackets(parser); break
    case 'lbrace':
      value = parseBlock(parser); break
    case 'upper':
      value = parseUpper(parser); break
    case 'number':
      value = parseNumber(parser); break
    case 'string':
      value = parseString(parser, 'string'); break
    case 'string start':
      value = parseTemplate(parser); break
    default:
      const { kind, meta } = parser.token

      if (operatorInfo[kind]) {
        value = parseUnary(parser); break
      } else {
        return error(parser, meta, `unexpected ${kind}`)
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

const parseDef = (parser, name) => {
  const mutable = maybeEat(parser, 'mut')
  const value = parseExpr(parser)
  const meta  = { mutable, ...name.meta }
  return expr('Def', [name, value], span(parser, meta))
}

const parseSet = (parser, target) => {
  const value = parseExpr(parser)
  return expr('Set', [target, value], span(parser, target.meta))
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
      name = identifier(parser)
    }
    expr = expr('Get', [expr, name || index], span(parser, expr.meta))
  }
  return expr
}

const parseUpper = (parser) => {
  const { value, meta } = eat(parser, 'upper')

  const name = node('Name', value, meta)

  if (matchLines(parser) && tokenIs(parser, 'dot')) {
    return parseGet(parser, name)
  }

  const values = parseArgs(parser, false)
  return expr('Symbol', [name, values], span(parser, meta))
}

const parseArgs = (parser, allowKeywords = true) => {
  const args = []

  loop:
  while (matchLines(parser) || (allowKeywords && peekIsKeyword(parser))) {

    let keyword
    if (peekIsKeyword(parser)) {
      keyword = identifier(parser)
      bump(parser) // :
    }

    switch (parser.token.kind) {
      case 'lower':
      case 'upper':
      case 'number':
      case 'string':
      case 'string start':
      case 'lparen':
      case 'lbrace':
      case 'lbracket': {
        const expr = parseTerm(parser, false)

        if (keyword) {
          expr.meta = { keyword, ...keyword.span }
          args.push(expr)
        } else {
          args.push(expr)
        }

        break
      }
      default:
        break loop
    }
  }

  return args
}

const peekIsKeyword = (parser) => {
  return tokenIs(parser, 'parser')
      && peekIs(parser, 'colon')
      && parser.token.meta.line === parser.peek.meta.line
}

const parseApply = (parser, fn) => {
  const args = parseArgs(parser)

  if (args.length === 0) {
    return fn
  } else {
    return expr('Apply', [fn, ...args], fn.meta)
  }
}

const parseNumber = (parser) => {
  const { value, meta } = eat(parser, 'number')
  return node('Number', value, meta)
}

const parseTemplate = (parser) => {
  const { meta } = parser.token

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

  return expr('Template', elements, span(parser, meta))
}

const parseString = (parser, kind) => {
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
  return node('String', value, { raw, ...meta })
}

const parseParens = (parser) => {
  const { meta } = parser.token
  const items = blockOf(parser, 'lparen', 'rparen', parseExpr)

  if (items.length === 0) {
    return node('Unit', undefined, span(parser, meta))
  }
  if (items.length === 1) {
    return items.pop()
  }
  return node('Tuple', items, span(parser, meta))
}

const parseBrackets = (parser) => {
  const { meta } = parser.token

  let items = []
  let kind

  eat(parser, 'lbracket')

  if (tokenIs(parser, 'colon') && peekIs(parser, 'rbracket')) {
    kind = 'Dict'
    bump(parser) // :
  }

  while (!done(parser)) {
    if (tokenIs(parser, 'rbracket')) {
      break
    }

    let key = parseExpr(parser)

    if (kind === undefined) {
      if (tokenIs(parser, 'colon') && matchLines(parser)) {
        kind = 'Dict'
      } else {
        kind = 'List'
      }
    }

    let value = key
    if (kind === 'Dict') {
      eat(parser, 'colon')
      value = parseExpr(parser)
    }

    if (kind === 'Dict') {
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
  const { meta } = parser.token
  const items = []

  eat(parser, 'lbrace')
  while (!done(parser)) {
    if (tokenIs(parser, 'rbrace')) {
      break
    }
    items.push(parseExpr(parser))
  }
  eat(parser, 'rbrace')

  return expr('Block', items, span(parser, meta))
}

const parseUnary = (parser) => {
  const { kind, meta } = parser.token

  const operator = node('Name', kind, meta)

  if (parser.prev.kind === 'lparen'
  &&  parser.peek.kind === 'rparen') {
    bump(parser)
    return operator
  }

  const info = operatorInfo[kind]

  if (info === undefined || info.precedence !== undefined) {
    return error(parser, meta, `'${kind}' is not a valid unary (prefix) operator`)
  }

  bump(parser)

  const rhs = parseTerm(parser)
  return expr('Apply', [operator, rhs], span(parser, meta))
}

const parseBinary = (parser, minPrecedence) => {
  let expr = parseTerm(parser)

  while (!done(parser)) {
    const { kind, meta } = parser.token

    const info = operatorInfo[kind]

    if (info == null) {
      break;
    }

    const { precedence, associativity } = info;

    if (precedence == null) {
      return error(parser, meta, `'${kind}' is not a valid binary (infix) operator`)
    }

    if (precedence < minPrecedence) {
        break;
    }

    const operator = node('Name', kind, meta)

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

    expr = expr('Apply', [operator, lhs, rhs], span(parser, lhs.meta))
  }

  return expr
}

const blockOf = (parser, open, close, fn) => {
  const result = []

  eat(parser, open)

  while (!tokenIs(parser, close)) {
    result.push(fn(parser))
    if (!maybeEat(parser, 'comma')) {
      break;
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
