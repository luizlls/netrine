const { operatorInfo, defaultToken } = require('./token')

const parser = (tokens) => ({
  tokens,
  token: defaultToken(),
  peek : defaultToken(),
  prev : defaultToken(),
  index: 0,
  nodes: [],
})

const node = (kind, args, meta) => {
  return { kind, ...args, meta }
}

const parseIdent = (parser) => {
  const { value, meta } = eat(parser, 'lower')
  return node('Name', { value }, meta)
}

const parseExpr = (parser) => {
  switch (parser.token.kind) {
    case 'fn':
      return parseFn(parser)
    case 'if':
      return parseIf(parser)
    case 'for':
      return parseFor(parser)
    case 'match':
      return parseMatch(parser)
    case 'mut':
      return parseMut(parser)
    default: {
      if (operatorInfo[parser.token.kind] !== undefined) {
        return parseUnary(parser)
      } else if (startTerm(parser)) {
        let value = parseTerm(parser)

        if (value.kind === 'Name') {
          switch (parser.token.kind) {
            case 'equals':
              return parseDef(parser, value)
            case 'walrus':
              return parseSet(parser, value)
            default:
              if (startTerm(parser) && matchLines(parser)) {
                value = parseApply(parser, value)
              }
              break
          }
        }

        if (operatorInfo[parser.token.kind] !== undefined) {
          return parseBinary(parser, 0, value)
        } else {
          return value
        }
      } else {
        return error(parser, parser.token.meta, `Unexpected ${parser.token.kind}`)
      }
    }
  }
}

const startTerm = (parser) => {
  switch (parser.token.kind) {
    case 'lower':
    case 'upper':
    case 'lparen':
    case 'lbracket':
    case 'number':
    case 'string':
    case 'string start':
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
    case 'number':
      value = parseNumber(parser); break
    case 'string':
      value = parseString(parser); break
    case 'string start':
      value = parseTemplate(parser); break
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
  const value = parseExpr(parser)
  return node('Def', { name, value }, span(parser, name.meta))
}

const parseSet = (parser, target) => {
  eat(parser, 'walrus')
  const value = parseExpr(parser)
  return node('Set', { target, value }, span(parser, target.meta))
}

const parseGet = (parser, main) => {
  while (maybeEat(parser, 'dot')) {
    let name, index
    if (tokenIs(parser, 'lbracket')) {
      eat(parser, 'lbracket')
      index = parseExpr(parser)
      eat(parser, 'rbracket')
    } else {
      name = parseIdent(parser)
    }
    main = node('Get', { main, name, index }, span(parser, main.meta))
  }
  return main
}

const parseMut = (parser) => {
  const meta = parser.token.meta
  eat(parser, 'mut')
  const value = parseExpr(parser)
  return node('Mut', { value }, span(parser, meta))
}

const parseUpper = (parser) => {
  const { value, meta } = eat(parser, 'upper')

  const name = node('Name', { value }, meta)

  if (matchLines(parser) && tokenIs(parser, 'dot')) {
    return parseGet(parser, name)
  }

  const values = parseArgs(parser)
  return node('Symbol', { name, values }, span(parser, meta))
}

const parseApply = (parser, fn) => {
  const args = parseArgs(parser)
  return args
    .reduce((fn, arg) => node('Apply', { fn, arg }, span(parser, fn.meta)), fn)
}

const parseArgs = (parser) => {
  return parseWhile(
    parser, parseTerm, pp => startTerm(pp) && matchLines(pp))
}

const parseUnary = (parser) => {
  const { kind, meta } = parser.token

  const operator = node('Name', { value: kind }, meta)

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

  return node('Unary', { operator, rhs: parseTerm(parser) }, span(parser, meta))
}

const parseBinary = (parser, minimum, expr) => {
  if (expr == null) {
      expr = parseExpr(parser)
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

    const operator = node('Name', { value: kind }, meta)

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

    expr = node('Binary', { operator, lhs, rhs }, span(parser, expr.meta))
  }

  return expr
}

const parseNumber = (parser) => {
  const { value, meta } = eat(parser, 'number')
  return node('Number', { value }, meta)
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

  return node('Template', { elements }, span(parser, meta))
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
  return node('String', { value }, { raw, ...meta })
}

const parseWildcard = (parser) => {
  const meta = parser.token.meta
  eat(parser, 'wildcard')
  return node('Wildcard', {}, meta)
}

const parseParens = (parser) => {
  const meta = parser.token.meta
  const items = blockOf(parser, 'lparen', 'rparen', parseExpr)

  switch (items.length) {
    case 0:
      return node('Unit', {}, span(parser, meta))
    case 1:
      const inner = items.pop()
      return node('Group', { inner }, span(parser, meta))
    default:
      return node('Tuple', { items }, span(parser, meta))
  }
}

const parseBrackets = (parser) => {
  const meta = parser.token.meta

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

    let val = key
    if (kind === 'Dict') {
      eat(parser, 'colon')
      val = parseExpr(parser)
    }

    if (kind === 'Dict') {
      const meta = {
        line: key.meta.line,
        span: {
          start:  key.meta.start,
          offset: val.meta.offset,
        }
      }
      items.push(node('Property', { key, value: val }, meta))
    } else {
      items.push(val)
    }

    if (!maybeEat(parser, 'comma')) {
      break
    }
  }

  eat(parser, 'rbracket')

  if (kind === undefined) {
      kind = 'List'
  }

  return node(kind, { items }, span(parser, meta))
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

  return node('Block', { items }, span(parser, meta))
}

const parseFn = (parser) => {
  const meta = parser.token.meta

  eat(parser, 'fn')

  const params = parseWhile(
    parser, parseIdent, pp => matchLines(pp) && tokenIs(pp, 'lower'))

  let value
  if (tokenIs(parser, 'lbrace') && matchLines(parser)) {
    value = parseBlock(parser)
  } else if (matchLines(parser)) {
    value = parseExpr(parser)
  } else if (params.length !== 0) {
    value = params.pop()
  } else {
    return error(parser, meta, 'fn `value` must start in the same line')
  }

  return node('Fn', { params, value }, span(parser, meta))
}

const parseIf = (parser) => {
  const meta = parser.token.meta
  eat(parser, 'if')

  const test = parseExpr(parser)

  let then
  if (tokenIs(parser, 'lbrace') && matchLines(parser)) {
    then = parseBlock(parser)
  } else {
    eat(parser, 'then')
    then = parseExpr(parser)
  }

  eat(parser, 'else')

  const otherwise = parseExpr(parser)

  return node('If', { test, then, otherwise }, span(parser, meta))
}

const parseFor = (parser) => {
  const meta = parser.token.meta
  eat(parser, 'for')

  const target = parseTerm(parser)
  const source = parseTerm(parser)
  let value
  if (tokenIs(parser, 'lbrace') && matchLines(parser)) {
    value = parseBlock(parser)
  } else {
    value = parseExpr(parser)
  }

  return node('For', { target, source, value }, span(parser, meta))
}

const parseMatch = (parser) => {
  const meta = parser.token.meta
  eat(parser, 'match')
  const value = parseExpr(parser)

  eat(parser, 'lbrace')

  const cases = []

  while (!done(parser)) {
    const pattern = parseTerm(parser)

    eat(parser, 'arrow')

    let result
    if (tokenIs(parser, 'lbrace') && matchLines(parser)) {
      result = parseBlock(parser)
    } else {
      result = parseExpr(parser)
    }

    cases.push(node('Case', { pattern, result }, span(parser, pattern.meta)))

    if (tokenIs(parser, 'rbrace')) {
      break
    }
  }

  eat(parser, 'rbrace')

  return node('Match', { value, cases }, span(parser, meta))
}

const parseWhile = (parser, fn, predicate) => {
  const result = []

  while (!done(parser) && predicate(parser)) {
    result.push(fn(parser))
  }

  return result
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

const peekIs = (parser, kind) => {
  return parser.peek.kind === kind
}

const error = (parser, meta, msg) => {
  throw new Error(`Error [${meta.line}]: ${msg}`)
}

exports.parse = (tokens) => {
  const pp = parser(tokens)
  bump(pp) // initialize

  const nodes = []

  while (!done(pp)) { nodes.push(parseExpr(pp)) }

  return { nodes }
}
