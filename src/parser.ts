import * as Syntax from './syntax'
import { Token, TokenKind, operatorInfo, defaultToken, Operator } from './token'

export interface Parser {
  tokens: Token[]
  token: Token
  peek : Token
  prev : Token
  index: number
  nodes: Syntax.Expr[]
}

const parser = (tokens: Token[]): Parser => ({
  tokens,
  token: defaultToken(),
  peek : defaultToken(),
  prev : defaultToken(),
  index: 0,
  nodes: [],
})

const node = <K extends string, T>(kind: K, props: T, meta: Syntax.Meta) => {
  return { kind, ...props, meta }
}

const parseName = (parser: Parser): Syntax.Name => {
  const { value, meta } = eat(parser, 'lower')
  if (value == undefined) {
    return error(parser, meta, 'Internal compiler error: error parsing value of identifier')
  }
  return node('Name', { value }, meta)
}

const parseExpr = (parser: Parser): Syntax.Expr => {
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
    default: break
  }

  if (operatorInfo[parser.token.kind] !== undefined) {
    return parseUnary(parser)
  }

  if (!startTerm(parser)) {
    return error(parser, parser.token.meta, `Unexpected ${parser.token.kind}`)
  }
  
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
}

const startTerm = (parser: Parser): boolean => {
  switch (parser.token.kind) {
    case 'lower':
    case 'upper':
    case 'lparen':
    case 'lbracket':
    case 'number':
    case 'string':
    case 'string start':
    case 'any':
      return true
    default:
      return false
  }
}

const parseTerm = (parser: Parser): Syntax.Expr => {
  let value: Syntax.Expr
  switch (parser.token.kind) {
    case 'lower':
      value = parseName(parser); break
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
    case 'any':
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

const parseDef = (parser: Parser, name: Syntax.Name): Syntax.Def => {
  eat(parser, 'equals')
  const value = parseExpr(parser)
  return node('Def', { name, value }, span(parser, name.meta))
}

const parseSet = (parser: Parser, target: Syntax.Name | Syntax.Get): Syntax.Set => {
  eat(parser, 'walrus')
  const value = parseExpr(parser)
  return node('Set', { target, value }, span(parser, target.meta))
}

const parseGet = (parser: Parser, main: Syntax.Expr): Syntax.Get => {
  let expr = main as Syntax.Get
  while (maybeEat(parser, 'dot')) {
    let member, index
    if (tokenIs(parser, 'lbracket')) {
      eat(parser, 'lbracket')
      index = parseExpr(parser)
      eat(parser, 'rbracket')
    } else {
      member = parseName(parser)
    }
    expr = node('Get', { main, member, index }, span(parser, main.meta))
  }
  return expr
}

const parseMut = (parser: Parser): Syntax.Mut => {
  const meta = parser.token.meta
  eat(parser, 'mut')
  const value = parseExpr(parser)
  return node('Mut', { value }, span(parser, meta))
}

const parseUpper = (parser: Parser): Syntax.Get | Syntax.Variant | Syntax.True | Syntax.False => {
  const { value, meta } = eat(parser, 'upper')
  
  if (value == undefined) {
    return error(parser, meta, 'Internal compiler error: error parsing value of identifier')
  }

  switch (value) {
    case 'True':
      return node('True', {}, meta)
    case 'False':
      return node('False', {}, meta)
    default:
      break
  }

  const name = node('Name', { value }, meta)

  if (matchLines(parser) && tokenIs(parser, 'dot')) {
    return parseGet(parser, name)
  }

  const values = parseArgs(parser)
  return node('Variant', { name, values }, span(parser, meta))
}

const parseApply = (parser: Parser, fn: Syntax.Expr): Syntax.Apply => {
  const args = parseArgs(parser)
  return args
    .reduce((fn, arg) => node('Apply', { fn, arg }, span(parser, fn.meta)), fn) as Syntax.Apply
}

const parseArgs = (parser: Parser): Syntax.Expr[] => {
  return parseWhile(
    parser, parseTerm, pp => startTerm(pp) && matchLines(pp))
}

const parseUnary = (parser: Parser): Syntax.Unary | Syntax.Partial => {
  const { kind, meta } = parser.token

  const operator = kind as Operator

  if (parser.prev.kind === 'lparen'
  &&  parser.peek.kind === 'rparen') {
    bump(parser)
    return node('Partial', { operator }, meta)
  }

  bump(parser)

  const rhs = parseTerm(parser)
  return node('Unary', { operator, rhs }, span(parser, meta))
}

const parseTermWithApply = (parser: Parser): Syntax.Expr => {
  const term = parseTerm(parser)

  if (startTerm(parser) && matchLines(parser)) {
    return parseApply(parser, term)
  }

  return term
}

const parseBinary = (parser: Parser, minimum: number, starting?: Syntax.Expr): Syntax.Binary => {
  let expr = starting ?? parseTermWithApply(parser)

  while (!done(parser)) {
    const { kind, meta } = parser.token

    if (operatorInfo[kind] === undefined) {
      break
    }

    const { precedence,
            associativity } = operatorInfo[kind]

    if (precedence == undefined) {
      return error(parser, meta, `'${kind}' is not a valid binary (infix) operator`)
    }

    if (precedence < minimum) {
        break
    }

    const operator = kind as Operator

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

  return expr as Syntax.Binary
}

const parseNumber = (parser: Parser): Syntax.Number => {
  const { value, meta } = eat(parser, 'number')
  
  if (value == undefined) {
    return error(parser, meta, 'Internal compiler error: error parsing value of number')
  }

  return node('Number', { value }, meta)
}

const parseTemplate = (parser: Parser) => {
  const meta = parser.token.meta

  const parts: Syntax.Expr[] = []

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

const parseString = (parser: Parser, kind: TokenKind = 'string'): Syntax.String => {
  const { value: raw, meta } = eat(parser, kind)

  if (raw == undefined) {
    return error(parser, meta, 'Internal compiler error: error parsing value of string')
  }

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
  return node('String', { value, raw }, { ...meta })
}

const parseWildcard = (parser: Parser): Syntax.Any => {
  const meta = parser.token.meta
  eat(parser, 'any')
  return node('Any', {}, meta)
}

const parseParens = (parser: Parser): Syntax.Unit | Syntax.Group | Syntax.Tuple => {
  const meta = parser.token.meta
  const items = blockOf(parser, parseExpr, 'lparen', 'rparen')

  switch (items.length) {
    case 0:
      return node('Unit', {}, span(parser, meta))
    case 1:
      const inner = items[0]
      return node('Group', { inner }, span(parser, meta))
    default:
      return node('Tuple', { items }, span(parser, meta))
  }
}

const parseBrackets = (parser: Parser): Syntax.List | Syntax.Dict => {
  const meta = parser.token.meta

  let listItems: Syntax.Expr[] = []

  let dictItems: { 
    key  : Syntax.Expr,
    value: Syntax.Expr
  }[] = []

  eat(parser, 'lbracket')

  let kind
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
      dictItems.push({ key, value: val })
    } else {
      listItems.push(val)
    }

    if (!maybeEat(parser, 'comma')) {
      break
    }
  }

  eat(parser, 'rbracket')

  if (kind === undefined) {
      kind = 'List'
  }

  if (kind == 'List') {
    return node('List', { items: listItems }, span(parser, meta))
  } else {
    return node('Dict', { items: dictItems }, span(parser, meta))
  }
}

const parseBlock = (parser: Parser): Syntax.Block => {
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

const parseFn = (parser: Parser): Syntax.Fn => {
  const meta = parser.token.meta

  eat(parser, 'fn')

  const params = parseWhile(
    parser, parseName, pp => matchLines(pp) && tokenIs(pp, 'lower'))

  let value: Syntax.Expr
  if (tokenIs(parser, 'lbrace') && matchLines(parser)) {
    value = parseBlock(parser)
  } else if (matchLines(parser)) {
    value = parseExpr(parser)
  } else if (params.length !== 0) {
    value = params[0]
  } else {
    return error(parser, meta, 'fn `value` must start in the same line')
  }

  return node('Fn', { params, value }, span(parser, meta))
}

const parseIf = (parser: Parser): Syntax.If => {
  const meta = parser.token.meta
  eat(parser, 'if')

  const test = parseExpr(parser)

  let then: Syntax.Expr
  if (tokenIs(parser, 'lbrace') && matchLines(parser)) {
    then = parseBlock(parser)
  } else {
    eat(parser, 'then')
    then = parseExpr(parser)
  }

  eat(parser, 'else')

  let otherwise: Syntax.Expr
  if (tokenIs(parser, 'lbrace') && matchLines(parser)) {
    otherwise = parseBlock(parser)
  } else {
    otherwise = parseExpr(parser)
  }

  return node('If', { test, then, otherwise }, span(parser, meta))
}

const parseFor = (parser: Parser): Syntax.For => {
  const meta = parser.token.meta
  eat(parser, 'for')

  const target = parseName(parser)
  const source = parseTerm(parser)

  let value: Syntax.Expr
  if (tokenIs(parser, 'lbrace') && matchLines(parser)) {
    value = parseBlock(parser)
  } else {
    value = parseExpr(parser)
  }

  return node('For', { target, source, value }, span(parser, meta))
}

const parseMatch = (parser: Parser): Syntax.Match => {
  const meta = parser.token.meta
  eat(parser, 'match')
  const value = parseExpr(parser)

  eat(parser, 'lbrace')

  const cases = []

  while (!done(parser)) {
    const pattern = parseTerm(parser)

    switch (pattern.kind) {
      case 'Name':
      case 'List':
      case 'Dict':
      case 'Tuple':
      case 'String':
      case 'Number':
      case 'Variant':
      case 'Any':
      case 'Unit':
        break;
      default:
        return error(parser, pattern.meta, `'${pattern.kind}' pattern not supported`)
    }

    eat(parser, 'arrow')

    let result
    if (tokenIs(parser, 'lbrace') && matchLines(parser)) {
      result = parseBlock(parser)
    } else {
      result = parseExpr(parser)
    }

    cases.push({ pattern, result })

    if (tokenIs(parser, 'rbrace')) {
      break
    }
  }

  eat(parser, 'rbrace')

  return node('Match', { value, cases }, span(parser, meta))
}

const parseWhile = <E extends Syntax.Expr>(parser: Parser, fn: (p: Parser) => E, predicate: (p: Parser) => boolean) => {
  const result = []

  while (!done(parser) && predicate(parser)) {
    result.push(fn(parser))
  }

  return result
}

const blockOf = <E extends Syntax.Expr>(parser: Parser, fn: (p: Parser) => E, open: TokenKind, close: TokenKind) => {
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

const span = (parser: Parser, meta?: Syntax.Meta) => {
  if (meta == undefined) {
    return error(parser, parser.token.meta, 'Internal compiler error: Could not parse metadata')
  }
  const end = parser.prev.meta.span
  return { ...meta, span: { ...meta.span, offset: end.offset } }
}

const matchLines = (parser: Parser) => {
  return parser.token.meta.line === parser.prev.meta.line
}

const maybeEat = (parser: Parser, kind: TokenKind): boolean => {
  if (tokenIs(parser, kind)) {
    bump(parser)
    return true
  } else {
    return false
  }
}

const eat = (parser: Parser, expected: TokenKind): Token => {
  if (tokenIs(parser, expected)) {
    bump(parser)
    return parser.prev
  } else {
    const { kind, meta } = parser.token
    return error(parser, meta, `expected '${expected}', but found '${kind}'`)
  }
}

const done = (parser: Parser) => {
  return parser.token.kind === 'eof'
}

const bump = (parser: Parser) => {
  parser.prev  = parser.token
  parser.token = parser.tokens[parser.index + 0] ?? defaultToken()
  parser.peek  = parser.tokens[parser.index + 1] ?? defaultToken()
  parser.index += 1
  return parser
}

const tokenIs = (parser: Parser, kind: TokenKind): boolean => {
  return parser.token.kind === kind
}

const peekIs = (parser: Parser, kind: TokenKind): boolean => {
  return parser.peek.kind === kind
}

const error = (parser: Parser, meta?: Syntax.Meta, msg?: string): never => {
  throw new Error(`Error [${meta?.line}]: ${msg}`)
}

export const parse = (tokens: Token[]): Syntax.Module => {
  const pp = parser(tokens)
  bump(pp) // initialize

  const nodes = []

  while (!done(pp)) { nodes.push(parseExpr(pp)) }

  return { nodes }
}
