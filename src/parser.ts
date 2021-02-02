import * as AST from './syntax'
import { Span, Expr } from './syntax'
import { Token, Kind, operatorInfo, defaultToken } from './token'

export interface Parser {
  tokens: Token[]
  token: Token
  prev : Token
  index: number
  nodes: AST.Expr[]
}

const parser = (tokens: Token[]): Parser => ({
  tokens,
  token: defaultToken(),
  prev : defaultToken(),
  index: 0,
  nodes: [],
})


const next = (parser: Parser): Expr => {
  switch (parser.token.kind) {
    case 'lower':
      return definition(parser)
      break
    default:
      throw 'TODO allow top level expressions'
  }
}

const identifier = (parser: Parser) => {
  const { value, span } = eat(parser, 'lower')
  return AST.name(value as string, span)
}

const definition = (parser: Parser): AST.Let => {
  const start = parser.token.span

  const name = identifier(parser)

  let value: Expr
  switch (parser.token.kind) {
    case 'lparen':
      value = parseFun(parser); break;
    case 'equals': {
      bump(parser)
      value = parseExpr(parser); break;
    }
    default:
      throw 'TODO allow top level expressions'
  }

  return AST.def(name, value, span(parser, start))
}

const parseFun = (parser: Parser): AST.Fun => {
  const start  = parser.token.span
  const params = blockOf(parser, 'lparen', 'rparen', identifier)
  eat(parser, 'equals')
  const value = parseExpr(parser)

  return AST.fn(params, value, span(parser, start))
}

const parseExpr = (parser: Parser): Expr => {
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

const parseTerm = (parser: Parser, apply = true): Expr => {
  let value: Expr
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

const parseLet = (parser: Parser, term: Expr): Expr => {
  bump(parser)
  return AST.def(term, parseExpr(parser), span(parser, term.span))
}

const parseMut = (parser: Parser, term: Expr): Expr => {
  bump(parser)
  return AST.mut(term, parseExpr(parser), span(parser, term.span))
}

const parseDot = (parser: Parser, term: Expr): Expr => {
  while (maybeEat(parser, 'dot')) {
    term = AST.member(term, identifier(parser), span(parser, term.span))
  }
  return term
}

const parseUpper = (parser: Parser): Expr => {
  const { value, span: start } = eat(parser, 'upper')

  if (value == null) {
    return error(parser, 'Internal Compiler Error: identifier without value')
  }

  const name = AST.name(value, start)

  if (matchLines(parser) && matches(parser, 'dot')) {
    return parseDot(parser, name)
  }

  return AST.variant(name, parseArgs(parser), span(parser, start))
}

const parseArgs = (parser: Parser): Expr[] => {
  const args: Expr[] = []

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

const parseApply = (parser: Parser, callee: Expr): AST.Apply | Expr => {
  const args = parseArgs(parser)

  if (args.length === 0) {
    return callee
  } else {
    return AST.apply(callee, args, span(parser, callee.span))
  }
}

const parseNumber = (parser: Parser): AST.Literal<number> => {
  const { value: raw, span } = eat(parser, 'number')
  if (raw == null) {
    return error(parser, 'Internal Compiler Error: number without value')
  }
  if (raw.includes('.')) {
    return AST.float(parseFloat(raw), raw, span)
  } else {
    return AST.integer(parseInt(raw), raw, span)
  }
}

const parseTemplate = (parser: Parser): AST.Template => {
  const start = parser.token.span

  const parts: Expr[] = []

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
      return (part.value as string).length > 0
    } else {
      return true
    }
  })

  return AST.template(elements, span(parser, start))
}

const parseString = (parser: Parser, kind: Kind): AST.Literal<string> => {
  const { value: raw, span } = eat(parser, kind)
  if (raw == null) {
    return error(parser, 'Internal Compiler Error: string without value')
  }
  let value: string
  switch (kind) {
    case 'string': {
      value = raw?.slice(1, raw.length - 1)
      break
    }
    case 'string start': {
      value = raw.slice(1)
      break
    }
    case 'string finish': {
      value = raw?.slice(0, raw.length - 1)
      break
    }
    default:
      value = raw
  }
  return AST.string(value, raw, span)
}

const parseParens = (parser: Parser): AST.Group | AST.Tuple => {
  const start = parser.token.span
  const items = blockOf(parser, 'lparen', 'rparen', parseExpr)

  if (items.length === 1) {
    const inner = items[0]
    return AST.group(inner, span(parser, start))
  } else {
    return AST.tuple(items, span(parser, start))
  }
}

const parseList = (parser: Parser): AST.List => {
  const start = parser.token.span
  const items = blockOf(parser, 'lbracket', 'rbracket', parseExpr)

  return AST.list(items, span(parser, start))
}

const parseRecord = (parser: Parser): AST.Record => {
  const start = parser.token.span
  const props = blockOf(parser, 'lbrace', 'rbrace', parseProperty)

  return AST.record(props, span(parser, start))
}

const parseProperty = (parser: Parser): AST.Property => {
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

const parseUnary = (parser: Parser): Expr => {
  const operator = AST.name(parser.token.kind, parser.token.span)

  if (parser.prev.kind === 'lparen' && peek(parser).kind === 'rparen') {
    bump(parser)
    return operator
  }

  bump(parser)

  if (matchLines(parser)) {
    const args = [
      parseTerm(parser, false)
    ]
    return AST.apply(operator, args, span(parser, operator.span))
  } else {
    return operator
  }
}

const parseBinary = (parser: Parser, minPrecedence: number): Expr => {
  let lhs = parseTerm(parser)

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

    let fix: number;
    switch (associativity) {
        case 'right':
          fix = 0; break
        case 'left':
        case 'none':
          fix = 1; break
    }

    const rhs = parseBinary(parser, precedence + fix)

    lhs = AST.apply(operator, [lhs, rhs], span(parser, lhs.span))
  }

  return lhs
}

const parseFn = (parser: Parser): AST.Fun => {
  const start = parser.token.span
  const params = blockOf(parser, 'fn', 'arrow', identifier)
  const value = parseExpr(parser)

  return AST.fn(params, value, span(parser, start))
}

const parseDo = (parser: Parser): AST.Block => {
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

const parseIf = (parser: Parser): AST.If => {
  const start = parser.token.span
  eat(parser, 'if')
  const test = parseExpr(parser)
  eat(parser, 'then')
  const then = parseExpr(parser)
  eat(parser, 'else')
  const otherwise = parseExpr(parser)

  return AST.cond(test, then, otherwise, span(parser, start))
}

const blockOf = <T extends unknown>(
  parser: Parser,
  open: Kind,
  close: Kind,
  fn: (pp: Parser) => T): T[] =>
{
  const result = Array<T>()

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

const span = (parser: Parser, start: Span, end?: Span): Span => {
  end = end ?? parser.prev.span
  return { ...start, offset: end.offset }
}

const matchLines = (parser: Parser) => {
  return parser.token.span.lineno === parser.prev.span.lineno
}

const maybeEat = (parser: Parser, kind: Kind) => {
  if (matches(parser, kind)) {
    bump(parser)
    return true
  } else {
    return false
  }
}

const eat = (parser: Parser, kind: Kind) => {
  if (matches(parser, kind)) {
    bump(parser)
    return parser.prev
  } else {
    return error(parser, `Expected '${kind}', but found '${parser.token.kind}'`)
  }
}

const done = (parser: Parser) => {
  return parser.token.kind === 'eof'
}

const bump = (parser: Parser) => {
  parser.prev  = parser.token
  parser.token = parser.tokens[parser.index] ?? defaultToken()
  parser.index += 1
}

const matches = (parser: Parser, kind: Kind) => {
  return parser.token.kind === kind
}

const peek = (parser: Parser) => {
  return parser.tokens[parser.index] ?? defaultToken()
}

const error = (parser: Parser, msg?: string) => {
  throw `Error [${parser.token.span.lineno}]: ${msg}`
}

export const parse = (tokens: Token[]): AST.Module => {
  const pp = parser(tokens)
  bump(pp) // initialize

  const nodes = []

  while (!done(pp)) {
    nodes.push(next(pp))
  }

  return { nodes }
}
