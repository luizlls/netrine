import * as AST from './syntax'
import { Span, Expr } from './syntax'
import { Token, Kind, operatorInfo, defaultToken } from './token'

export interface Parser {
  tokens: Token[]
  token?: Token
  prev? : Token
  index: number
  nodes: AST.Expr[]
}

const parser = (tokens: Token[]) => ({
  tokens,
  token: defaultToken(),
  prev : defaultToken(),
  index: 0,
  nodes: [],
}) as Parser


const next = (parser: Parser) => {
  let node: Expr
  switch (parser.token.kind) {
    case 'lower':
      node = definition(parser)
      break
    default:
      throw 'TODO allow top level expressions'
  }
  parser.nodes.push(node)
}

const identifier = (parser: Parser) => {
  const { value: name, span } = eat(parser, 'lower')

  return { kind: 'Name', name, span } as AST.Name
}

const definition = (parser: Parser) => {
  const start = parser.token.span

  const name = identifier(parser)

  let value: Expr
  switch (parser.token.kind) {
    case 'lparen':
      value = parseFun(parser); break;
    case 'equals':
      value = parseExpr(parser); break;
    default:
      throw 'TODO allow top level expressions'
  }

  return { kind: 'Let', name, value, span: complete(parser, start) } as AST.Let
}

const parseFun = (parser: Parser) => {
  const start  = parser.token.span
  const params = blockOf(parser, 'lparen', 'rparen', parseExpr)
  eat(parser, 'equals')
  const value = parseExpr(parser)

  return { kind: 'Function', params, value, span: complete(parser, start) } as AST.Fun
}

const parseExpr = (parser: Parser): Expr => {
  switch (parser.token.kind) {
    case 'fn': return parseFn(parser)
    case 'if': return parseIf(parser)
    default: return parseBinary(parser, 0)
  }
}

const parseTerm = (parser: Parser, apply = true): Expr => {
  switch (parser.token.kind) {
    case 'lower': {
      const term = identifier(parser)
      return apply ? parseApply(parser, term) : term
    }
    case 'upper': return parseUpper(parser)
    case 'number': return parseNumber(parser)
    case 'string': return parseTemplate(parser)
    case 'lbrace': return parseRecord(parser)
    case 'lbracket': return parseSequence(parser)
    case 'lparen': {
      const term = parseParens(parser)
      return apply ? parseApply(parser, term) : term
    }
    default:
      if (operatorInfo[parser.token.kind]) {
        return parseUnary(parser)
      } else {
        return error(parser, `Unexpected ${parser.token.kind}`)
      }
  }
}

const parseUpper = (parser: Parser) => {
  const { value: name, span } = eat(parser, 'upper')

  if (matchLines(parser) && matches(parser, 'dot')) {
    eat(parser, 'dot')
    const qualifier = { kind: 'Name', name, span }
    const property  = identifier(parser)
    return { kind: 'Member', qualifier, property, span: complete(parser, span) } as AST.Member
  }

  const values = parseArgs(parser)

  return { kind: 'Variant', values, span: complete(parser, span) } as AST.Variant
}

const parseArgs = (parser: Parser) => {
  const args: Expr[] = []

  loop:
  while (matchLines(parser)) {
    switch (parser.token.kind) {
      case 'lower':
      case 'upper':
      case 'number':
      case 'string':
      case 'fragment':
      case 'lparen':
      case 'lbrace':
      case 'lbracket': {
        args.push(parseTerm(parser, false))
      }
        break;
      default:
        break loop;
    }
  }

  return args
}

const parseApply = (parser: Parser, callee: Expr) => {
  const args = parseArgs(parser)

  if (args.length === 0) {
    return callee
  } else {
    return { kind: 'Apply', fun: callee, args, span: complete(parser, callee.span) } as AST.Apply
  }
}

const parseNumber = (parser: Parser) => {
  const { value, span } = eat(parser, 'number')
  if (value.includes('.')) {
    return { kind: 'Float', value: parseFloat(value), span } as AST.Literal<number>
  } else {
    return { kind: 'Integer', value: parseInt(value), span } as AST.Literal<number>
  }
}

const parseTemplate = (parser: Parser) => {
  return parseString(parser, 'string')
}

const parseString = (parser: Parser, kind: Kind) => {
  const { value, span } = eat(parser, kind)
  return { kind: 'String', value, span } as AST.Literal<string>
}

const parseParens = (parser: Parser) => {
  const start = parser.token.span
  const items = blockOf(parser, 'lparen', 'rparen', parseExpr)

  if (items.length === 1) {
    return items.pop()
  } else {
    return { kind: 'Tuple', items, span: complete(parser, start) } as AST.Tuple
  }
}

const parseSequence = (parser: Parser) => {
  const start = parser.token.span
  const items = blockOf(parser, 'lbracket', 'rbracket', parseExpr)

  return { kind: 'Sequence', items, span: complete(parser, start) } as AST.Sequence
}

const parseRecord = (parser: Parser) => {
  const start = parser.token.span
  const props = blockOf(parser, 'lbrace', 'rbrace', parseProperty)

  return { kind: 'Record', props, span: complete(parser, start) } as AST.Record
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
  const start = parser.token.span

  const operator = {
    kind: 'Operator',
    name: parser.token.kind,
    span: parser.token.span
  } as AST.Operator

  bump(parser)

  if (matchLines(parser)) {
    const args = [
      parseTerm(parser, false)
    ]
    return { kind: 'Apply', fun: operator, args, span: complete(parser, start) } as AST.Apply
  } else {
    return operator
  }
}

const parseBinary = (parser: Parser, minPrecedence: number) => {
  let lhs = parseTerm(parser)

  while (!atEnd(parser)) {
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

    const operator = {
      kind: 'Operator',
      name: parser.token.kind,
      span: parser.token.span
    } as AST.Operator

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
    const fun = operator

    lhs = { kind: 'Apply', fun, args: [lhs, rhs], span: complete(parser, lhs.span) } as AST.Apply
  }

  return lhs
}

const parseFn = (parser: Parser) => {
  const start = parser.token.span
  const params = blockOf(parser, 'fn', 'arrow', parseExpr)
  const value = parseExpr(parser)

  return { kind: 'Function', params, value, span: complete(parser, start) } as AST.Fun
}

const parseIf = (parser: Parser) => {
  const start = parser.token.span
  eat(parser, 'if')
  const test = parseExpr(parser)
  eat(parser, 'then')
  const then = parseExpr(parser)
  eat(parser, 'else')
  const otherwise = parseExpr(parser)

  return { kind: 'If', test, then, otherwise, span: complete(parser, start) } as AST.If
}

const blockOf = <T extends unknown>(
  parser: Parser,
  open: Kind,
  close: Kind,
  fn: (pp: Parser) => T) =>
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

const complete = (parser: Parser, start: Span, end?: Span) => {
  end = end ?? parser.prev.span
  return { ...start, offset: end.offset } as Span
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

const atEnd = (parser: Parser) => {
  return parser.index >= parser.tokens.length
}

const bump = (parser: Parser) => {
  parser.prev  = parser.token
  parser.token = parser.tokens[parser.index] ?? defaultToken()
  parser.index += 1
}

const matches = (parser: Parser, kind: Kind) => {
  return parser.token.kind === kind
}

const error = (parser: Parser, msg?: string) => {
  throw `Error [${parser.token.span.lineno}]: ${msg}`
}

export const parse = (tokens: Token[]): AST.Module => {
  const pp = parser(tokens)
  bump(pp) // initialize token

  while (!atEnd(pp)) { next(pp) }

  return { nodes: pp.nodes } as AST.Module
}
