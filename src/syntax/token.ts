export interface Token {
  type: TType
  value?: string
  line: number
  span: {
    start: number
    offset: number
  }
}

export function defaultToken(): Token {
  return {
    type: tokens.eof, line: 0, span: { start: 0, offset: 0 }
  }
}

export interface TType {
  label: string
  operator?: boolean
  prefix?: boolean
  precedence?: number
}

function make(label: string, operator?: boolean, precedence?: number, prefix?: boolean) : TType {
  return {
    label, operator: !!operator, prefix: !!prefix, precedence
  }
}

export const tokens = {
  lparen: make('('),
  rparen: make(')'),
  lbrace: make('{'),
  rbrace: make('}'),
  lbracket: make('['),
  rbracket: make(']'),
  dot: make('.'),
  comma: make(','),
  colon: make(':'),
  hash: make('#'),
  semicolon: make(';'),
  underscore: make('_'),
  arrow: make('->'),
  equals: make('='),
  walrus: make(':='),
  
  mut: make('mut'),
  if:  make('if'),
  then: make('then'),
  else: make('else'),

  mul: make('*', true, 7),
  div: make('/', true, 7),
  mod: make('%', true, 7),
  add: make('+', true, 6),
  sub: make('-', true, 6),
  lt: make('<', true, 5),
  le: make('<=', true, 5),
  gt: make('>', true, 5),
  ge: make('>=', true, 5),
  ne: make('/=', true, 5),
  eq: make('==', true, 5),
  and: make('and', true, 4),
  or: make('or', true, 3),
  is: make('is', true, 2),
  pipe: make('|>', true, 1),

  not: make('not', true, 0, true),
  
  lower: make('lowercase name'),
  upper: make('uppercase name'),
  number: make('number'),
  string: make('string'),
  
  stringStart: make('start of a template'),
  stringPart: make('part of a template'),
  stringFinish: make('end of a template'),

  eof: make('eof'),
}

export const keywords: { [key: string]: TType } = {
  'and': tokens.and,
  'or': tokens.or,
  'is': tokens.is,
  'not': tokens.not,
  'if': tokens.if,
  'then': tokens.then,
  'else': tokens.else,
  'mut': tokens.mut,
}

export const symbols: { [key: string]: TType } = {
  ':': tokens.colon,
  '.': tokens.dot,
  '->': tokens.arrow,
  '=': tokens.equals,
  ':=': tokens.walrus,
  '*': tokens.mul,
  '/': tokens.div,
  '%': tokens.mod,
  '+': tokens.add,
  '-': tokens.sub,
  '<': tokens.lt,
  '<=': tokens.le,
  '>': tokens.gt,
  '>=': tokens.ge,
  '/=': tokens.ne,
  '==': tokens.eq,
  '|>': tokens.pipe,
}