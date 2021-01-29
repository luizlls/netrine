export interface Token {
  kind: Kind
  value?: string
  span: {
    lineno: number
    start: number
    offset: number
  }
}

export const defaultToken = (): Token => {
  return { kind: 'eof', span: { lineno: 0, start: 0, offset: 0 } } as Token
}

type Associativity =
  'left'
| 'right'
| 'none'

type OperatorInfo = {
  precedence?: number
  associativity: Associativity
}

export const operatorInfo: { [key: string]: OperatorInfo } = {
  'not':    { associativity: 'none' },
  'bitnot': { associativity: 'none' },
  'mul':    { associativity: 'left',  precedence: 10, },
  'rem':    { associativity: 'left',  precedence: 10, },
  'add':    { associativity: 'left',  precedence: 9, },
  'sub':    { associativity: 'left',  precedence: 9, },
  'concat': { associativity: 'left',  precedence: 9, },
  'bitshr': { associativity: 'left',  precedence: 8, },
  'bitshl': { associativity: 'left',  precedence: 8, },
  'bitand': { associativity: 'left',  precedence: 7, },
  'bitxor': { associativity: 'left',  precedence: 6, },
  'bitor':  { associativity: 'left',  precedence: 5, },
  'lt':     { associativity: 'left',  precedence: 4, },
  'le':     { associativity: 'left',  precedence: 4, },
  'gt':     { associativity: 'left',  precedence: 4, },
  'ge':     { associativity: 'left',  precedence: 4, },
  'ne':     { associativity: 'left',  precedence: 4, },
  'eq':     { associativity: 'left',  precedence: 4, },
  'and':    { associativity: 'left',  precedence: 3, },
  'or':     { associativity: 'left',  precedence: 2, },
  'pipe':   { associativity: 'left',  precedence: 1, },
  'semi':   { associativity: 'right', precedence: 0, },
}

export const keywords: { [key: string]: Keywords } = {
  'if': 'if',
  'then': 'then',
  'else': 'else',
  'fn': 'fn',
  'is': 'is',
  'and': 'and',
  'or': 'or',
  'not': 'not',
}

export const operators: { [key: string]: Operators } = {
  '->' : 'arrow',
  ':'  : 'colon',
  '.'  : 'dot',
  '='  : 'equals',
  ':=' : 'walrus',
  '+'  : 'add',
  '-'  : 'sub',
  '*'  : 'mul',
  '/'  : 'div',
  '%'  : 'rem',
  '&&&': 'bitand',
  '|||': 'bitor',
  '~~~': 'bitnot',
  '^^^': 'bitxor',
  '>>>': 'bitshl',
  '<<<': 'bitshr',
  '++' : 'concat',
  '|>' : 'pipe',
  '==' : 'eq',
  '!=' : 'ne',
  '<'  : 'lt',
  '<=' : 'le',
  '>'  : 'gt',
  '>=' : 'ge',
}

export type Kind =
  Keywords
| Operators
| Others

export type Operators =
  'add'
| 'sub'
| 'mul'
| 'div'
| 'rem'
| 'and'
| 'or'
| 'bitand'
| 'bitor'
| 'bitxor'
| 'bitshr'
| 'bitshl'
| 'concat'
| 'not'
| 'bitnot'
| 'is'
| 'eq'
| 'ne'
| 'lt'
| 'le'
| 'gt'
| 'ge'
| 'pipe'
| 'dot'
| 'semi'
| 'colon'
| 'arrow'
| 'equals'
| 'walrus'


export type Keywords =
  'if'
| 'then'
| 'else'
| 'fn'
| 'is'
| 'and'
| 'or'
| 'not'


export type Others =
  'lparen'
| 'rparen'
| 'lbrace'
| 'rbrace'
| 'lbracket'
| 'rbracket'
| 'comma'
| 'lower'
| 'upper'
| 'number'
| 'string start'
| 'string finish'
| 'string fragment'
| 'string'
| 'eof'