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
  'mul':    { associativity: 'left',  precedence: 11, },
  'div':    { associativity: 'left',  precedence: 11, },
  'rem':    { associativity: 'left',  precedence: 11, },
  'add':    { associativity: 'left',  precedence: 10, },
  'sub':    { associativity: 'left',  precedence: 10, },
  'concat': { associativity: 'left',  precedence: 10, },
  'bitshr': { associativity: 'left',  precedence: 9,  },
  'bitshl': { associativity: 'left',  precedence: 9,  },
  'bitand': { associativity: 'left',  precedence: 8,  },
  'bitxor': { associativity: 'left',  precedence: 7,  },
  'bitor':  { associativity: 'left',  precedence: 6,  },
  'lt':     { associativity: 'left',  precedence: 5,  },
  'le':     { associativity: 'left',  precedence: 5,  },
  'gt':     { associativity: 'left',  precedence: 5,  },
  'ge':     { associativity: 'left',  precedence: 5,  },
  'ne':     { associativity: 'left',  precedence: 5,  },
  'eq':     { associativity: 'left',  precedence: 5,  },
  'and':    { associativity: 'left',  precedence: 4,  },
  'or':     { associativity: 'left',  precedence: 3,  },
  'pipe':   { associativity: 'left',  precedence: 2,  },
}

export const keywords: { [key: string]: Keywords } = {
  'do': 'do',
  'if': 'if',
  'then': 'then',
  'else': 'else',
  'fn': 'fn',
  'is': 'is',
  'and': 'and',
  'or': 'or',
  'not': 'not',
}

export const operators: { [key: string]: Kind } = {
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
| 'not'
| 'concat'
| 'bitand'
| 'bitor'
| 'bitxor'
| 'bitshr'
| 'bitshl'
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
| 'equals'
| 'walrus'


export type Keywords =
  'do'
| 'if'
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
| 'arrow'
| 'colon'
| 'comma'
| 'lower'
| 'upper'
| 'number'
| 'string start'
| 'string finish'
| 'string fragment'
| 'string'
| 'eof'