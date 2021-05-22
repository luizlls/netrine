export interface Token {
  kind: TokenKind
  value?: string
  meta: {
    line: number
    span: {
      start: number
      offset: number
    }
  }
}

export const defaultToken = (): Token => ({
  kind: 'eof', meta: { line: 0, span: { start: 0, offset: 0 } }
})


type OperatorInfo = {
  precedence?: number
  associativity: 'left' | 'right' | 'none'
}

export const operatorInfo: { [key: string]: OperatorInfo } = {
  'not':    { associativity: 'none' },
  'bitnot': { associativity: 'none' },
  'mul':    { associativity: 'left',  precedence: 11, },
  'div':    { associativity: 'left',  precedence: 11, },
  'mod':    { associativity: 'left',  precedence: 11, },
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
  'lpipe':  { associativity: 'left',  precedence: 2,  },
  'rpipe':  { associativity: 'left',  precedence: 2,  },
}

export const keywords: { [key: string]: Keyword } = {
  'fn': 'fn',
  'if': 'if',
  'then': 'then',
  'else': 'else',
  'for': 'for',
  'match': 'match',
  'and': 'and',
  'or' : 'or',
  'not': 'not',
  'mut': 'mut',
}


export const operators: { [key: string]: Operator } = {
  '->' : 'arrow',
  ':'  : 'colon',
  '.'  : 'dot',
  '='  : 'equals',
  ':=' : 'walrus',
  '+'  : 'add',
  '-'  : 'sub',
  '*'  : 'mul',
  '/'  : 'div',
  '%'  : 'mod',
  '&&&': 'bitand',
  '|||': 'bitor',
  '~~~': 'bitnot',
  '^^^': 'bitxor',
  '>>>': 'bitshl',
  '<<<': 'bitshr',
  '++' : 'concat',
  '|>' : 'lpipe',
  '<|' : 'rpipe',
  '==' : 'eq',
  '!=' : 'ne',
  '<'  : 'lt',
  '<=' : 'le',
  '>'  : 'gt',
  '>=' : 'ge',
}


export type TokenKind =
  Keyword
| Operator
| Other

export type Operator =
  'add'
| 'sub'
| 'mul'
| 'div'
| 'mod'
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
| 'lpipe'
| 'rpipe'
| 'dot'
| 'equals'
| 'walrus'
| 'arrow'
| 'colon'


export type Keyword =
  'fn'
| 'if'
| 'then'
| 'else'
| 'match'
| 'for'
| 'mut'
| 'and'
| 'or'
| 'not'


export type Other =
  'lparen'
| 'rparen'
| 'lbrace'
| 'rbrace'
| 'lbracket'
| 'rbracket'
| 'comma'
| 'semi'
| 'any'
| 'lower'
| 'upper'
| 'number'
| 'string start'
| 'string finish'
| 'string fragment'
| 'string'
| 'eof'