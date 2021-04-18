exports.defaultToken = () => ({
  kind: 'eof', meta: { line: 0, span: { start: 0, offset: 0 } }
})

exports.operatorInfo = {
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

exports.keywords = {
  'case': 'case',
  'of': 'of',
  'and': 'and',
  'or' : 'or',
  'not': 'not',
  'mut': 'mut',
}


exports.operators = {
  '='  : 'equals',
  ':=' : 'walrus',
  '=>' : 'arrow',
  '.'  : 'dot',
  ':'  : 'colon',
  '|'  : 'pipe',
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
