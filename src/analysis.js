const analyzer = () => ({})

const node = (kind, props = {}, meta = {}) => {
  return { kind, ...props, meta }
}

const check = (analyzer, expr) => {
  switch (expr.kind) {
    case 'Fn':
      return checkFn(analyzer, expr)
    case 'Def':
      return checkDef(analyzer, expr)
    case 'Set':
      return checkSet(analyzer, expr)
    case 'Get':
      return checkGet(analyzer, expr)
    case 'Mut':
      return checkMut(analyzer, expr)
    case 'Apply':
      return checkApply(analyzer, expr)
    case 'Unary':
      return checkUnary(analyzer, expr)
    case 'Binary':
      return checkBinary(analyzer, expr)
    case 'Block':
      return checkBlock(analyzer, expr)
    case 'Group':
      return checkGroup(analyzer, expr)
    case 'If':
      return checkIf(analyzer, expr)
    case 'Match':
      return checkMatch(analyzer, expr)
    case 'For':
      return checkFor(analyzer, expr)
    case 'Tuple':
      return checkTuple(analyzer, expr)
    case 'List':
      return checkList(analyzer, expr)
    case 'Dict':
      return checkDict(analyzer, expr)
    case 'Symbol':
      return checkSymbol(analyzer, expr)
    case 'Template':
      return checkTemplate(analyzer, expr)
    default:
      return expr
  }
}

const checkName = (analyzer, name) => {
  // const item = find(ctx, name)

  // if (!item) {
  //   return error(analyzer, name, `Cannot find value '${name}' in this scope`)
  // }

  return name
}

const checkFn = (analyzer, fn) => {
  if (fn.params.length == 0) {
      fn.params.push(node('Name', { value: '' }, {}))
  }

  const value = check(analyzer, fn.value)

  return fn.params
    .reverse()
    .reduce((value, param) => node('Fn', { param, value }, param.meta), value)
}

const checkDef = (analyzer, def) => {
  def.value = check(analyzer, def.value)
  return def
}

const checkSet = (analyzer, set) => {
  set.value = check(analyzer, set.value)
  set.target = check(analyzer, set.target)
  return set
}

const checkGet = (analyzer, get) => {
  const expr = check(analyzer, get.main)
  if (get.index) {
      get.index = check(analyzer, get.index)
  }

  return node('Get', { main, index: get.index, name: get.name }, get.meta)
}

const checkMut = (analyzer, mut) => {
  return check(analyzer, mut.value)
}

const checkApply = (analyzer, app) => {
  app.fn  = check(analyzer, app.fn)
  app.arg = check(analyzer, app.arg)
  return app
}

const checkUnary = (analyzer, unary) => {
  const fn  = check(analyzer, unary.operator)
  const arg = check(analyzer, unary.rhs)

  return node('Apply', { fn, arg }, unary.meta)
}

const checkBinary = (analyzer, binary) => {
  if (binary.operator.value.includes('pipe')) {
    return checkPipe(analyzer, binary)
  }

  const main = check(analyzer, binary.operator)

  const args = [
    check(analyzer, binary.lhs),
    check(analyzer, binary.rhs),
  ]

  return args
    .reduce((fn, arg) => node('Apply', { fn, arg }, arg.meta), main)
}

const checkPipe = (analyzer, pipe) => {
  const lhs = check(analyzer, pipe.lhs)
  const rhs = check(analyzer, pipe.rhs)

  let fn, arg
  if (pipe.operator.kind === 'lpipe') {
    fn  = rhs
    arg = lhs
  } else {
    fn  = lhs
    arg = rhs
  }

  return node('Apply', { fn, arg }, pipe.meta)
}

const checkBlock = (analyzer, block) => {
  const items = block.items.map(item => check(analyzer, item))

  if (items.length == 1) {
    return items.pop()
  }

  return node('Block', { items }, block.meta)
}

const checkGroup = (analyzer, group) => {
  return check(analyzer, group.inner)
}

const checkIf = (analyzer, exprIf) => {
  const conditions = []

  conditions.push(
      node('Case', {
        test: check(analyzer, exprIf.test),
        then: check(analyzer, exprIf.then),
      }, {}))

  let otherwise = exprIf.otherwise

  while (otherwise.kind === 'If') {
    conditions.push(
        node('Case', {
          test: check(analyzer, otherwise.test),
          then: check(analyzer, otherwise.then),
        }, {}))

    otherwise = otherwise.otherwise
  }

  otherwise = check(analyzer, otherwise)

  return node('Cond', { conditions, otherwise }, exprIf.meta)
}

const checkMatch = (analyzer, match) => {
  const conditions = match.cases.map(item => {

    const { conditions
          , bindings } = checkPattern(analyzer, match.value, item.pattern)

    let test
    if (conditions.length !== 1) {
      test = node('Native', {
        action: 'And',
        values: conditions,
      })
    } else {
      test = conditions.pop()
    }

    const then = item.result

    return node('Case', {
      test: check(analyzer, test),
      then: check(analyzer, then),
    }, {})
  })

  const last = conditions[conditions.length - 1]

  let otherwise
  if (last.test.kind === 'Symbol' && last.test.name.value === 'True') {
    otherwise = undefined
  } else {
    otherwise = node('Raise', {
      error: node('String', {
        value: 'Could not match any of the patterns'
      })
    }, match.value.meta)
  }

  return node('Cond', { conditions, otherwise }, match.meta)
}

const checkPattern = (analyzer, value, pattern) => {
  switch (pattern.kind) {
    case 'Number':
    case 'String':
      return literalPattern(analyzer, value, pattern)
    case 'Wildcard':
      return anyPattern(analyzer, value, pattern)
    case 'List':
      return listPattern(analyzer, value, pattern)
    case 'Dict':
      //return dictPattern(analyzer, value, pattern)
    case 'Tuple':
      //return tuplePattern(analyzer, value, pattern)
    case 'Symbol':
      //return variantPattern(analyzer, value, pattern)
    case 'Name':
      //return namePattern(analyzer, value, pattern)
    default:
      return error(analyzer, item.pattern.meta, `'${item.pattern.kind}' not supported`)
  }
}

const literalPattern = (analyzer, value, pattern) => {
  return {
    conditions: [
      node('Native', { action: 'Equals', values: [value, pattern] })
    ],
    bindings: [],
  }
}

const namePattern = (analyzer, value, pattern) => {
  return {
    conditions: [
      node('Symbol', { name: node('Name', { value: 'True' }), values: [] }, pattern.meta)
    ],
    bindings: [
      {
        name: pattern.value, value
      }
    ]
  }
}

const anyPattern = (analyzer, value, pattern) => {
  return {
    conditions: [
      node('Symbol', { name: node('Name', { value: 'True' }), values: [] }, pattern.meta)
    ],
    bindings: []
  }
}

const listPattern = (analyzer, value, pattern) => {
  const arrayCheck = node('Native', {
    action: 'Call',
    values: [
      node('Literal', { value: 'Array.isArray' }),
      value,
    ]
  })

  if (pattern.items.length === 0) {
    const emptyCheck = node('Native', {
      action: 'Equals',
      values: [
        node('Get', {
          main: value,
          name: node('Name', { value: 'length' }),
        }),
        node('Number', { value: '0' })
      ]
    })

    return {
      conditions: [
        arrayCheck,
        emptyCheck,
      ],
      bindings: []
    }
  }

  const items = pattern.items.map((item, index) => {
    const element = node('Get', {
      main: value,
      index: node('Number', { value: index.toString() })
    })
    return checkPattern(analyzer, element, item)
  })

  items.unshift({ conditions: [arrayCheck], bindings: [] })

  return items.reduce((total, item) => {
    return {
      conditions: [
        ...total.conditions, ...item.conditions,
      ],
      bindings: [
        ...total.bindings, ...item.bindings,
      ]
    }
  })
}

const dictPattern = (analyzer, value, pattern) => {
}

const tuplePattern = (analyzer, value, pattern) => {
}

const variantPattern = (analyzer, value, pattern) => {
}

const checkFor = (analyzer, exprFor) => {
  const { target
        , source
        , value } = exprFor

  const map = node('Name', { value: 'map' }, {})

  const arg = node('Fn', {
    value,
    params: [ target ],
  }, {})

  const first = node('Apply', { fn: map, arg })
  const final = node('Apply', { fn: first, arg: source })

  return check(analyzer, final)
}

const checkTuple = (analyzer, tuple) => {
  const items = tuple.items.map(item => check(analyzer, item))

  return node('List', { items }, tuple.meta)
}

const checkList = (analyzer, list) => {
  const items = list.items.map(item => check(analyzer, item))
  return node('List', { items }, list.meta)
}

const checkDict = (analyzer, record) => {
  const items = record.items.map(prop => {
    return { key: check(analyzer, prop.key), value: check(analyzer, prop.value) }
  })

  return node('Dict', { items }, record.meta)
}

const checkSymbol = (analyzer, symbol) => {
  const values = symbol.values.map(value => check(analyzer, value))

  return node('Symbol', { name: symbol.name, values }, symbol.meta)
}

const checkTemplate = (analyzer, template) => {
  const elements = template.elements.map(elem => check(analyzer, elem))

  return node('Template', { elements }, template.meta)
}

const error = (analyzer, meta, msg) => {
  throw `Error [${meta.line}] ${msg}`
}

exports.analyze = (module) => {
  const aa = analyzer(module)
  const nodes = module.nodes.map(node => check(aa, node))

  return { nodes }
}