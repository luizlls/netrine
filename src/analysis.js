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
  const main = check(analyzer, get.main)
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
          , definitions } = checkPattern(analyzer, match.value, item.pattern)

    let test
    if (conditions.length !== 1) {
      test = node('NativeAnd', { values: conditions })
    } else {
      test = conditions.pop()
    }

    const bindings = definitions.map(({ name, value }) => {
      return node('Def', {
        name: node('Name', { value: name }, name.meta),
        value,
      })
    })

    const then = node('Block', { items: [...bindings, item.result] })

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
    case 'Name':
      return namePattern(analyzer, value, pattern)
    case 'Number':
    case 'String':
      return literalPattern(analyzer, value, pattern)
    case 'Wildcard':
      return anyPattern(analyzer, value, pattern)
    case 'List':
    case 'Tuple':
      return listPattern(analyzer, value, pattern)
    case 'Dict':
      return dictPattern(analyzer, value, pattern)
    case 'Symbol':
      return variantPattern(analyzer, value, pattern)
    default:
      return error(analyzer, pattern.meta, `'${pattern.kind}' pattern not supported`)
  }
}

const literalPattern = (analyzer, value, pattern) => {
  return {
    conditions: [
      node('NativeEquals', { values: [value, pattern] })
    ],
    definitions: [],
  }
}

const namePattern = (analyzer, value, pattern) => {
  return {
    conditions: [
      node('Symbol', { name: node('Name', { value: 'True' }), values: [] }, pattern.meta)
    ],
    definitions: [
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
    definitions: []
  }
}

const listPattern = (analyzer, value, pattern) => {
  const arrayCheck = node('AssertList', { values: [ value ] })

  if (pattern.items.length === 0) {
    const emptyCheck = node('AssertEmptyList', { values: [ value ] })

    return {
      conditions: [
        arrayCheck,
        emptyCheck,
      ],
      definitions: []
    }
  }

  const items = pattern.items.map((item, index) => {
    const element = node('Get', {
      main: value,
      index: node('Number', { value: index.toString() })
    })
    return checkPattern(analyzer, element, item)
  })

  items.unshift({ conditions: [arrayCheck], definitions: [] })

  return items.reduce((total, item) => {
    return {
      conditions: [
        ...total.conditions, ...item.conditions,
      ],
      definitions: [
        ...total.definitions, ...item.definitions,
      ]
    }
  })
}

const dictPattern = (analyzer, value, pattern) => {
  const dictCheck = node('AssertDict', { values: [ value ] })

  if (pattern.items.length === 0) {
    const emptyCheck = node('AssertEmptyDict', { values: [ value ] })
    return {
      conditions: [
        dictCheck,
        emptyCheck,
      ],
      definitions: []
    }
  }

  const items = pattern.items.map(item => {
    let index
    if (item.key.kind === 'Name') {
      index = node('String', { value: item.key.value })
    } else {
      index = item.key
    }

    const property = node('Get', {
      main: value, index
    })

    const result = checkPattern(analyzer, property, item.value)

    result.conditions.unshift(
      node('AssertNotNull', { values: [ property ] }))

    return result
  })

  items.unshift({ conditions: [dictCheck], definitions: [] })

  return items.reduce((total, item) => {
    return {
      conditions: [
        ...total.conditions, ...item.conditions,
      ],
      definitions: [
        ...total.definitions, ...item.definitions,
      ]
    }
  })
}

const variantPattern = (analyzer, value, pattern) => {
  const kindCheck = node('NativeEquals', {
    values: [
      node('Get', {
        main: value,
        name: node('Name', {
          value: '$$'
        })
      }),
      node('String', {
        value: pattern.name.value
      }),
    ]
  })

  const values = pattern.values.map((item, index) => {
    const element = node('Get', {
      main: value,
      name: node('Name', { value: `$${index.toString()}` })
    })

    const result = checkPattern(analyzer, element, item)

    result.conditions.unshift(
      node('AssertNotNull', { values: [ element ] }))

    return result
  })

  values.unshift({ conditions: [kindCheck], definitions: [] })

  return values.reduce((total, item) => {
    return {
      conditions: [
        ...total.conditions, ...item.conditions,
      ],
      definitions: [
        ...total.definitions, ...item.definitions,
      ]
    }
  })
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
  throw new Error(`Error [${meta.line}] ${msg}`)
}

exports.analyze = (module) => {
  const aa = analyzer(module)
  const nodes = module.nodes.map(node => check(aa, node))

  return { nodes }
}
