import * as Syntax from './syntax'

interface Analyzer {

}

const analyzer = (module: Syntax.Module) => ({
  module
})

const node = <K extends string, T>(kind: K, props: T, meta?: Syntax.Meta) => {
  return { kind, ...props, meta }
}

const check = (analyzer: Analyzer, expr: Syntax.Expr): Syntax.Expr => {
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
    case 'Variant':
      return checkVariant(analyzer, expr)
    case 'Template':
      return checkTemplate(analyzer, expr)
    default:
      return expr
  }
}

const checkName = (analyzer: Analyzer, name: Syntax.Name): Syntax.Name => {
  // const item = find(ctx, name)

  // if (!item) {
  //   return error(analyzer, name, `Cannot find value '${name}' in this scope`)
  // }

  return name
}

const checkFn = (analyzer: Analyzer, fn: Syntax.Fn): Syntax.Fn => {
  if (fn.params.length == 0) {
      fn.params.push(node('Name', { value: '' }))
  }

  const value = check(analyzer, fn.value)

  return fn.params
    .reverse()
    .reduce((value, param) => node('Fn', { params: [param], value }, param.meta), value) as Syntax.Fn
}

const checkDef = (analyzer: Analyzer, def: Syntax.Def): Syntax.Def => {
  def.value = check(analyzer, def.value)
  return def
}

const checkSet = (analyzer: Analyzer, set: Syntax.Set): Syntax.Set => {
  set.value = check(analyzer, set.value)
  set.target = check(analyzer, set.target) as Syntax.Get
  return set
}

const checkGet = (analyzer: Analyzer, get: Syntax.Get): Syntax.Get => {
  get.main = check(analyzer, get.main)
  if (get.index) {
      get.index = check(analyzer, get.index)
  }
  return get
}

const checkMut = (analyzer: Analyzer, mut: Syntax.Mut): Syntax.Mut => {
  mut.value = check(analyzer, mut.value)
  return mut
}

const checkApply = (analyzer: Analyzer, app: Syntax.Apply): Syntax.Apply => {
  app.fn  = check(analyzer, app.fn)
  app.arg = check(analyzer, app.arg)
  return app
}

const checkUnary = (analyzer: Analyzer, unary: Syntax.Unary): Syntax.Apply => {
  const fn  = node('Name', { value: unary.operator })
  const arg = check(analyzer, unary.rhs)

  return node('Apply', { fn, arg }, unary.meta)
}

const checkBinary = (analyzer: Analyzer, binary: Syntax.Binary): Syntax.Apply => {
  if (binary.operator.includes('pipe')) {
    return checkPipe(analyzer, binary)
  }

  const fn = node('Name', { value: binary.operator })

  const args = [
    check(analyzer, binary.lhs),
    check(analyzer, binary.rhs),
  ]

  return args
    .reduce((fn, arg) => node('Apply', { fn, arg }, arg.meta), fn) as Syntax.Apply
}

const checkPipe = (analyzer: Analyzer, pipe: Syntax.Binary): Syntax.Apply => {
  const lhs = check(analyzer, pipe.lhs)
  const rhs = check(analyzer, pipe.rhs)

  let fn, arg
  if (pipe.operator === 'lpipe') {
    fn  = rhs
    arg = lhs
  } else {
    fn  = lhs
    arg = rhs
  }

  return node('Apply', { fn, arg }, pipe.meta)
}

const checkBlock = (analyzer: Analyzer, block: Syntax.Block): Syntax.Expr => {
  if (block.items.length === 1) {
    return check(analyzer, block.items[0])
  }

  block.items = block.items.map(item => check(analyzer, item))
  return block
}

const checkGroup = (analyzer: Analyzer, group: Syntax.Group): Syntax.Expr => {
  return check(analyzer, group.inner)
}

const checkIf = (analyzer: Analyzer, conditional: Syntax.If): Syntax.If => {
  conditional.test = check(analyzer, conditional.test)
  conditional.then = check(analyzer, conditional.then)
  conditional.otherwise = check(analyzer, conditional.otherwise)

  return conditional
}

const checkMatch = (analyzer: Analyzer, match: Syntax.Match): Syntax.Expr => {
  const cases = match.cases.map(item => {

    const { conditions
          , definitions } = checkPattern(analyzer, match.value, item.pattern)

    let test: Syntax.Expr
    if (conditions.length > 1) {
      test = node('Native', { operation: 'And', values: conditions })
    } else {
      test = conditions[0]
    }

    const bindings = definitions
      .map(def => node('Def', { name: def.name, value: def.value })) as Syntax.Def[]

    const then = node('Block', { items: [...bindings, item.result] }) as Syntax.Block

    return {
      test: check(analyzer, test),
      then: check(analyzer, then),
    }
  })

  if (cases.length === 0) {
    return error(analyzer, match.meta, 'Internal Compiler Error: `match` expression without cases')
  }

  const last = cases.pop()!

  let otherwise: Syntax.Expr
  if (last.test.kind === 'True') {
    otherwise = last.then
  } else {
    cases.push(last) // return to list of cases

    otherwise = node('Raise', {
      error: node('String', {
        value: 'Could not match any of the patterns'
      })
    }, match.value.meta)
  }

  if (cases.length === 0) {
    return otherwise
  }

  const first = cases.shift()!

  const conditional = node('If', { test: first.test, then: first.then, otherwise }) as Syntax.If

  return cases
    .reduce((cond: Syntax.If, item) => {
      cond.otherwise = node('If', { test: item.test, then: item.then, otherwise }) as Syntax.If
      return cond
    }, conditional)
}

interface PatternResult {
  conditions: Syntax.Expr[]
  definitions: {
    name: Syntax.Name
    value: Syntax.Expr
  }[]
}

const checkPattern = (analyzer: Analyzer, value: Syntax.Expr, pattern: Syntax.Pattern): PatternResult => {
  switch (pattern.kind) {
    case 'Name':
      return namePattern(analyzer, value, pattern)
    case 'Number':
    case 'String':
      return literalPattern(analyzer, value, pattern)
    case 'Any':
      return anyPattern(analyzer, value, pattern)
    case 'List':
    case 'Tuple':
      return listPattern(analyzer, value, pattern)
    case 'Dict':
      return dictPattern(analyzer, value, pattern)
    case 'Variant':
      return variantPattern(analyzer, value, pattern)
    default:
      return error(analyzer, pattern.meta, `'${pattern.kind}' pattern not supported`)
  }
}

const literalPattern = (analyzer: Analyzer, value: Syntax.Expr, pattern: Syntax.String | Syntax.Number): PatternResult => {
  return {
    conditions: [
      node('Native', { operation: 'Equals', values: [value, pattern] })
    ],
    definitions: [],
  }
}

const namePattern = (analyzer: Analyzer, value: Syntax.Expr, pattern: Syntax.Name): PatternResult => {
  return {
    conditions: [
      node('True', {}, pattern.meta)
    ],
    definitions: [
      {
        name: pattern, value
      }
    ]
  }
}

const anyPattern = (analyzer: Analyzer, value: Syntax.Expr, pattern: Syntax.Any): PatternResult => {
  return {
    conditions: [
      node('True', {}, pattern.meta)
    ],
    definitions: []
  }
}

const listPattern = (analyzer: Analyzer, value: Syntax.Expr, pattern: Syntax.List | Syntax.Tuple): PatternResult => {

  const arrayCheck = node('Native', { operation: 'AssertList', values: [ value ] }) as Syntax.Native

  if (pattern.items.length === 0) {
    const emptyCheck = node('Native', { operation: 'EmptyList', values: [ value ] }) as Syntax.Native

    return {
      conditions: [
        arrayCheck,
        emptyCheck,
      ],
      definitions: []
    }
  }

  const items = pattern.items.map((item, index) => {
    switch (item.kind) {
      case 'Name':
      case 'List':
      case 'Dict':
      case 'Tuple':
      case 'String':
      case 'Number':
      case 'Variant':
      case 'Any':
      case 'Unit':
        break
      default:
        return error(analyzer, item.meta, `'${item.kind}' pattern not supported`)
    }

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

const dictPattern = (analyzer: Analyzer, value: Syntax.Expr, pattern: Syntax.Dict): PatternResult => {

  const dictCheck = node('Native', { operation: 'AssertDict', values: [ value ] }) as Syntax.Native

  if (pattern.items.length === 0) {
    const emptyCheck = node('Native', { operation: 'EmptyDict', values: [ value ] }) as Syntax.Native

    return {
      conditions: [
        dictCheck,
        emptyCheck,
      ],
      definitions: []
    }
  }

  const items = pattern.items.map(item => {
    switch (item.value.kind) {
      case 'Name':
      case 'List':
      case 'Dict':
      case 'Tuple':
      case 'String':
      case 'Number':
      case 'Variant':
      case 'Any':
      case 'Unit':
        break
      default:
        return error(analyzer, item.value.meta, `'${item.value.kind}' pattern not supported`)
    }

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
      node('Native', { operation: 'NotNull', values: [ property ] }))

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

const variantPattern = (analyzer: Analyzer, value: Syntax.Expr, pattern: Syntax.Variant): PatternResult => {
  const kindCheck = node('Native', {
    operation: 'Equals',
    values: [
      node('Get', {
        main: value,
        member: node('Name', { value: '$$' })
      }),
      node('String', {
        value: pattern.name.value
      }),
    ]
  }) as Syntax.Native

  const values = pattern.values.map((item, index) => {
    switch (item.kind) {
      case 'Name':
      case 'List':
      case 'Dict':
      case 'Tuple':
      case 'String':
      case 'Number':
      case 'Variant':
      case 'Any':
      case 'Unit':
        break
      default:
        return error(analyzer, item.meta, `'${item.kind}' pattern not supported`)
    }

    const element = node('Get', {
      main: value,
      member: node('Name', { value: `$${index.toString()}` })
    })

    const result = checkPattern(analyzer, element, item)

    result.conditions.unshift(
      node('Native', { operation: 'NotNull', values: [ element ] }))

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

const checkFor = (analyzer: Analyzer, comprehension: Syntax.For): Syntax.Expr => {
  const { target
        , source
        , value } = comprehension

  const expression = node('Apply', {
    fn: node('Apply', {
      fn: node('Name', { value: 'map' }),
      arg: node('Fn', {
        value,
        params: [ target ],
      }),
    }),
    arg: source
  })

  return check(analyzer, expression)
}

const checkTuple = (analyzer: Analyzer, tuple: Syntax.Tuple): Syntax.List => {
  const items = tuple.items.map(item => check(analyzer, item))
  return node('List', { items }, tuple.meta)
}

const checkList = (analyzer: Analyzer, list: Syntax.List): Syntax.List => {
  const items = list.items.map(item => check(analyzer, item))
  return node('List', { items }, list.meta)
}

const checkDict = (analyzer: Analyzer, dict: Syntax.Dict): Syntax.Dict => {
  const items = dict.items.map(item => {
    return { key: check(analyzer, item.key), value: check(analyzer, item.value) }
  })

  return node('Dict', { items }, dict.meta)
}

const checkVariant = (analyzer: Analyzer, variant: Syntax.Variant): Syntax.Variant => {
  variant.values = variant.values.map(value => check(analyzer, value))
  return variant
}

const checkTemplate = (analyzer: Analyzer, template: Syntax.Template): Syntax.Expr => {
  const elements = template.elements.map(elem => check(analyzer, elem))

  const concats  = elements.reduce((lhs, rhs) =>
    node('Binary', { operator: 'concat', lhs, rhs }))

  return check(analyzer, concats)
}

const error = (analyzer: Analyzer, meta?: Syntax.Meta, msg?: string) => {
  throw new Error(`Error [${meta?.line}] ${msg}`)
}

export const analyze = (module: Syntax.Module): Syntax.Module => {
  const aa = analyzer(module)
  const nodes = module.nodes.map(node => check(aa, node))

  return { nodes }
}
