const analyzer = () => ({
  definitions: {},
  constructors: {},
  nodes: [],
})

const node = (kind, props, meta) => {
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
  const { result: last } = match.cases.pop()

  const conditions = match.cases.map(item => {

    const test = node('Binary', {
      operator: node('Name', { value: 'eq' }, {}),
      lhs: match.value,
      rhs: item.pattern,
    }, {})

    const then = item.result

    return node('Case', {
      test: check(analyzer, test),
      then: check(analyzer, then),
    }, {})
  })

  const otherwise = check(analyzer, last)

  return node('Cond', { conditions, otherwise }, match.meta)
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

  if (analyzer.constructors[symbol.name] === undefined) {
      analyzer.constructors[symbol.name] = symbol
  }

  return node('Symbol', { name: symbol.name, values }, symbol.meta)
}

const checkTemplate = (analyzer, template) => {
  const elements = template.elements.map(elem => check(analyzer, elem))

  return node('Template', { elements }, template.meta)
}

const constructor = (analyzer, symbol) => {
  return node('Constructor', { name: symbol.name }, symbol.meta)
}

const definition = (analyzer, name, def) => {
  if (Array.isArray(def)) {
    return combine()
  } else {
    return checkDef(analyzer, def)
  }
}

const error = (analyzer, meta, msg) => {
  throw `Error [${meta.line}] ${msg}`
}

exports.analyze = (module) => {
  const aa = analyzer(module)

  for (const node of module.nodes) {
    aa.nodes.push(check(aa, node))
  }

  const nodes = []

  for (const symbol of Object.values(aa.constructors)) {
    nodes.push(constructor(aa, symbol))
  }

  for (const [name, def] of Object.entries(aa.definitions)) {
    nodes.push(definition(aa, name, def))
  }

  nodes.push(...aa.nodes)

  return { nodes }
}