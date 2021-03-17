const analyzer = () => ({
  definitions: {},
  constructors: {},
  nodes: [],
})

const node = (kind, props, span) => {
  return { kind, ...props, span }
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
    case 'Apply':
      return checkApply(analyzer, expr)
    case 'Unary':
      return checkUnary(analyzer, expr)
    case 'Binary':
      return checkBinary(analyzer, expr)
    case 'Block':
      return checkBlock(analyzer, expr)
    case 'If':
      return checkIf(analyzer, expr)
    case 'Tuple':
      return checkTuple(analyzer, expr)
    case 'List':
      return checkList(analyzer, expr)
    case 'Record':
      return checkRecord(analyzer, expr)
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
  const params = fn.params.map(param => {
    switch (param.kind) {
      case 'Unit':
        return node('Name', { value: '' }, fn.span)
      case 'Name':
        return param
      default:
        return error(analyzer, param.span, 'invalid pattern for a function parameter')
    }
  })

  const value = check(analyzer, fn.value)

  return params
    .reverse()
    .reduce((value, param) => node('Fn', { param, value }, param.span), value)
}

const checkDef = (analyzer, def) => {
  if (def.patt.kind !== 'Name') {
    return error(analyzer, span, 'pattern destructuring is not supported for now')
  }

  const value = check(analyzer, def.value)

  return node('Def', { patt: def.patt, value }, def.span)
}

const checkSet = (analyzer, set) => {
  switch (set.target.kind) {
    case 'Name': break
    case 'Get' : break
    default:
      return error(analyzer, set.span, 'mutable destructuring is not allowed')
  }

  const value = check(analyzer, set.value)

  return node('Set', { target: set.target, value }, set.span)
}

const checkGet = (analyzer, get) => {
  const expr = check(analyzer, get.expr)
  if (get.index) {
      get.index = check(analyzer, get.index)
  }

  return node('Get', { expr, index: get.index, name: get.name }, get.span)
}

const checkApply = (analyzer, app) => {
  app.fn  = check(analyzer, app.fn)
  app.arg = check(analyzer, app.arg)
  return app
}

const checkUnary = (analyzer, unary) => {
  const fn  = check(analyzer, unary.operator)
  const arg = check(analyzer, unary.rhs)

  return node('Apply', { fn, arg }, unary.span)
}

const checkBinary = (analyzer, binary) => {
  if (binary.operator.value === 'pipe') {
    return checkPipe(analyzer, binary)
  }

  const main = check(analyzer, binary.operator)

  const args = [
    check(analyzer, binary.lhs),
    check(analyzer, binary.rhs),
  ]

  return args
    .reduce((fn, arg) => node('Apply', { fn, arg }, arg.span), main)
}

const checkPipe = (analyzer, pipe) => {
  const arg = check(analyzer, pipe.lhs)
  const fn  = check(analyzer, pipe.rhs)

  return node('Apply', { fn, arg }, pipe.span)
}

const checkBlock = (analyzer, block) => {
  const items = block.items.map(item => check(analyzer, item))

  return node('Block', { items }, block.span)
}

const checkIf = (analyzer, cond) => {
  const test = check(analyzer, cond.test)
  const then = check(analyzer, cond.then)
  const otherwise = check(analyzer, cond.otherwise)

  return node('Cond', { test, then, otherwise }, cond.span)
}

const checkTuple = (analyzer, tuple) => {
  const items = tuple.items.map(item => check(analyzer, item))

  return node('List', { items }, tuple.span)
}

const checkList = (analyzer, list) => {
  const items = list.items.map(item => check(analyzer, item))

  return node('List', { items }, list.span)
}

const checkRecord = (analyzer, record) => {
  const properties = record.properties.map(prop => {
    return { name: prop.name, value: check(analyzer, prop.value) }
  })

  return node('Record', { properties }, record.span)
}

const checkSymbol = (analyzer, symbol) => {
  const values = symbol.values.map(value => check(analyzer, value))

  if (analyzer.constructors[symbol.name] === undefined) {
      analyzer.constructors[symbol.name] = symbol
  }

  return node('Symbol', { name: symbol.name, values }, symbol.span)
}

const checkTemplate = (analyzer, template) => {
  const elements = template.elements.map(elem => check(analyzer, elem))

  return node('Template', { elements }, template.span)
}

const constructor = (analyzer, symbol) => {
  return node('Constructor', { name: symbol.name }, symbol.span)
}

const definition = (analyzer, name, def) => {
  if (Array.isArray(def)) {
    return combine()
  } else {
    return checkDef(analyzer, def)
  }
}

const error = (analyzer, span, msg) => {
  throw `Error [${span.lineno}] ${msg}`
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