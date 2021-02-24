const analyzer = (module) => ({
  module,
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
    case 'Group':
      return checkGroup(analyzer, expr)
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

const checkName = (aa, name) => {
  // const item = find(ctx, name)

  // if (!item) {
  //   return error(aa, name, `Cannot find value '${name}' in this scope`)
  // }

  return name
}

const checkFn = (aa, fn) => {
  if (fn.params.length === 0) {
      fn.params.push(node('Name', { value: '' }, fn.span))
  }

  const value = check(aa, fn.value)

  return fn.params
    .reverse()
    .reduce((value, param) => node('Fn', { param, value }, param.span), value)
}

const checkDef = (aa, def) => {
  if (def.pattern.kind !== 'Name') {
    return error(aa, span, 'destructuring is not supported for now')
  }

  const value = check(aa, def.value)

  return node('Def', { pattern:def.pattern, value }, def.span)
}

const checkSet = (aa, set) => {
  if (set.pattern.kind !== 'Name') {
    return error(aa, span, 'mutable destructuring is not allowed')
  }

  const value = check(aa, set.value)

  return node('Set', { pattern: set.pattern, value }, set.span)
}

const checkGet = (aa, get) => {
  const expr = check(aa, get.expr)
  return node('Get', { expr, name: get.name }, get.span)
}

const checkApply = (aa, app) => {
  const args = app.args.map(arg => check(aa, arg))

  const main = check(aa, app.fn)

  return args
    .reduce((fn, arg) => node('Apply', { fn, arg }, arg.span), main)
}

const checkUnary = (aa, unary) => {
  const fn  = check(aa, unary.operator)
  const arg = check(aa, unary.rhs)

  return node('Apply', { fn, arg }, unary.span)
}

const checkBinary = (aa, binary) => {
  if (binary.operator.value === 'pipe') {
    return checkPipe(aa, binary)
  }

  const main = check(aa, binary.operator)

  const args = [
    check(aa, binary.lhs),
    check(aa, binary.rhs),
  ]

  return args
    .reduce((fn, arg) => node('Apply', { fn, arg }, arg.span), main)
}

const checkPipe = (aa, pipe) => {
  const arg = check(aa, pipe.lhs)
  const fn  = check(aa, pipe.rhs)

  return node('Apply', { fn, arg }, pipe.span)
}

const checkBlock = (aa, block) => {
  const items = block.items.map(item => check(aa, item))

  return node('Block', { items }, block.span)
}

const checkIf = (aa, cond) => {
  const test = check(aa, cond.test)
  const then = check(aa, cond.then)
  const otherwise = check(aa, cond.otherwise)

  return node('Cond', { test, then, otherwise }, cond.span)
}

const checkGroup = (aa, group) => {
  return check(aa, group.inner)
}

const checkTuple = (aa, tuple) => {
  const items = tuple.items.map(item => check(aa, item))

  return node('List', { items }, tuple.span)
}

const checkList = (aa, list) => {
  const items = list.items.map(item => check(aa, item))

  return node('List', { items }, list.span)
}

const checkRecord = (aa, record) => {
  const properties = record.properties.map(prop => {
    return { name: prop.name, value: check(aa, prop.value) }
  })

  return node('Record', { properties }, record.span)
}

const checkSymbol = (aa, symbol) => {
  const values = symbol.values.map(value => check(aa, value))

  return node('Symbol', { name: symbol.name, values }, symbol.span)
}

const checkTemplate = (aa, template) => {
  const elements = template.elements.map(elem => check(aa, elem))

  return node('Template', { elements }, template.span)
}

const error = (aa, span, msg) => {
  throw `Error [${span.lineno}] ${msg}`
}

exports.analyze = (module) => {
  const aa = analyzer(module)
  const nodes = module.nodes.map(node => check(aa, node))

  return { nodes }
}