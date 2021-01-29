import * as AST from './syntax'

interface Analyzer {
  module: AST.Module
}

const analyzer = (module: AST.Module): Analyzer => ({
  module,
})

const checkExpr = (analyzer: Analyzer, expr: AST.Expr): AST.Expr => {
  switch (expr.kind) {
    case 'Function':
      return checkFun(analyzer, expr)
    case 'Let':
      return checkLet(analyzer, expr)
    case 'Mut':
      return checkMut(analyzer, expr)
    case 'Apply':
      return checkApply(analyzer, expr)
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
    case 'Member':
      return checkMember(analyzer, expr)
    case 'Variant':
      return checkVariant(analyzer, expr)
    case 'Template':
      return checkTemplate(analyzer, expr)
    default:
      return expr
  }
}

const checkFun = (dd: Analyzer, fun: AST.Fun): AST.Fun => {
  if (fun.params.length === 0) {
      fun.params.push({ kind: 'Name', value: '', span: fun.span })
  }

  const final = fun.params.reverse().reduce((value, param) => {
    return { kind: 'Function', params: [param], value, span: param.span } as AST.Fun
  }, checkExpr(dd, fun.value))

  return final as AST.Fun
}

const checkLet = (dd: Analyzer, expr: AST.Let): AST.Let => {
  if (expr.pattern.kind !== 'Name') {
    switch (expr.pattern.kind) {
      case 'Tuple':
      case 'List':
      case 'Record':
      case 'Variant':
        return error(dd, expr, `'${expr.pattern.kind}' destructuring is not supported for now`)
      default:
        return error(dd, expr, 'Invalid destructuring')
    }
  }

  expr.value = checkExpr(dd, expr.value)
  return expr
}

const checkMut = (dd: Analyzer, expr: AST.Mut): AST.Mut => {
  if (expr.pattern.kind !== 'Name') {
    return error(dd, expr, 'Mutable destructuring is not supported')
  }

  expr.value = checkExpr(dd, expr.value)
  return expr
}

const checkApply = (dd: Analyzer, app: AST.Apply): AST.Expr => {
  if (app.fun.kind === 'Operator') {
    switch (app.fun.operator) {
      case 'pipe':
        return checkPipe(dd, app)
      case 'semi':
        return checkChain(dd, app)
    }
  }

  app.fun  = checkExpr(dd, app.fun)
  app.args = app.args.map(arg => checkExpr(dd, arg))

  return app
}

const checkPipe = (dd: Analyzer, app: AST.Apply): AST.Apply => {
  const [arg, callee] = app.args
  return { kind: 'Apply', fun: callee, args: [checkExpr(dd, arg)], span: app.span }
}

const checkChain = (dd: Analyzer, app: AST.Apply): AST.Block => {
  const items = []

  let curr: AST.Expr = app

  while (curr.kind === 'Apply' && curr.fun.kind === 'Operator' && curr.fun.operator === 'semi') {
    const [expr, next] = curr.args
    curr = next as AST.Expr
    items.push(checkExpr(dd, expr))
  }

  items.push(checkExpr(dd, curr))

  return { kind: 'Block', items, span: app.span }
}

const checkBlock = (dd: Analyzer, block: AST.Block): AST.Block => {
  block.items = block.items.map(item => checkExpr(dd, item))
  return block
}

const checkIf = (dd: Analyzer, cond: AST.If): AST.If => {
  cond.test = checkExpr(dd, cond.test)
  cond.then = checkExpr(dd, cond.then)
  cond.otherwise = checkExpr(dd, cond.otherwise)
  return cond
}

const checkGroup = (dd: Analyzer, group: AST.Group): AST.Group => {
  group.inner =  checkExpr(dd, group.inner)
  return group
}

const checkTuple = (dd: Analyzer, tuple: AST.Tuple): AST.Tuple => {
  tuple.items = tuple.items.map(item => checkExpr(dd, item))
  return tuple
}

const checkList = (dd: Analyzer, list: AST.List): AST.List => {
  list.items = list.items.map(item => checkExpr(dd, item))
  return list
}

const checkRecord = (dd: Analyzer, record: AST.Record): AST.Record => {
  record.props = record.props.map(prop => {
    if (prop.value) {
        prop.value = checkExpr(dd, prop.value)
    }
    return prop
  })
  return record
}

const checkMember = (dd: Analyzer, member: AST.Member): AST.Member => {
  member.main = checkExpr(dd, member.main)
  return member
}

const checkVariant = (dd: Analyzer, variant: AST.Variant): AST.Variant => {
  variant.values = variant.values?.map(value => checkExpr(dd, value))
  return variant
}

const checkTemplate = (dd: Analyzer, template: AST.Template): AST.Template => {
  template.elements = template.elements.map(element => checkExpr(dd, element))
  return template
}

const error = (dd: Analyzer, expr: AST.Expr, msg?: string) => {
  throw `Error [${expr.span.lineno}]: ${msg}`
}

export const analyze = (module: AST.Module): AST.Module => {
  const aa = analyzer(module)

  const nodes = module.nodes.map(node => checkExpr(aa, node))

  return { nodes }
}