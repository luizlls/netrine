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

const checkFun = (aa: Analyzer, fun: AST.Fun): AST.Fun => {
  if (fun.params.length === 0) {
      fun.params.push(AST.name('', fun.span))
  }

  const final = fun.params
    .reverse()
    .reduce((value, param) => AST.fn([param], value, param.span), checkExpr(aa, fun.value))

  return final as AST.Fun
}

const checkLet = (aa: Analyzer, expr: AST.Let): AST.Let => {
  if (expr.pattern.kind !== 'Name') {
    switch (expr.pattern.kind) {
      case 'Tuple':
      case 'List':
      case 'Record':
      case 'Variant':
        return error(aa, expr, `'${expr.pattern.kind}' destructuring is not supported for now`)
      default:
        return error(aa, expr, 'Invalid destructuring')
    }
  }

  expr.value = checkExpr(aa, expr.value)
  return expr
}

const checkMut = (aa: Analyzer, expr: AST.Mut): AST.Mut => {
  if (expr.pattern.kind !== 'Name') {
    return error(aa, expr, 'Mutable destructuring is not supported')
  }

  expr.value = checkExpr(aa, expr.value)
  return expr
}

const checkApply = (aa: Analyzer, app: AST.Apply): AST.Expr => {
  if (app.fun.kind === 'Name') {
    switch (app.fun.value) {
      case 'pipe':
        return checkPipe(aa, app)
    }
  }

  app.fun  = checkExpr(aa, app.fun)
  app.args = app.args.map(arg => checkExpr(aa, arg))

  return app
}

const checkPipe = (aa: Analyzer, app: AST.Apply): AST.Apply => {
  const [arg, callee] = app.args
  return AST.apply(callee, [checkExpr(aa, arg)], app.span)
}

const checkBlock = (aa: Analyzer, block: AST.Block): AST.Block => {
  block.items = block.items.map(item => checkExpr(aa, item))
  return block
}

const checkIf = (aa: Analyzer, cond: AST.If): AST.If => {
  cond.test = checkExpr(aa, cond.test)
  cond.then = checkExpr(aa, cond.then)
  cond.otherwise = checkExpr(aa, cond.otherwise)
  return cond
}

const checkGroup = (aa: Analyzer, group: AST.Group): AST.Group => {
  group.inner =  checkExpr(aa, group.inner)
  return group
}

const checkTuple = (aa: Analyzer, tuple: AST.Tuple): AST.Tuple => {
  tuple.items = tuple.items.map(item => checkExpr(aa, item))
  return tuple
}

const checkList = (aa: Analyzer, list: AST.List): AST.List => {
  list.items = list.items.map(item => checkExpr(aa, item))
  return list
}

const checkRecord = (aa: Analyzer, record: AST.Record): AST.Record => {
  record.props = record.props.map(prop => {
    if (prop.value) {
        prop.value = checkExpr(aa, prop.value)
    }
    return prop
  })
  return record
}

const checkMember = (aa: Analyzer, member: AST.Member): AST.Member => {
  member.main = checkExpr(aa, member.main)
  return member
}

const checkVariant = (aa: Analyzer, variant: AST.Variant): AST.Variant => {
  variant.values = variant.values?.map(value => checkExpr(aa, value))
  return variant
}

const checkTemplate = (aa: Analyzer, template: AST.Template): AST.Template => {
  template.elements = template.elements.map(element => checkExpr(aa, element))
  return template
}

const error = (aa: Analyzer, expr: AST.Expr, msg?: string) => {
  throw `Error [${expr.span.lineno}]: ${msg}`
}

export const analyze = (module: AST.Module): AST.Module => {
  const aa = analyzer(module)

  const nodes = module.nodes.map(node => checkExpr(aa, node))

  return { nodes }
}