import * as AST from './syntax'

interface Desugarer {
  module: AST.Module
}

const desugarer = (module: AST.Module): Desugarer => ({
  module,
})

const desugarExpr = (desugarer: Desugarer, expr: AST.Expr): AST.Expr => {
  switch (expr.kind) {
    case 'Function':
      return desugarFun(desugarer, expr)
    case 'Let':
      return desugarLet(desugarer, expr)
    case 'Mut':
      return desugarMut(desugarer, expr)
    case 'Apply':
      return desugarApply(desugarer, expr)
    case 'Block':
      return desugarBlock(desugarer, expr)
    case 'If':
      return desugarIf(desugarer, expr)
    case 'Tuple':
      return desugarTuple(desugarer, expr)
    case 'List':
      return desugarList(desugarer, expr)
    case 'Record':
      return desugarRecord(desugarer, expr)
    case 'Member':
      return desugarMember(desugarer, expr)
    case 'Variant':
      return desugarVariant(desugarer, expr)
    case 'Template':
      return desugarTemplate(desugarer, expr)
    default:
      return expr
  }
}

const desugarFun = (dd: Desugarer, fun: AST.Fun): AST.Fun => {
  if (fun.params.length === 0) {
      fun.params.push({ kind: 'Name', value: '', span: fun.span })
  }

  const final = fun.params.reverse().reduce((value, param) => {
    return { kind: 'Function', params: [param], value, span: param.span } as AST.Fun
  }, desugarExpr(dd, fun.value))

  return final as AST.Fun
}

const desugarLet = (dd: Desugarer, expr: AST.Let): AST.Let => {
  expr.value = desugarExpr(dd, expr.value)
  return expr
}

const desugarMut = (dd: Desugarer, expr: AST.Mut): AST.Mut => {
  expr.value = desugarExpr(dd, expr.value)
  return expr
}

const desugarApply = (dd: Desugarer, app: AST.Apply): AST.Expr => {
  if (app.fun.kind === 'Operator') {
    switch (app.fun.operator) {
      case 'pipe':
        return desugarPipe(dd, app)
      case 'equals':
        return desugarEquals(dd, app)
      case 'walrus':
        return desugarWalrus(dd, app)
      case 'semi':
        return desugarChain(dd, app)
    }
  }

  app.fun  = desugarExpr(dd, app.fun)
  app.args = app.args.map(arg => desugarExpr(dd, arg))

  return app
}

const desugarPipe = (dd: Desugarer, app: AST.Apply): AST.Apply => {
  const [arg, callee] = app.args
  return { kind: 'Apply', fun: callee, args: [desugarExpr(dd, arg)], span: app.span }
}

const desugarEquals = (dd: Desugarer, app: AST.Apply): AST.Let => {
  const [pattern, value] = app.args

  if (pattern.kind !== 'Name') {
    switch (pattern.kind) {
      case 'Tuple':
      case 'List':
      case 'Record':
      case 'Variant':
        return error(dd, app, `'${pattern.kind}' destructuring is not supported for now`)
      default:
        return error(dd, app, 'Invalid destructuring')
    }
  }

  return { kind: 'Let', name: pattern, value: desugarExpr(dd, value), span: app.span }
}

const desugarWalrus = (dd: Desugarer, app: AST.Apply): AST.Mut => {
  const [pattern, value] = app.args

  if (pattern.kind !== 'Name') {
    return error(dd, app, 'Mutable destructuring is not supported')
  }

  return { kind: 'Mut', name: pattern, value: desugarExpr(dd, value), span: app.span }
}

const desugarChain = (dd: Desugarer, app: AST.Apply): AST.Block => {
  const items = []

  let curr: AST.Expr = app

  while (curr.kind === 'Apply' && curr.fun.kind === 'Operator' && curr.fun.operator === 'semi') {
    const [expr, next] = curr.args
    curr = next as AST.Expr
    items.push(desugarExpr(dd, expr))
  }

  items.push(desugarExpr(dd, curr))

  return { kind: 'Block', items, span: app.span }
}

const desugarBlock = (dd: Desugarer, block: AST.Block): AST.Block => {
  block.items = block.items.map(item => desugarExpr(dd, item))
  return block
}

const desugarIf = (dd: Desugarer, cond: AST.If): AST.If => {
  cond.test = desugarExpr(dd, cond.test)
  cond.then = desugarExpr(dd, cond.then)
  cond.otherwise = desugarExpr(dd, cond.otherwise)
  return cond
}

const desugarTuple = (dd: Desugarer, tuple: AST.Tuple): AST.Tuple => {
  tuple.items = tuple.items.map(item => desugarExpr(dd, item))
  return tuple
}

const desugarList = (dd: Desugarer, list: AST.List): AST.List => {
  list.items = list.items.map(item => desugarExpr(dd, item))
  return list
}

const desugarRecord = (dd: Desugarer, record: AST.Record): AST.Record => {
  record.props = record.props.map(prop => {
    if (prop.value) {
        prop.value = desugarExpr(dd, prop.value)
    }
    return prop
  })
  return record
}

const desugarMember = (dd: Desugarer, member: AST.Member): AST.Member => {
  member.qualifier = desugarExpr(dd, member.qualifier)
  return member
}

const desugarVariant = (dd: Desugarer, variant: AST.Variant): AST.Variant => {
  variant.values = variant.values?.map(value => desugarExpr(dd, value))
  return variant
}

const desugarTemplate = (dd: Desugarer, template: AST.Template): AST.Template => {
  template.elements = template.elements.map(element => desugarExpr(dd, element))
  return template
}

const error = (dd: Desugarer, expr: AST.Expr, msg?: string) => {
  throw `Error [${expr.span.lineno}]: ${msg}`
}

export const desugar = (module: AST.Module): AST.Module => {
  const dd = desugarer(module)

  const nodes = module.nodes.map(node => desugarExpr(dd, node))

  return { nodes }
}