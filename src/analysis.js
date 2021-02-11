const AST = require('./ast')

const analyzer = (module) => ({
  module,
})

const checkExpr = (analyzer, expr) => {
  switch (expr.kind) {
    case 'Function':
      return checkFun(analyzer, expr)
    case 'Let':
      return checkLet(analyzer, expr)
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
    case 'Symbol':
      return checkSymbol(analyzer, expr)
    case 'Template':
      return checkTemplate(analyzer, expr)
    default:
      return expr
  }
}

const checkFun = (aa, fun) => {
  if (fun.params.length === 0) {
      fun.params.push(AST.name('', fun.span))
  }

  const final = fun.params
    .reverse()
    .reduce((value, param) => AST.fn([param], value, param.span), checkExpr(aa, fun.value))

  return final
}

const checkLet = (aa, expr) => {
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

const checkMut = (aa, expr) => {
  if (expr.pattern.kind !== 'Name') {
    return error(aa, expr, 'Mutable destructuring is not supported')
  }

  expr.value = checkExpr(aa, expr.value)
  return expr
}

const checkApply = (aa, app) => {
  app.fun  = checkExpr(aa, app.fun)
  app.args = app.args.map(arg => checkExpr(aa, arg))

  return app
}

const checkUnary = (aa, unary) => {
  unary.op  = checkExpr(aa, unary.op)
  unary.rhs = checkExpr(aa, unary.rhs)

  return AST.apply(unary.op, [unary.rhs], unary.span)
}

const checkBinary = (aa, binary) => {
  if (binary.op.value === 'pipe') {
    return checkPipe(aa, binary)
  }

  binary.op  = checkExpr(aa, binary.op)
  binary.lhs = checkExpr(aa, binary.lhs)
  binary.rhs = checkExpr(aa, binary.rhs)

  return AST.apply(binary.op, [binary.lhs, binary.rhs], binary.span)
}

const checkPipe = (aa, pipe) => {
  pipe.lhs = checkExpr(aa, pipe.lhs)
  pipe.rhs = checkExpr(aa, pipe.rhs)

  return AST.apply(pipe.rhs, [pipe.lhs], pipe.span)
}

const checkBlock = (aa, block) => {
  block.items = block.items.map(item => checkExpr(aa, item))
  return block
}

const checkIf = (aa, cond) => {
  cond.test = checkExpr(aa, cond.test)
  cond.then = checkExpr(aa, cond.then)
  cond.otherwise = checkExpr(aa, cond.otherwise)
  return cond
}

const checkGroup = (aa, group) => {
  group.inner =  checkExpr(aa, group.inner)
  return group
}

const checkTuple = (aa, tuple) => {
  tuple.items = tuple.items.map(item => checkExpr(aa, item))
  return tuple
}

const checkList = (aa, list) => {
  list.items = list.items.map(item => checkExpr(aa, item))
  return list
}

const checkRecord = (aa, record) => {
  record.props = record.props.map(prop => {
    if (prop.value) {
        prop.value = checkExpr(aa, prop.value)
    }
    return prop
  })
  return record
}

const checkMember = (aa, member) => {
  member.main = checkExpr(aa, member.main)
  return member
}

const checkSymbol = (aa, symbol) => {
  symbol.values = symbol.values.map(value => checkExpr(aa, value))
  return symbol
}

const checkTemplate = (aa, template) => {
  template.elements = template.elements.map(el => checkExpr(aa, el))
  return template
}

const error = (aa, expr, msg) => {
  throw `Error [${expr.span.lineno}]}`
}

exports.analyze = (module) => {
  const aa = analyzer(module)

  const nodes = module.nodes.map(node => checkExpr(aa, node))

  return { nodes }
}