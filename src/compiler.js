const AST = require('./ast')

const INDENT_SPACES = 4

const compiler = (module) => ({
  module,
  buffer: [],
  indent: 0,
  newline: false,
})


const compileExpr = (compiler, expr) => {
  switch (expr.kind) {
    case 'Function':
      return compileFun(compiler, expr)
    case 'Name':
      return compileName(compiler, expr)
    case 'Let':
      return compileLet(compiler, expr)
    case 'Mut':
      return compileMut(compiler, expr)
    case 'Apply':
      return compileApply(compiler, expr)
    case 'Block':
      return compileBlock(compiler, expr)
    case 'If':
      return compileIf(compiler, expr)
    case 'Group':
      return compileGroup(compiler, expr)
    case 'Tuple':
      return compileTuple(compiler, expr)
    case 'List':
      return compileList(compiler, expr)
    case 'Record':
      return compileRecord(compiler, expr)
    case 'Member':
      return compileMember(compiler, expr)
    case 'Symbol':
      return compileSymbol(compiler, expr)
    case 'Number':
      return compileNumber(compiler, expr)
    case 'String':
      return compileString(compiler, expr)
    case 'Template':
      return compileTemplate(compiler, expr)
  }
}

const compileFun = (compiler, fun) => {
  return `function(${fun.params[0].value}) { ${compileBody(compiler, fun.value)} }`
}

const compileBody = (compiler, body) => {
  let value = compileExpr(compiler, body);

  switch (body.kind) {
    case 'Let': break
    case 'Mut': break
    default:
      value = `return ${value};`
  }

  return value
}

const compileName = (compiler, name) => {
  return name.value
}

const compileLet = (compiler, decl) => {
  if (!(decl.pattern.kind === 'Name')) {
    return error(compiler, decl, `'${decl.pattern.kind}' destructuring is not supported for now`)
  }
  return `var ${decl.pattern.value} = ${compileExpr(compiler, decl.value)}`
}

const compileMut = (compiler, decl) => {
  if (!(decl.pattern.kind === 'Name')) {
    return error(compiler, decl, 'Mutable destructuring is not supported')
  }
  return `var ${decl.pattern.value} = ${compileExpr(compiler, decl.value)}`
}

const compileApply = (compiler, app) => {
  const args = app.args.map(arg => `(${compileExpr(compiler, arg)})`)
  return `${compileExpr(compiler, app.fun)}${args.join('')}`
}

const compileBlock = (compiler, block) => {
  const items = block.items.map(item => `${compileExpr(compiler, item)};`)
  const last  = items.pop()
  items.push(`return ${last}`)

  return `(function() { ${items.join('')} })()`
}

const compileIf = (compiler, cond) => {
  const { test, then, otherwise } = cond
  return `(${compileExpr(compiler, test)} ? ${compileExpr(compiler, then)} : ${compileExpr(compiler, otherwise)})`
}

const compileSymbol = (compiler, symbol) => {
  switch (symbol.name.value) {
    case 'True':  return 'true'
    case 'False': return 'false'
    default:
      return error(compiler, symbol, `symbols are not supported for now`)
  }
}

const compileGroup = (compiler, group) => {
  return `${compileExpr(compiler, group.inner)}`
}

const compileList = (compiler, seq) => {
  return `[${seq.items.map(item => compileExpr(compiler, item)).join(', ')}]`
}

const compileTuple = (compiler, tuple) => {
  return `[${tuple.items.map(item => compileExpr(compiler, item)).join(', ')}]`
}

const compileRecord = (compiler, record) => {
  const props = record.props.map(prop => `${prop.name.value} : ${compileExpr(compiler, prop.value)}`)
  return `{ ${props.join(', ')} }`
}

const compileMember = (compiler, member) => {
  return `${compileExpr(compiler, member.main)}.${member.property.value}`
}

const compileTemplate = (compiler, template) => {
  const parts = template.elements.map(element => {
    if (element.kind === 'String') {
      return `'${element.value}'`
    } else {
      return `(${compileExpr(compiler, element)}).toString()`
    }
  })
  return `${parts.join(' + ')}`
}

const compileString = (compiler, literal) => {
  return `'${literal.value}'`
}

const compileNumber = (compiler, literal) => {
  return literal.value
}

const emit = (compiler, str) => {
  if (compiler.newline) {
    compiler.buffer += ' '.repeat(compiler.indent)
    compiler.newline = false
  }
  compiler.buffer += str
}

const line = (compiler) => {
  compiler.newline = true
  compiler.buffer += '\n'
}

const indent = (compiler) => {
  compiler.indent += INDENT_SPACES
}

const dedent = (compiler) => {
  compiler.indent -= INDENT_SPACES
}

const error = (compiler, expr, msg) => {
  throw `Error [${expr.span.lineno}] ${msg}`
}

exports.compile = (module) => {
  const cc = compiler(module)

  const nodes = module.nodes.map(node => compileExpr(cc, node))

  return nodes.join('\n')
}