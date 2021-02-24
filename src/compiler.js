const INDENT_SPACES = 4

const compiler = (module) => ({
  module,
  buffer: [],
  indent: 0,
  newline: false,
})


const compileExpr = (compiler, expr) => {
  switch (expr.kind) {
    case 'Fn':
      return compileFn(compiler, expr)
    case 'Name':
      return compileName(compiler, expr)
    case 'Def':
      return compileDef(compiler, expr)
    case 'Set':
      return compileSet(compiler, expr)
    case 'Member':
      return compileGet(compiler, expr)
    case 'Apply':
      return compileApply(compiler, expr)
    case 'Block':
      return compileBlock(compiler, expr)
    case 'Cond':
      return compileCond(compiler, expr)
    case 'List':
      return compileList(compiler, expr)
    case 'Record':
      return compileRecord(compiler, expr)
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

const compileFn = (compiler, fun) => {
  return `function(${fun.param.value}) { ${compileBody(compiler, fun.value)} }`
}

const compileBody = (compiler, body) => {
  let value = compileExpr(compiler, body);

  switch (body.kind) {
    case 'Def': break
    case 'Set': break
    default:
      value = `return ${value};`
  }

  return value
}

const compileName = (compiler, name) => {
  return name.value
}

const compileDef = (compiler, def) => {
  return `var ${def.pattern.value} = ${compileExpr(compiler, def.value)};`
}

const compileSet = (compiler, set) => {
  return     `${set.pattern.value} = ${compileExpr(compiler, set.value)};`
}

const compileGet = (compiler, member) => {
  return `${compileExpr(compiler, member.main)}.${member.property.value}`
}

const compileApply = (compiler, app) => {
  const fn  = compileExpr(compiler, app.fn)
  const arg = compileExpr(compiler, app.arg)
  return `${fn}(${arg})`
}

const compileBlock = (compiler, block) => {
  const items = block.items.map(item => `${compileExpr(compiler, item)};`)
  const last  = items.pop()
  items.push(`return ${last}`)

  return `(function() { ${items.join(' ')} })()`
}

const compileCond = (compiler, cond) => {
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

const compileList = (compiler, seq) => {
  return `[${seq.items.map(item => compileExpr(compiler, item)).join(', ')}]`
}

const compileRecord = (compiler, record) => {
  const props = record.properties.map(prop => {
    return `${prop.name.value} : ${compileExpr(compiler, prop.value)}`
  })
  return `{ ${props.join(', ')} }`
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