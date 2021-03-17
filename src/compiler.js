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
    case 'Get':
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
    case 'Constructor':
      return compileConstructor(compiler, expr)
    case 'Symbol':
      return compileSymbol(compiler, expr)
    case 'Number':
      return compileNumber(compiler, expr)
    case 'String':
      return compileString(compiler, expr)
    case 'Template':
      return compileTemplate(compiler, expr)
    case 'Unit':
      return ''
  }
}

const compileFn = (compiler, fn) => {
  return `function(${fn.param.value}) { ${compileBody(compiler, fn.value)} }`
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
  const name  = compileExpr(compiler, def.patt)
  const value = compileExpr(compiler, def.value)
  return `var ${name} = ${value};`
}

const compileSet = (compiler, set) => {
  const target = compileExpr(compiler, set.target)
  const value  = compileExpr(compiler, set.value)
  return `${target} = ${value};`
}

const compileGet = (compiler, get) => {
  const target = compileExpr(compiler, get.expr)
  if (get.index !== undefined) {
    return `${target}[${compileExpr(compiler, get.index)}]`
  } else {
    return `${target}.${get.name.value}`
  }
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
  const _test = compileExpr(compiler, test)
  const _then = compileExpr(compiler, then)
  const _else = compileExpr(compiler, otherwise)
  return `(${_test} ? ${_then} : ${_else})`
}

const compileConstructor = (compiler, ctor) => {
  const build = `for (var i = 0; i < arguments.length; i++) { this['_' + i] = arguments[i]; }`
  const inner = `function ${ctor.name.value}() { ${build}; return this; }`
  return `var ${ctor.name.value} = (function() { ${inner}; return ${ctor.name.value}; })();`
}

const compileSymbol = (compiler, symbol) => {
  switch (symbol.name.value) {
    case 'True':  return 'true'
    case 'False': return 'false'
    default: {
      const values = symbol.values.map(value => compileExpr(compiler, value))
      return `new ${symbol.name.value}(${values})`
    }
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