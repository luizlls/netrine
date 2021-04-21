const INDENT_SPACES = 4

const compiler = (module) => ({
  module,
  buffer: [],
  indent: 0,
  newline: false,
})


const compile = (compiler, expr) => {
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
    case 'Dict':
      return compileDict(compiler, expr)
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
  let value = compile(compiler, body)

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
  const name  = compile(compiler, def.name)
  const value = compile(compiler, def.value)
  return `var ${name} = ${value}`
}

const compileSet = (compiler, set) => {
  const target = compile(compiler, set.target)
  const value  = compile(compiler, set.value)
  return `${target} = ${value}`
}

const compileGet = (compiler, get) => {
  const target = compile(compiler, get.expr)
  if (get.index !== undefined) {
    return `${target}[${compile(compiler, get.index)}]`
  } else {
    return `${target}.${get.name.value}`
  }
}

const compileApply = (compiler, app) => {
  const fn  = compile(compiler, app.fn)
  const arg = compile(compiler, app.arg)
  return `${fn}(${arg})`
}

const compileBlock = (compiler, block) => {
  const items = block.items.map(item => `${compile(compiler, item)};`)
  const last  = items.pop()
  items.push(`return ${last}`)

  return `(function() { ${items.join(' ')} })()`
}

const compileCond = (compiler, cond) => {
  const conditions = cond
    .conditions
    .map(cond => {
      const test = compile(compiler, cond.test)
      const then = compile(compiler, cond.then)
      return `if (${test}) { return ${then}; }`
    })

  const otherwise = compile(compiler, cond.otherwise)

  return `(function() { ${conditions.join(' else ')} else { return ${otherwise} } })()`
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
      const values = symbol.values.map(value => compile(compiler, value))
      return `new ${symbol.name.value}(${values})`
    }
  }
}

const compileList = (compiler, seq) => {
  return `[${seq.items.map(item => compile(compiler, item)).join(', ')}]`
}

const compileDict = (compiler, dict) => {
  const items = dict.items.map(item => {
    let key
    switch (item.key.kind) {
      case 'Name':
        key = item.key.value
        break
      case 'String':
        key = compileString(compiler, item.key)
        break
      default:
        key = `[${compile(compiler, item.key)}]`
    }

    const value = compile(compiler, item.value)

    return `${key} : ${value}`
  })
  return `{${items.join(', ')}}`
}

const compileTemplate = (compiler, template) => {
  const parts = template.elements.map(element => {
    if (element.kind === 'String') {
      return `'${element.value}'`
    } else {
      return `(${compile(compiler, element)}).toString()`
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
  throw `Error [${expr.meta.line}] ${msg}`
}

exports.compile = (module) => {
  const cc = compiler(module)

  const nodes = module.nodes.map(node => compile(cc, node))

  return nodes.join('\n')
}