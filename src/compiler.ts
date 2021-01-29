import * as AST from './syntax'

const INDENT_SPACES = 4

interface Compiler {
  buffer: string // will be used for formatting
  indent: number // will be used for formatting
  newline: boolean // will be used for formatting
  module: AST.Module
}

const compiler = (module: AST.Module): Compiler => ({
  module,
  buffer: '',
  indent: 0,
  newline: true,
})


const compileExpr = (compiler: Compiler, expr: AST.Expr): string => {
  switch (expr.kind) {
    case 'Function':
      return compileFun(compiler, expr)
    case 'Name':
      return compileName(compiler, expr)
    case 'Operator':
      return compileOperator(compiler, expr)
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
    case 'Variant':
      return compileVariant(compiler, expr)
    case 'Template':
      return compileTemplate(compiler, expr)
    case 'Float':
    case 'Integer':
      return compileNumber(compiler, expr as AST.Literal<number>)
    case 'String':
      return compileString(compiler, expr as AST.Literal<string>)
  }
}

const compileFun = (compiler: Compiler, fun: AST.Fun): string => {
  return `function(${fun.params[0].value}) {${compileBody(compiler, fun.value)}}`
}

const compileBody = (compiler: Compiler, body: AST.Expr): string => {
  let value = compileExpr(compiler, body);

  switch (body.kind) {
    case 'Let': break
    case 'Mut': break
    default:
      value = `return ${value};`
  }

  return value
}

const compileName = (compiler: Compiler, name: AST.Name): string => {
  return name.value
}

const compileOperator = (compiler: Compiler, app: AST.Operator): string => {
  return app.operator
}

const compileLet = (compiler: Compiler, decl: AST.Let): string => {
  if (decl.pattern.kind !== 'Name') {
    return error(compiler, decl, `'${decl.pattern.kind}' destructuring is not supported for now`)
  }
  return `var ${decl.pattern.value} = ${compileExpr(compiler, decl.value)};`
}

const compileMut = (compiler: Compiler, decl: AST.Mut): string => {
  if (decl.pattern.kind !== 'Name') {
    return error(compiler, decl, 'Mutable destructuring is not supported')
  }
  return `var ${decl.pattern.value} = ${compileExpr(compiler, decl.value)};`
}

const compileApply = (compiler: Compiler, app: AST.Apply): string => {
  const args = app.args.map(arg => `(${compileExpr(compiler, arg)})`)
  return `${compileExpr(compiler, app.fun)}${args.join('')}`
}

const compileBlock = (compiler: Compiler, block: AST.Block): string => {
  const items = block.items.map(item => `${compileExpr(compiler, item)};`)
  const last  = items[items.length - 1]
  items[items.length - 1] = `return ${last}`

  return `(function() {${items.join('\n')}})()`
}

const compileIf = (compiler: Compiler, cond: AST.If): string => {
  const { test, then, otherwise } = cond
  return `(${compileExpr(compiler, test)} ? ${compileExpr(compiler, then)} : ${compileExpr(compiler, otherwise)})`
}

const compileVariant = (compiler: Compiler, variant: AST.Variant): string => {
  switch (variant.name.value) {
    case 'True':  return 'true'
    case 'False': return 'false'
    default:
      return error(compiler, variant, `variants are not supported for now`)
  }
}

const compileGroup = (compiler: Compiler, group: AST.Group): string => {
  return `${compileExpr(compiler, group.inner)}`
}

const compileList = (compiler: Compiler, seq: AST.List): string => {
  return `[${seq.items.map(item => compileExpr(compiler, item)).join(', ')}]`
}

const compileTuple = (compiler: Compiler, tuple: AST.Tuple): string => {
  return `[${tuple.items.map(item => compileExpr(compiler, item)).join(', ')}]`
}

const compileRecord = (compiler: Compiler, record: AST.Record): string => {
  const props = record.props.map(prop => `${prop.name.value}: ${prop.value ? compileExpr(compiler, prop.value) : prop.name.value}`)
  return `{ ${props.join(', ')} }`
}

const compileMember = (compiler: Compiler, member: AST.Member): string => {
  return `${compileExpr(compiler, member.main)}.${member.property.value}`
}

const compileTemplate = (compiler: Compiler, template: AST.Template): string => {
  const parts = template.elements.map(el => {
    if (el.kind === 'String') {
      return `'${el.value}'`
    } else {
      return `(${compileExpr(compiler, el)}).toString()`
    }
  })
  return `${parts.join(' + ')}`
}

const compileNumber = (compiler: Compiler, literal: AST.Literal<number>): string => {
  return literal.value.toString()
}

const compileString = (compiler: Compiler, literal: AST.Literal<string>): string => {
  return `'${literal.value}'`
}

const emit = (compiler: Compiler, str: string) => {
  if (compiler.newline) {
    compiler.buffer += ' '.repeat(compiler.indent)
    compiler.newline = false
  }
  compiler.buffer += str
}

const line = (compiler: Compiler) => {
  compiler.newline = true
  compiler.buffer += '\n'
}

const indent = (compiler: Compiler) => {
  compiler.indent += INDENT_SPACES
}

const dedent = (compiler: Compiler) => {
  compiler.indent -= INDENT_SPACES
}

const error = (compiler: Compiler, expr: AST.Expr, msg?: string) => {
  throw `Error [${expr.span.lineno}]: ${msg}`
}

export const compile = (module: AST.Module): string => {
  const cc = compiler(module)

  const nodes = module.nodes.map(node => compileExpr(cc, node))

  return nodes.join('\n')
}