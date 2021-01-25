import * as AST from './syntax'

const INDENT_SPACES = 4

interface Compiler {
  buffer: string
  indent: number
  newline: boolean
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
    case 'If':
      return compileIf(compiler, expr)
    case 'Tuple':
      return compileTuple(compiler, expr)
    case 'Sequence':
      return compileSequence(compiler, expr)
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
  return `return ${compileExpr(compiler, body)};`
}

const compileName = (compiler: Compiler, name: AST.Name): string => {
  return name.value
}

const compileOperator = (compiler: Compiler, app: AST.Operator): string => {
  return `core.${app.operator}`
}

const compileLet = (compiler: Compiler, le_: AST.Let): string => {
  return `var ${le_.name.value} = ${compileExpr(compiler, le_.value)};`
}

const compileMut = (compiler: Compiler, mut: AST.Mut): string => {
  return `var ${mut.name.value} = ${compileExpr(compiler, mut.value)};`
}

const compileApply = (compiler: Compiler, app: AST.Apply): string => {
  const args = app.args.map(arg => `(${compileExpr(compiler, arg)})`)

  return `${compileExpr(compiler, app.fun)}${args.join('')}`
}

const compileIf = (compiler: Compiler, cond: AST.If): string => {
  const { test, then, otherwise } = cond
  return `(${compileExpr(compiler, test)}) ? (${compileExpr(compiler, then)}) : (${compileExpr(compiler, otherwise)})`
}

const compileVariant = (compiler: Compiler, variant: AST.Variant): string => {
  switch (variant.name.value) {
    case 'True':  return 'true'
    case 'False': return 'false'
    default:
      return error(compiler, variant, `variants are not supported for now`)
  }
}

const compileSequence = (compiler: Compiler, seq: AST.Sequence): string => {
  return `[${seq.items.map(item => compileExpr(compiler, item)).join(', ')}]`
}

const compileTuple = (compiler: Compiler, tuple: AST.Tuple): string => {
  return `[${tuple.items.map(item => compileExpr(compiler, item)).join(', ')}]`
}

const compileRecord = (compiler: Compiler, record: AST.Record): string => {
  const props = record.props.map(prop => `${prop.name}: ${prop.value ? compileExpr(compiler, prop.value) : prop.name}`)
  return `{ ${props.join(', ')} }`
}

const compileMember = (compiler: Compiler, member: AST.Member): string => {
  return `${compileExpr(compiler, member.qualifier)}.${member.property}`
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
  return literal.value
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

  const buffer = []
  for (const node of module.nodes) {
    buffer.push(compileExpr(cc, node))
  }

  return buffer.join('\n')
}