import * as Syntax from './syntax'

interface Compiler {
  module: Syntax.Module
  buffer: string[]
  indent: number
  newline: boolean
}

const compiler = (module: Syntax.Module) => ({
  module,
  buffer: [],
  indent: 0,
  newline: false,
})


const compileExpr = (compiler: Compiler, expr: Syntax.Expr, fullBlock = true): string => {
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
    case 'Mut':
      return compileMut(compiler, expr)
    case 'Apply':
      return compileApply(compiler, expr)
    case 'Block':
      return compileBlock(compiler, expr, fullBlock)
    case 'Cond':
      return compileCond(compiler, expr, fullBlock)
    case 'List':
      return compileList(compiler, expr)
    case 'Dict':
      return compileDict(compiler, expr)
    case 'Variant':
      return compileVariant(compiler, expr)
    case 'Number':
      return compileNumber(compiler, expr)
    case 'String':
      return compileString(compiler, expr)
    case 'Raise':
      return compileRaise(compiler, expr)
    case 'Native':
      return compileNative(compiler, expr)
    case 'True':
      return 'true'
    case 'False':
      return 'false'
    case 'Any':
      return '_'
    case 'Unit':
      return ''
    default:
      throw new Error(`[${expr.meta?.line}] Internal Compiler Error: Unhandled \`${expr.kind}\` during compilation`)
  }
}

const compileFn = (compiler: Compiler, fn: Syntax.Fn): string => {
  const param = fn.params[0].value
  const body  = compileBody(compiler, fn.value)

  return `function(${param}) { ${body}; }`
}

const compileBody = (compiler: Compiler, body: Syntax.Expr): string => {

  let value = compileExpr(compiler, body, false)

  switch (body.kind) {
    case 'Def':
    case 'Set':
    case 'Cond':
    case 'Block': break
    case 'Raise':
      value = `throw ${value}`
      break
    default:
      value = `return ${value}`
  }

  return value
}

const compileName = (compiler: Compiler, name: Syntax.Name): string => {
  return name.value
}

const compileDef = (compiler: Compiler, def: Syntax.Def): string => {
  const name  = compileExpr(compiler, def.name)
  const value = compileExpr(compiler, def.value)
  return `var ${name} = ${value}`
}

const compileSet = (compiler: Compiler, set: Syntax.Set): string => {
  const target = compileExpr(compiler, set.target)
  const value  = compileExpr(compiler, set.value)
  return `${target} = ${value}`
}

const compileGet = (compiler: Compiler, get: Syntax.Get): string => {
  const main = compileExpr(compiler, get.main)
  if (get.index !== undefined) {
    return `${main}[${compileExpr(compiler, get.index)}]`
  } else {
    return `${main}.${get.member?.value}`
  }
}

const compileMut = (compiler: Compiler, mut: Syntax.Mut): string => {
  return compileExpr(compiler, mut.value)
}

const compileApply = (compiler: Compiler, app: Syntax.Apply): string => {
  const fn  = compileExpr(compiler, app.fn)
  const arg = compileExpr(compiler, app.arg)
  return `${fn}(${arg})`
}

const compileBlock = (compiler: Compiler, block: Syntax.Block, full = true): string => {
  const items = block.items.map(item => `${compileExpr(compiler, item)}`)
  const last  = items.pop()
  items.push(`return ${last}`)

  if (full) {
    return `(function() { ${items.join('; ')} })()`
  } else {
    return items.join('; ')
  }
}

const compileCond = (compiler: Compiler, cond: Syntax.Cond, full = true): string => {
  const conditions = cond
    .clauses
    .map(clause => {
      const cond = compileExpr(compiler, clause.condition)
      const value = compileExpr(compiler, clause.result, false)

      switch (clause.result.kind) {
        case 'Cond':
        case 'Block':
          return `if (${cond}) { ${value} }`
        case 'Raise':
          return `if (${cond}) {throw ${value} }`
        default:
          return `if (${cond}) { return ${value} }`
      }
    })
    .join(' else ')

  const otherwise = compileExpr(compiler, cond.otherwise, false)

  let final
  switch (cond.otherwise.kind) {
    case 'Cond':
    case 'Block':
      final = `${conditions} else { ${otherwise} }`; break
    case 'Raise':
      final = `${conditions} else { throw ${otherwise} }`; break
    default:
      final = `${conditions} else { return ${otherwise} }`; break
  }

  return full ? `(function() { ${final} })()` : final
}

const compileVariant = (compiler: Compiler, variant: Syntax.Variant): string => {
  const values = variant.values
    .map(value => {
      return compileExpr(compiler, value)
    })
    .map((value, idx) => {
      return `$${idx}: ${value}`
    })
    .join(', ')

  return `{ $$: '${variant.name.value}', ${values} }`
}

const compileList = (compiler: Compiler, list: Syntax.List): string => {
  const items = list.items
    .map(item => {
      return compileExpr(compiler, item)
    })
    .join(', ')

  return `[${items}]`
}

const compileDict = (compiler: Compiler, dict: Syntax.Dict): string => {
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
        key = `[${compileExpr(compiler, item.key)}]`
    }

    const value = compileExpr(compiler, item.value)

    return `${key} : ${value}`
  })
  .join(', ')

  return `{${items}}`
}

const compileString = (compiler: Compiler, literal: Syntax.String): string => {
  return `"${literal.value}"`
}

const compileNumber = (compiler: Compiler, literal: Syntax.Number): string => {
  return literal.value
}

const compileNative = (compiler: Compiler, expr: Syntax.Native): string => {
  switch (expr.operation) {
    case 'Equals':
      return compileNativeEquals(compiler, expr)
    case 'And':
      return compileNativeAnd(compiler, expr)
    case 'AssertList':
      return compileAssertList(compiler, expr)
    case 'EmptyList':
      return compileEmptyList(compiler, expr)
    case 'AssertDict':
      return compileAssertDict(compiler, expr)
    case 'EmptyDict':
      return compileEmptyDict(compiler, expr)
    case 'NotNull':
      return compileNotNull(compiler, expr)
  }
}

const compileNotNull = (compiler: Compiler, native: Syntax.Native): string => {
  return native.values
    .map(value => {
      return compileExpr(compiler, value)
    })
    .map(value => {
      return `${value} != undefined`
    })
    .join(' && ')
}

const compileNativeEquals = (compiler: Compiler, native: Syntax.Native): string => {
  const values = native.values.map(it => compileExpr(compiler, it))
  return values.join(' === ')
}

const compileNativeAnd = (compiler: Compiler, native: Syntax.Native): string => {
  const values = native.values.map(it => compileExpr(compiler, it))
  return values.join(' && ')
}

const compileAssertList = (compiler: Compiler, native: Syntax.Native): string => {
  return native.values
    .map(value => {
      return compileExpr(compiler, value)
    })
    .map(value => {
      return `Array.isArray(${value})`
    })
    .join(' && ')
}

const compileEmptyList = (compiler: Compiler, native: Syntax.Native): string => {
  return native.values
    .map(value => {
      return compileExpr(compiler, value)
    })
    .map(value => {
      return `${value}.length === 0`
    })
    .join(' && ')
}

const compileAssertDict = (compiler: Compiler, native: Syntax.Native): string => {
  return native.values
    .map(value => {
      return compileExpr(compiler, value)
    })
    .map(value => {
      return `${value}.constructor === Object`
    })
    .join(' && ')
}

const compileEmptyDict = (compiler: Compiler, native: Syntax.Native): string => {
  return native.values
    .map(value => {
      return compileExpr(compiler, value)
    })
    .map(value => {
      return `Object.keys(${value}).length === 0`
    })
    .join(' && ')
}

const compileRaise = (compiler: Compiler, raise: Syntax.Raise): string => {
  return `new Error(${compileExpr(compiler, raise.error)})`
}

export const compile = (module: Syntax.Module): string => {
  const cc = compiler(module)

  const nodes = module.nodes.map(node => compileExpr(cc, node))

  return nodes.join(';\n')
}