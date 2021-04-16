const pretty = (node) =>
  JSON.stringify(node, null, 4)


const sexpr = (node) => {
  if (node.value) {
    switch (node.kind) {
      case 'string':
        return `"${node.value}"`
      default:
        return node.value
    }
  } else if (node.kind) {
    return `(${node.kind} ${node.args.map(arg => sexpr(arg)).join(' ')})`
  } else if (Array.isArray(node)) {
    return `(${node.map(it => sexpr(it)).join(' ')})`
  }
}


exports.sexpr = sexpr
exports.pretty = pretty