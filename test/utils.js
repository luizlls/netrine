const removeSpans = (node) => {
  if (!node || !node.kind && !Array.isArray(node)) {
    return node
  }

  if (node.hasOwnProperty('meta')) {
    delete node.meta.span
    delete node.meta.line
  }

  for (const prop in node) {
    node[prop] = removeSpans(node[prop])
  }

  return node
}

exports.removeSpans = removeSpans