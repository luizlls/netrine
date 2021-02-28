const removeSpans = (node) => {
  if (!node.kind && !Array.isArray(node)) {
    return node
  }

  if (node.hasOwnProperty('span')) {
    delete node.span
  }

  for (const prop in node) {
    node[prop] = removeSpans(node[prop])
  }

  return node
}

exports.removeSpans = removeSpans