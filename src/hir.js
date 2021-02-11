exports.name = (value, span) => ({
  kind: 'Name', value, span
})

exports.fn = (params, value, span) => ({
  kind: 'Function', params, value, span
})

exports.def = (name, value, span) => ({
  kind: 'Let', name, value, span
})

exports.set = (name, value, span) => ({
  kind: 'Set', name, value, span
})

exports.get = (expr, name, span) => ({
  kind: 'Get', expr, name, span
})

exports.apply = (fun, arg, span) => ({
  kind: 'Apply', fun, arg, span
})

exports.block = (items, span) => ({
  kind: 'Block', items, span
})

exports.cond = (test, then, otherwise, span) => ({
  kind: 'If', test, then, otherwise, span
})

exports.list = (items, span) => ({
  kind: 'List', items, span
})

exports.record = (props, span) => ({
  kind: 'Record', props, span
})

exports.number = (value, span) => ({
  kind: 'Number', value, span
})

exports.string = (value, span) => ({
  kind: 'String', value, span
})

exports.symbol = (name, values, span) => ({
  kind: 'Symbol', name, values, span
})
