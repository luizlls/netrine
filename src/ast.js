exports.name = (value, span) => ({
  kind: 'Name', value, span
})

exports.fn = (params, value, span) => ({
  kind: 'Function', params, value, span
})

exports.def = (pattern, value, span) => ({
  kind: 'Let', pattern, value, span
})

exports.mut = (pattern, value, span) => ({
  kind: 'Mut', pattern, value, span
})

exports.apply = (fun, args, span) => ({
  kind: 'Apply', fun, args, span
})

exports.unary = (op, rhs, span) => ({
  kind: 'Unary', op, rhs, span
})

exports.binary = (op, lhs, rhs, span) => ({
  kind: 'Binary', op, lhs, rhs, span
})

exports.block = (items, span) => ({
  kind: 'Block', items, span
})

exports.cond = (test, then, otherwise, span) => ({
  kind: 'If', test, then, otherwise, span
})

exports.group = (inner, span) => ({
  kind: 'Group', inner, span
})

exports.tuple = (items, span) => ({
  kind: 'Tuple', items, span
})

exports.list = (items, span) => ({
  kind: 'List', items, span
})

exports.record = (props, span) => ({
  kind: 'Record', props, span
})

exports.member = (main, property, span) => ({
  kind: 'Member', main, property, span
})

exports.number = (value, span) => ({
  kind: 'Number', value, span
})

exports.string = (value, raw, span) => ({
  kind: 'String', value, raw, span
})

exports.template = (elements, span) => ({
  kind: 'Template', elements, span
})

exports.symbol = (name, values, span) => ({
  kind: 'Symbol', name, values, span
})
