exports.mul = a => b => a * b;

exports.div = a => b => a / b;

exports.add = a => b => a + b;

exports.sub = a => b => a - b;

exports.rem = a => b => a % b;

exports.eq  = a => b => a === b;

exports.ne  = a => b => a !== b;

exports.le  = a => b => a <= b;

exports.lt  = a => b => a <  b;

exports.ge  = a => b => a >= b;

exports.gt  = a => b => a >  b;

exports.and = a => b => a && b;

exports.or  = a => b => a || b;

exports.not = a => !a;

exports.bitand = a => b => a & b;

exports.bitor  = a => b => a | b;

exports.bitxor = a => b => a ^ b;

exports.bitnot = a => ~a;

exports.concat = a => b => a.concat(b);

