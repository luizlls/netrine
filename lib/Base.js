var mul = lhs => rhs => lhs * rhs;

var div = lhs => rhs => lhs / rhs;

var add = lhs => rhs => lhs + rhs;

var sub = lhs => rhs => lhs - rhs;

var rem = lhs => rhs => lhs % rhs;


var eq = lhs => rhs => lhs === rhs;

var ne = lhs => rhs => lhs !== rhs;

var le = lhs => rhs => lhs <= rhs;

var lt = lhs => rhs => lhs  < rhs;

var ge = lhs => rhs => lhs >= rhs;

var gt = lhs => rhs => lhs  > rhs;


var and = lhs => rhs => lhs && rhs;

var or  = lhs => rhs => lhs || rhs;

var not = rhs => !rhs;


var bitand = lhs => rhs => lhs & rhs;

var bitor  = lhs => rhs => lhs | rhs;

var bitxor = lhs => rhs => lhs ^ rhs;

var bitnot = rhs => ~rhs;


var concat = lhs => rhs => lhs.concat(rhs);
var pipe   = lhs => rhs => rhs(lhs);
