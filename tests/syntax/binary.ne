// test: basic
1 + 1
--
(+ 1 1)


// test: integers and floats
3.14 ^ 2
--
(^ 3.14 2)


// test: operator precedence
1 + 2 - 3 * 4 / 5 ^ 2 ^ 2
--
(- (+ 1 2) (/ (* 3 4) (^ 5 (^ 2 2))))
