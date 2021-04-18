if = macro test then: then else: else {
  case unquote test of
    True  => unquote then
  | False => unquote else
}