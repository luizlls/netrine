counter = fn {
  total = mut 0
  div [
    button "+" [ "click" => fn (total := total + 1) ],
    p "total " (strong total),
    button "-" [ "click" => fn (total := total - 1) ],
  ]
}