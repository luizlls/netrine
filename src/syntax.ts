export interface Span {
  lineno: number
  start:  number
  offset: number
}

export interface Module {
  name?: string
  nodes: Expr[]
}

export type Expr =
  Name
| Operator
| Fun
| Let
| Mut
| Apply
| If
| Tuple
| Sequence
| Record
| Member
| Template
| Variant
| Literal<number>
| Literal<string>

interface Node {
  span: Span
}

export interface Name extends Node {
  kind: 'Name'
  name: string
}

export interface Operator extends Node {
  kind: 'Operator'
  name: string
}

export interface Fun extends Node {
  kind: 'Function'
  params: Array<Expr>
  value: Expr
}

export interface Let extends Node {
  kind: 'Let'
  name: Name
  value: Expr
}

export interface Mut extends Node {
  kind: 'Mut'
  name: Name
  value: Expr
}

export interface Apply extends Node {
  kind: 'Apply'
  fun: Expr
  args: Array<Node>
}

export interface If extends Node {
  kind: 'If'
  test: Expr
  then: Expr
  otherwise: Expr
}

export interface Tuple extends Node {
  kind: 'Tuple'
  items: Array<Expr>
}

export interface Sequence extends Node {
  kind: 'Sequence'
  items: Array<Expr>
}

export interface Property {
  name: Name
  value?: Expr
}

export interface Record extends Node {
  kind: 'Record'
  props: Array<Property>
}

export interface Member extends Node {
  kind: 'Member'
  qualifier: Expr
  property: Name
}

export interface Literal<T> extends Node {
  kind: 'Float' | 'Integer' | 'String'
  value: T
}

export interface Template extends Node {
  kind: 'Template'
  elements: Array<Expr>
}

export interface Variant extends Node {
  kind: 'Variant'
  name: Name
  values?: Array<Expr>
}
