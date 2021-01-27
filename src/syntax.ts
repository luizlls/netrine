import { Operators } from "./token";

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
| Block
| If
| Group
| Tuple
| List
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
  value: string
}

export interface Operator extends Node {
  kind: 'Operator'
  operator: Operators
}

export interface Fun extends Node {
  kind: 'Function'
  params: Name[]
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
  args: Expr[]
}

export interface Block extends Node {
  kind: 'Block'
  items: Expr[]
}

export interface If extends Node {
  kind: 'If'
  test: Expr
  then: Expr
  otherwise: Expr
}

export interface Group extends Node {
  kind: 'Group'
  inner: Expr
}

export interface Tuple extends Node {
  kind: 'Tuple'
  items: Expr[]
}

export interface List extends Node {
  kind: 'List'
  items: Expr[]
}

export interface Property {
  name: Name
  value?: Expr
}

export interface Record extends Node {
  kind: 'Record'
  props: Property[]
}

export interface Member extends Node {
  kind: 'Member'
  main: Expr
  property: Name
}

export interface Literal<T> extends Node {
  kind: 'Float' | 'Integer' | 'String'
  value: T
  raw: string
}

export interface Template extends Node {
  kind: 'Template'
  elements: Expr[]
}

export interface Variant extends Node {
  kind: 'Variant'
  name: Name
  values?: Expr[]
}
