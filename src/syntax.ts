import { Operator } from "./token"

export interface Module {
  name?: string
  nodes: Expr[]
}

export type Expr =
    Name
  | Fn
  | Def
  | Set
  | Get
  | Mut
  | Apply
  | Unary
  | Binary
  | Partial
  | If
  | Match
  | For
  | Block
  | Group
  | Tuple
  | List
  | Dict
  | Template
  | String
  | Number
  | Variant
  | True
  | False
  | Any
  | Unit
  | Native
  | Raise

export interface Meta {
  line: number
  span: Span
}

export interface Span {
  start: number
  offset: number
}

interface Node {
  meta?: Meta
}

export interface Name extends Node {
  readonly kind: 'Name'
  value: string
}

export interface Fn extends Node {
  readonly kind: 'Fn'
  params: Name[]
  value: Expr
}

export interface Def extends Node {
  readonly kind: 'Def'
  name: Name
  value: Expr
}

export interface Set extends Node {
  readonly kind: 'Set'
  target: Name | Get
  value: Expr
}

export interface Get extends Node {
  readonly kind: 'Get'
  main: Expr
  member?: Name
  index?: Expr
}

export interface Mut extends Node {
  readonly kind: 'Mut'
  value: Expr
}

export interface Apply extends Node {
  readonly kind: 'Apply'
  fn: Expr
  arg: Expr
}

export interface Unary extends Node {
  readonly kind: 'Unary'
  operator: Operator
  rhs: Expr
}

export interface Binary extends Node {
  readonly kind: 'Binary'
  operator: Operator
  lhs: Expr
  rhs: Expr
}

export interface Partial extends Node {
  readonly kind: 'Partial'
  operator: Operator
  expr?: Expr
}

export interface Block extends Node {
  readonly kind: 'Block'
  items: Expr[]
}

export interface If extends Node {
  readonly kind: 'If'
  test: Expr
  then: Expr
  otherwise: Expr
}

export type Pattern =
    Name
  | List
  | Dict
  | Tuple
  | String
  | Number
  | Variant
  | Any
  | Unit

export interface Match extends Node {
  readonly kind: 'Match'
  value: Expr
  cases: { pattern: Pattern, result: Expr }[]
}

export interface For extends Node {
  readonly kind: 'For'
  target: Name
  source: Expr
  value: Expr
}

export interface Group extends Node {
  readonly kind: 'Group'
  inner: Expr
}

export interface Tuple extends Node {
  readonly kind: 'Tuple'
  items: Expr[]
}

export interface List extends Node {
  readonly kind: 'List'
  items: Expr[]
}

export interface Dict extends Node {
  readonly kind: 'Dict'
  items: { key: Expr, value: Expr }[]
}

export interface String extends Node {
  readonly kind: 'String'
  value: string
  raw?: string
}

export interface Number extends Node {
  readonly kind: 'Number'
  value: string
}

export interface Template extends Node {
  readonly kind: 'Template'
  elements: Expr[]
}

export interface Variant extends Node {
  readonly kind: 'Variant'
  name: Name
  values: Expr[]
}

export interface Unit extends Node {
  readonly kind: 'Unit'
}

export interface Any extends Node {
  readonly kind: 'Any'
}

export interface True extends Node {
  readonly kind: 'True'
}

export interface False extends Node {
  readonly kind: 'False'
}

export interface Native extends Node {
  readonly kind: 'Native'
  operation: NativeOperation
  values: Expr[]
}

export type NativeOperation =
    'Equals'
  | 'And'
  | 'AssertList'
  | 'EmptyList'
  | 'AssertDict'
  | 'EmptyDict'
  | 'NotNull'

export interface Raise extends Node {
  readonly kind: 'Raise'
  error: Expr
}