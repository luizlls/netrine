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

export interface Fun extends Node {
  kind: 'Function'
  params: Name[]
  value: Expr
}

export interface Let extends Node {
  kind: 'Let'
  pattern: Expr
  value: Expr
}

export interface Mut extends Node {
  kind: 'Mut'
  pattern: Expr
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

export const name = (value: string, span: Span): Name => ({
  kind: 'Name', value, span
})

export const fn = (params: Name[], value: Expr, span: Span): Fun => ({
  kind: 'Function', params, value, span
})

export const def = (pattern: Expr, value: Expr, span: Span): Let => ({
  kind: 'Let', pattern, value, span
})

export const mut = (pattern: Expr, value: Expr, span: Span): Mut => ({
  kind: 'Mut', pattern, value, span
})

export const apply = (fun: Expr, args: Expr[], span: Span): Apply => ({
  kind: 'Apply', fun, args, span
})

export const block = (items: Expr[], span: Span): Block => ({
  kind: 'Block', items, span
})

export const cond = (test: Expr, then: Expr, otherwise: Expr, span: Span): If => ({
  kind: 'If', test, then, otherwise, span
})

export const group = (inner: Expr, span: Span): Group => ({
  kind: 'Group', inner, span
})

export const tuple = (items: Expr[], span: Span): Tuple => ({
  kind: 'Tuple', items, span
})

export const list = (items: Expr[], span: Span): List => ({
  kind: 'List', items, span
})

export const record = (props: Property[], span: Span): Record => ({
  kind: 'Record', props, span
})

export const member = (main: Expr, property: Name, span: Span): Member => ({
  kind: 'Member', main, property, span
})

export const integer = (value: number, raw: string, span: Span): Literal<number> => ({
  kind: 'Integer', value, raw, span
})

export const float = (value: number, raw: string, span: Span): Literal<number> => ({
  kind: 'Float', value, raw, span
})

export const string = (value: string, raw: string, span: Span): Literal<string> => ({
  kind: 'String', value, raw, span
})

export const template = (elements: Expr[], span: Span): Template => ({
  kind: 'Template', elements, span
})

export const variant = (name: Name, values: Expr[], span: Span): Variant => ({
  kind: 'Variant', name, values, span
})
