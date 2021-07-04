export interface Module {
  name?: string
  nodes: Expr[]
}

export type Expr = any