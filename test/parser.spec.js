const assert = require('assert')

const { tokenize } = require('../src/lexer')
const { parse } = require('../src/parser')
const { removeSpans } = require('./utils')

describe('Parser', () => {
  describe('parser', () => {

    it('parse basic definition', () => {
      const { nodes: [node] } = parse(tokenize('variable = 1'))

      const clean = removeSpans(node)

      assert.deepStrictEqual(clean, {
        kind: "Def",
        meta: {
          mutable: false,
        },
        args: [
          {
            kind: "Name",
            value: "variable",
            meta: {}
          },
          {
            kind: "Number",
            value: "1",
            meta: {}
          }
        ],
      })

    })

    it('parse basic list definition', () => {
      const { nodes: [node] } = parse(tokenize('[1, "A", 3.14, Ok, ["nested"]]'))

      const clean = removeSpans(node)

      assert.deepStrictEqual(clean, {
        kind: "List",
        meta: {},
        args: [
          {
            kind: "Number",
            value: "1",
            meta: {}
          },
          {
            kind: "String",
            value: "A",
            meta: {
              raw: "\"A\""
            }
          },
          {
            kind: "Number",
            value: "3.14",
            meta: {}
          },
          {
            kind: "Symbol",
            args: [
              {
                kind: "Name",
                value: "Ok",
                meta: {}
              },
              []
            ],
            meta: {}
          },
          {
            kind: "List",
            args: [
              {
                kind: "String",
                value: "nested",
                meta: {
                  raw: "\"nested\""
                }
              }
            ],
            meta: {}
          }
        ],
      })
    })

    it('parse basic dict definition', () => {
      const { nodes: [node] } = parse(tokenize('["number": 1, "symbol": Some 5, "list": [1, "A", "3.14"], "nested": ["inner": "value"]]'))

      const clean = removeSpans(node)

      assert.deepStrictEqual(clean, {
        kind: "Dict",
        args: [
          [
            {
              kind: "String",
              value: "number",
              meta: {
                raw: "\"number\"",
              }
            },
            {
              kind: "Number",
              value: "1",
              meta: {}
            }
          ],
          [
            {
              kind: "String",
              value: "symbol",
              meta: {
                raw: "\"symbol\"",
              }
            },
            {
              kind: "Symbol",
              args: [
                {
                  kind: "Name",
                  value: "Some",
                  meta: {}
                },
                [
                  {
                    kind: "Number",
                    value: "5",
                    meta: {}
                  }
                ]
              ],
              meta: {}
            }
          ],
          [
            {
              kind: "String",
              value: "list",
              meta: {
                raw: "\"list\"",
              }
            },
            {
              kind: "List",
              args: [
                {
                  kind: "Number",
                  value: "1",
                  meta: {}
                },
                {
                  kind: "String",
                  value: "A",
                  meta: {
                    raw: "\"A\"",
                  }
                },
                {
                  kind: "String",
                  value: "3.14",
                  meta: {
                    raw: "\"3.14\"",
                  }
                }
              ],
              meta: {
              }
            }
          ],
          [
            {
              kind: "String",
              value: "nested",
              meta: {
                raw: "\"nested\"",
              }
            },
            {
              kind: "Dict",
              args: [
                [
                  {
                    kind: "String",
                    value: "inner",
                    meta: {
                      raw: "\"inner\"",
                    }
                  },
                  {
                    kind: "String",
                    value: "value",
                    meta: {
                      raw: "\"value\"",
                    }
                  }
                ]
              ],
              meta: {}
            }
          ]
        ],
        meta: {}
      })
    })

    it('parse basic function', () => {
      const { nodes: [node] } = parse(tokenize('hello = fn (print "Hello!")'))

      const clean = removeSpans(node)

      assert.deepStrictEqual(clean, {
        kind: "Def",
        meta: {
          mutable: false,
        },
        args: [
          {
            kind: "Name",
            value: "hello",
            meta: {}
          },
          {
            kind: "Apply",
            args: [
              {
                kind: "Name",
                value: "fn",
                meta: {}
              },
              {
                kind: "Apply",
                args: [
                  {
                    kind: "Name",
                    value: "print",
                    meta: {}
                  },
                  {
                    kind: "String",
                    value: "Hello!",
                    meta: {
                      raw: "\"Hello!\""
                    }
                  }
                ],
                meta: {}
              }
            ],
            meta: {}
          }
        ],
      })
    })
  })
})
