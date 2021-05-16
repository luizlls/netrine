const assert = require('assert')

const { tokenize } = require('../src/lexer')
const { parse } = require('../src/parser')
const { removeSpans } = require('./utils')

describe('Parser', () => {
  describe('parser', () => {

    it('parse basic definition', () => {
      const { nodes: [node] } = parse(tokenize('var = 1'))

      const clean = removeSpans(node)

      assert.deepStrictEqual(clean, {
        kind: "Def",
        meta: {},
        name: {
          kind: "Name",
          value: "var",
          meta: {}
        },
        value: {
          kind: "Number",
          value: "1",
          meta: {}
        },
      })

    })

    it('parse basic list definition', () => {
      const { nodes: [node] } = parse(tokenize('[1, "A", 3.14, Ok, ["nested"]]'))

      const clean = removeSpans(node)

      assert.deepStrictEqual(clean, {
        kind: "List",
        meta: {},
        items: [
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
            name: {
              kind: "Name",
              value: "Ok",
              meta: {}
            },
            values: [],
            meta: {}
          },
          {
            kind: "List",
            items: [
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
      const { nodes: [node] } = parse(tokenize('["number": 1, "symbol": Some 5, "list": [1, "A", 3.14], "nested": ["inner": "value"]]'))

      const clean = removeSpans(node)

      assert.deepStrictEqual(clean, {
        kind: "Dict",
        items: [
          {
            kind: 'Property',
            meta: {},
            key: {
              kind: "String",
              value: "number",
              meta: {
                raw: "\"number\"",
              }
            },
            value: {
              kind: "Number",
              value: "1",
              meta: {}
            }
          },
          {
            kind: 'Property',
            meta: {},
            key: {
              kind: "String",
              value: "symbol",
              meta: {
                raw: "\"symbol\"",
              }
            },
            value: {
              kind: "Symbol",
              name: {
                kind: "Name",
                value: "Some",
                meta: {}
              },
              values: [
                {
                  kind: "Number",
                  value: "5",
                  meta: {}
                }
              ],
              meta: {}
            }
          },
          {
            kind: 'Property',
            meta: {},
            key: {
              kind: "String",
              value: "list",
              meta: {
                raw: "\"list\"",
              }
            },
            value: {
              kind: "List",
              items: [
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
                  kind: "Number",
                  value: "3.14",
                  meta: {}
                }
              ],
              meta: {}
            }
          },
          {
            kind: 'Property',
            meta: {},
            key: {
              kind: "String",
              value: "nested",
              meta: {
                raw: "\"nested\"",
              }
            },
            value: {
              kind: "Dict",
              items: [
                {
                  kind: 'Property',
                  meta: {},
                  key: {
                    kind: "String",
                    value: "inner",
                    meta: {
                      raw: "\"inner\"",
                    }
                  },
                  value: {
                    kind: "String",
                    value: "value",
                    meta: {
                      raw: "\"value\"",
                    }
                  }
                }
              ],
              meta: {}
            }
          }
        ],
        meta: {}
      })
    })

    it('parse basic function', () => {
      const { nodes: [node] } = parse(tokenize('hello = fn (print "Hello!")'))

      const clean = removeSpans(node)

      assert.deepStrictEqual(clean, {
        kind: "Def",
        meta: {},
        name: {
          kind: "Name",
          value: "hello",
          meta: {}
        },
        value: {
            kind: "Fn",
            params: [],
            value: {
              kind: "Group",
              inner: {
                kind: "Apply",
                fn: {
                  kind: "Name",
                  value: "print",
                  meta: {}
                },
                arg: {
                  kind: "String",
                  value: "Hello!",
                  meta: {
                    raw: "\"Hello!\""
                  }
                },
                meta: {}
              },
              meta: {},
            },
            meta: {}
          },
      })
    })
  })
})
