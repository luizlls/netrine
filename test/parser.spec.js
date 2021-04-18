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
        kind: "def",
        meta: {
          mutable: false,
        },
        args: [
          {
            kind: "name",
            value: "var",
            meta: {}
          },
          {
            kind: "number",
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
        kind: "list",
        meta: {},
        args: [
          {
            kind: "number",
            value: "1",
            meta: {}
          },
          {
            kind: "string",
            value: "A",
            meta: {
              raw: "\"A\""
            }
          },
          {
            kind: "number",
            value: "3.14",
            meta: {}
          },
          {
            kind: "symbol",
            args: [
              {
                kind: "name",
                value: "Ok",
                meta: {}
              },
              []
            ],
            meta: {}
          },
          {
            kind: "list",
            args: [
              {
                kind: "string",
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
      const { nodes: [node] } = parse(tokenize(`
        [
          "number" => 1,
          "string" => "Hello",
          "list" => [1, "Hi"],
          "symbol" => Some 42,
          "nested" => [
            "inner" => "value"
          ]
        ]`))

      const clean = removeSpans(node)

      assert.deepStrictEqual(clean, {
        kind: "dict",
        args: [
          [
            {
              kind: "string",
              value: "number",
              meta: {
                raw: "\"number\"",
              }
            },
            {
              kind: "number",
              value: "1",
              meta: {}
            }
          ],
          [
            {
              kind: "string",
              value: "string",
              meta: {
                raw: "\"string\"",
              }
            },
            {
              kind: "string",
              value: "Hello",
              meta: {
                raw: "\"Hello\"",
              }
            }
          ],
          [
            {
              kind: "string",
              value: "list",
              meta: {
                raw: "\"list\"",
              }
            },
            {
              kind: "list",
              args: [
                {
                  kind: "number",
                  value: "1",
                  meta: {}
                },
                {
                  kind: "string",
                  value: "Hi",
                  meta: {
                    raw: "\"Hi\"",
                  }
                }
              ],
              meta: {
              }
            }
          ],
          [
            {
              kind: "string",
              value: "symbol",
              meta: {
                raw: "\"symbol\"",
              }
            },
            {
              kind: "symbol",
              args: [
                {
                  kind: "name",
                  value: "Some",
                  meta: {}
                },
                [
                  {
                    kind: "number",
                    value: "42",
                    meta: {}
                  }
                ]
              ],
              meta: {}
            }
          ],
          [
            {
              kind: "string",
              value: "nested",
              meta: {
                raw: "\"nested\"",
              }
            },
            {
              kind: "dict",
              args: [
                [
                  {
                    kind: "string",
                    value: "inner",
                    meta: {
                      raw: "\"inner\"",
                    }
                  },
                  {
                    kind: "string",
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
        kind: "def",
        meta: {
          mutable: false,
        },
        args: [
          {
            kind: "name",
            value: "hello",
            meta: {}
          },
          {
            kind: "apply",
            args: [
              {
                kind: "name",
                value: "fn",
                meta: {}
              },
              {
                kind: "group",
                args: [
                  {
                    kind: "apply",
                    args: [
                      {
                        kind: "name",
                        value: "print",
                        meta: {}
                      },
                      {
                        kind: "string",
                        value: "Hello!",
                        meta: {
                          raw: "\"Hello!\""
                        }
                      }
                    ],
                    meta: {}
                  }
                ],
                meta: {},
              }
            ],
            meta: {}
          }
        ],
      })
    })
  })
})
