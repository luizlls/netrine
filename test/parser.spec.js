const assert = require('assert')

const { tokenize } = require('../src/lexer')
const { parse } = require('../src/parser')
const { removeSpans } = require('./utils')

describe('Parser', () => {
  describe('parser', () => {

    it('parse basic definition', () => {
      const { nodes: [node] } = parse(tokenize('variable = 1'))
      assert.strictEqual(node.kind, 'Def')
      assert.strictEqual(node.patt.value, 'variable')
      assert.strictEqual(node.patt.kind, 'Name')
      assert.strictEqual(node.value.value, '1')
      assert.strictEqual(node.value.kind, 'Number')
    })


    it('parse basic function', () => {
      const { nodes: [node] } = parse(tokenize('hello = () => print "Hello!"'))

      const clean = removeSpans(node)

      assert.deepStrictEqual(clean, {
        kind: "Def",
        patt: {
          kind: "Name",
          value: "hello"
        },
        mutable: false,
        value: {
          kind: "Fn",
          params: [
            {
              kind: "Unit",
            }
          ],
          value: {
            kind: "Apply",
            fn: {
              kind: "Name",
              value: "print"
            },
            arg: {
              kind: "String",
              value: "Hello!",
              raw: "\"Hello!\"",
            },
          }
        }
      })
    })
  })
})
