const assert = require('assert')

const { tokenize } = require('../src/lexer')

describe('Lexer', () => {
  describe('tokenizer', () => {
    it('tokenize basic definition', () => {
      const [variable, eq, number] = tokenize('variable = 1')
      assert.strictEqual(variable.kind, 'lower')
      assert.strictEqual(eq.kind, 'equals')
      assert.strictEqual(number.kind, 'number')
    })

    it('tokenize symbol', () => {
      const [symbol] = tokenize('Symbol')
      assert.strictEqual(symbol.kind, 'upper')
    })

    it('tokenize keywords', () => {
      const tokens = tokenize('if x > y then X else Y')
      assert.strictEqual(tokens[0].kind, 'if')
      assert.strictEqual(tokens[1].kind, 'lower')
      assert.strictEqual(tokens[2].kind, 'gt')
      assert.strictEqual(tokens[3].kind, 'lower')
      assert.strictEqual(tokens[4].kind, 'then')
      assert.strictEqual(tokens[5].kind, 'upper')
      assert.strictEqual(tokens[6].kind, 'else')
      assert.strictEqual(tokens[7].kind, 'upper')
    })

    it('tokenize operators', () => {
      const tokens = tokenize('1 + 2 * 3 / 4 - 5 |> 6 % 7 ++ 8')
      assert.strictEqual(tokens[1].kind, 'add')
      assert.strictEqual(tokens[3].kind, 'mul')
      assert.strictEqual(tokens[5].kind, 'div')
      assert.strictEqual(tokens[7].kind, 'sub')
      assert.strictEqual(tokens[9].kind, 'pipe')
      assert.strictEqual(tokens[11].kind, 'rem')
      assert.strictEqual(tokens[13].kind, 'concat')
    })

    it('tokenize numbers', () => {
      const [integer, _, decimal] = tokenize('42 + 3.1459')
      assert.strictEqual(integer.kind, 'number')
      assert.strictEqual(decimal.kind, 'number')
    })

    it('tokenize basic string', () => {
      const [str] = tokenize('"Hello, World"')
      assert.strictEqual(str.kind, 'string')
    })

    it('tokenize string template', () => {
      const [start, _1, name, _2, finish] = tokenize('"Hello, {name}!"')
      assert.strictEqual(start.kind, 'string start')
      assert.strictEqual(name.kind, 'lower')
      assert.strictEqual(finish.kind, 'string finish')
    })
  })
})
