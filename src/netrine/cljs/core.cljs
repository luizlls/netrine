(ns netrine.cljs.core
  (:require
   [netrine.lexer :refer [tokenize]]))

(let [source "hello = fn name ->
                print (\"Hello, \" + name)"
      tokens (tokenize source)]
  (println tokens))

