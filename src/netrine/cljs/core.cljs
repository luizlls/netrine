(ns netrine.cljs.core
  (:require
   [netrine.lexer :refer [tokenize]]))

(let [source "hello = name ->
                print (\"Hello, \" + name)"
      tokens (tokenize source)]
  (println tokens))

