(ns netrine.clj.core
  (:require
    [clojure.pprint :refer [pprint]]
    [netrine.lexer :refer [tokenize]]))


(defn -main []
  (let [source "hello = name ->
                print (\"Hello, \" + name)"
        tokens (tokenize source)]
    (pprint tokens)))

