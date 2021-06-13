(ns netrine.clj.core
  (:require
    [clojure.pprint :refer [pprint]]
    [netrine.lexer :refer [tokenize]]))


(defn -main []
  (let [source "hello = fn name ->
                print ('Hello, ' + name)"
        tokens (tokenize source)]
    (pprint tokens)))

