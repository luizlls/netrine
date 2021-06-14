(ns netrine.clj.core
  (:require
    [clojure.pprint :refer [pprint]]
    [netrine.lexer :refer [tokenize]]
    [netrine.parser :refer [parse]]))


(defn -main []
  (let [source "hello = fn name -> print name"
        output (->> source
                    tokenize
                    parse)]
    (pprint output)))
