(ns lexer-test
  (:require
    [clojure.test :refer :all]
    [netrine.lexer :as lexer]))

(deftest lexer
  (testing "tokenize basic string"
    (is (= (lexer/tokenize "'Hello'")
           [{:kind  :tk/string
            :value "'Hello'"
            :span {:line 1 :start 0 :end 7}}])))

  (testing "tokenize basic integer number"
    (is (= (lexer/tokenize "42")
           [{:kind  :tk/number
             :value "42"
             :span {:line 1 :start 0 :end 2}}])))

  (testing "tokenize basic float number"
    (is (= (lexer/tokenize "3.14")
           [{:kind  :tk/number
             :value "3.14"
             :span {:line 1 :start 0 :end 4}}])))

  (testing "tokenize basic lowercase identifier"
    (is (= (lexer/tokenize "name")
           [{:kind  :tk/lower
             :value "name"
             :span {:line 1 :start 0 :end 4}}])))

  (testing "tokenize basic uppercase identifier"
    (is (= (lexer/tokenize "True")
           [{:kind  :tk/upper
             :value "True"
             :span {:line 1 :start 0 :end 4}}])))

  (testing "tokenize keywords"
    (let [tokens (lexer/tokenize "if True and not False then 1 else 0")
          [if _ and not _ then _ else _] tokens]
      (do (is (= (:kind if) :kw/if))
          (is (= (:kind and) :op/and))
          (is (= (:kind not) :op/not))
          (is (= (:kind then) :kw/then))
          (is (= (:kind else) :kw/else)))))

  (testing "tokenize operators"
    (let [tokens (lexer/tokenize "value = a + b - c * d / e == f != g > h <= i >= j and k is not l |> m")
          [_ equals _ add _ sub _ mul _ div _ eq _ ne _ gt _ le _ ge _ and _ is' not _ pipe] tokens]
      (do (is (= (:kind equals) :tk/def))
          (is (= (:kind add) :op/add))
          (is (= (:kind sub) :op/sub))
          (is (= (:kind mul) :op/mul))
          (is (= (:kind div) :op/div))
          (is (= (:kind eq) :op/eq))
          (is (= (:kind ne) :op/ne))
          (is (= (:kind gt) :op/gt))
          (is (= (:kind le) :op/le))
          (is (= (:kind ge) :op/ge))
          (is (= (:kind and) :op/and))
          (is (= (:kind is') :op/is))
          (is (= (:kind not) :op/not))
          (is (= (:kind pipe) :op/pipe)))))

  (testing "ignore comments"
    (is (= (lexer/tokenize "a + b # here are the comments ...")
           [{:kind :tk/lower
             :value "a"
             :span {:line 1 :start 0 :end 1}}
            {:kind :op/add
             :value nil
             :span {:line 1 :start 2 :end 3}}
            {:kind :tk/lower
             :value "b"
             :span {:line 1 :start 4 :end 5}}
            {:kind :tk/eof
             :value nil
             :span {:line 1 :start 33 :end 34}}]))))