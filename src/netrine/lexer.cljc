(ns netrine.lexer)

(def keywords
  #{"if" "then" "else"
    "and" "or" "not" "is"
    "fn" "do" "mut"})

(def symbols
  #{\% \: \= \!
    \+ \- \* \/
    \< \> \|})

(def operators
  {"="  :tk/equals
   ":=" :tk/walrus
   "->" :tk/arrow
   ":"  :op/range
   "+"  :op/add
   "-"  :op/sub
   "*"  :op/mul
   "/"  :op/div
   "%"  :op/mod
   "==" :op/eq
   "!=" :op/ne
   "<"  :op/lt
   "<=" :op/le
   ">"  :op/gt
   ">=" :op/ge
   "|>" :op/pipe
   "and" :op/and
   "or"  :op/or
   "is"  :op/is
   "not" :op/not})


(defn new-lexer
  [source]
  {:source source
   :line   1
   :prev   0
   :pos    0
   :token  nil})

(defn- token
  ([lexer kind]
   (token lexer kind nil))
  ([lexer kind value]
   (let [token {:kind  kind
                :value value
                :span  {:line  (:line lexer)
                        :start (:prev lexer)
                        :end   (:pos lexer)}}]
     (assoc lexer :token token))))

(defn- bump
  [lexer]
  (update lexer :pos inc))

(defn- slice
  [lexer]
  (subs (:source lexer) (:prev lexer) (:pos lexer)))

(defn- curr
  [lexer]
  (nth (:source lexer) (:pos lexer) nil))

(defn- done?
  [lexer]
  (>= (:pos lexer) (count (:source lexer))))

(defn- digit?
  [char]
  (and (>= (compare char \0) 0)
       (<= (compare char \9) 0)))

(defn- lower?
  [char]
  (or (and (>= (compare char \a) 0)
           (<= (compare char \z) 0))
      (= char \_)))

(defn- upper?
  [char]
  (and (>= (compare char \A) 0)
       (<= (compare char \Z) 0)))

(defn- alpha?
  [char]
  (or (lower? char) (upper? char) (digit? char)))

(defn- symbolic?
  [char]
  (contains? symbols char))

(defn- bump-while
  [lexer pred]
  (if (or (done? lexer)
          (not (pred (curr lexer))))
    lexer
    (recur (bump lexer) pred)))

(defn- string
  [lexer kind]
  (let [lexer (bump-while (bump lexer) #(and (not= % kind)
                                             (not= % \newline)))]
    (if (or (= \newline (curr lexer))
            (done? lexer))
      (token lexer :tk/error "Unterminated string")
      (let [lexer (bump lexer)
            value (slice lexer)]
        (token lexer :tk/string value)))))

(defn- number
  [lexer]
  (let [lexer (bump-while lexer digit?)
        lexer (if (= (curr lexer) \.)
                (bump-while (bump lexer) digit?)
                lexer)
        value (slice lexer)]
    (token lexer :tk/number value)))

(defn- ident
  [lexer]
  (let [lexer (bump-while lexer alpha?)
        value (slice lexer)]
    (cond
      (contains? operators value)
        (token lexer (keyword "op" value))
      (contains? keywords value)
        (token lexer (keyword "kw" value))
      (lower? (first value))
        (token lexer :tk/lower value)
      :else
        (token lexer :tk/upper value))))

(defn- operator
  [lexer]
  (let [lexer (bump-while lexer symbolic?)
        value (slice lexer)
        operator (get operators value)]
    (if operator
      (token lexer operator)
      (token lexer :tk/error "Not a valid operator"))))

(defn next-token
  [lexer]
  (let [lexer (assoc lexer :prev (:pos lexer))
        char (curr lexer)]
    (case char
      \( (token (bump lexer) :tk/lparen)
      \) (token (bump lexer) :tk/rparen)
      \{ (token (bump lexer) :tk/lbrace)
      \} (token (bump lexer) :tk/rbrace)
      \[ (token (bump lexer) :tk/lbracket)
      \] (token (bump lexer) :tk/rbracket)
      \, (token (bump lexer) :tk/comma)
      \. (token (bump lexer) :tk/dot)
      \; (token (bump lexer) :tk/semi)
      \& (token (bump lexer) :tk/amp)
      \_ (token (bump lexer) :tk/any)
      \space  (recur (bump-while lexer #(= % \space)))
      \tab    (recur (bump-while lexer #(= % \tab)))
      \return (recur (bump-while lexer #(= % \return)))
      \newline (let [lexer (bump lexer)
                     lexer (update lexer :line inc)]
                 (recur lexer))
      \# (recur (bump-while lexer #(not= % \newline)))
      \" (string lexer \")
      \' (string lexer \')
      nil (token (bump lexer) :tk/eof)
      (cond
        (digit? char) (number lexer)
        (alpha? char) (ident lexer)
        (symbolic? char) (operator lexer)
        :else (token (bump lexer)
                     :tk/error
                     (str "unexpected character `" char "`"))))))

(defn tokenize
  [source]
  (loop [lexer (new-lexer source)
         tokens []]
    (if (done? lexer)
      tokens
      (let [lexer (next-token lexer)
            tokens (conj tokens (:token lexer))]
        (recur lexer tokens)))))