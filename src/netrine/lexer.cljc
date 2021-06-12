(ns netrine.lexer)

(def keywords
  #{"if"
    "then"
    "else"
    "and"
    "or"
    "not"
    "is"
    "do"
    "mut"})

(def symbols
  #{\% \: \= \!
    \+ \- \* \/
    \< \> \|})

(def operators
  {"="  :tk/def
   ":=" :tk/set
   ":"  :tk/colon
   "->" :tk/arrow
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
   "++" :op/concat
   ">>" :op/compose
   "|>" :op/pipe})


(defn new-lexer
  [source]
  {:source source
   :line   1
   :prev   0
   :pos    0
   :token  nil})

(defn- new-token
  ([lexer kind]
   (new-token lexer kind nil))
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
  (nth (:source lexer) (:pos lexer)))

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
  (if (or (done? lexer) (not (pred (curr lexer))))
    lexer
    (recur (bump lexer) pred)))

(defn- string
  [lexer]
  (let [lexer (bump lexer)
        lexer (bump-while lexer #(and (not= % \")
                                      (not= % \newline)))]
    (if (or (= \newline (curr lexer))
            (done? lexer))
      (new-token lexer :tk/error "Unterminated string")
      (let [lexer (bump lexer)
            value (slice lexer)]
        (new-token lexer :tk/string value)))))

(defn- number
  [lexer]
  (let [lexer (bump-while lexer digit?)
        lexer (if (= \. (curr lexer))
                (bump-while (bump lexer) digit?)
                lexer)
        value (slice lexer)]
    (new-token lexer :tk/number value)))

(defn- ident
  [lexer]
  (let [lexer (bump-while lexer alpha?)
        value (slice lexer)]
    (cond
      (contains? keywords value)
        (new-token lexer (keyword "tk" value) value)
      (lower? (first value))
        (new-token lexer :tk/lower value)
      :else
        (new-token lexer :tk/upper value))))

(defn- operator
  [lexer]
  (let [lexer (bump-while lexer symbolic?)
        value (slice lexer)
        operator (get operators value)]
    (if operator
      (new-token lexer operator value)
      (new-token lexer :tk/error "Not a valid operator"))))

(declare next)

(defn- next-token
  [lexer]
  (let [char (curr lexer)]
    (case char
      \( (new-token (bump lexer) :tk/lparen)
      \) (new-token (bump lexer) :tk/rparen)
      \{ (new-token (bump lexer) :tk/lbrace)
      \} (new-token (bump lexer) :tk/rbrace)
      \[ (new-token (bump lexer) :tk/lbracket)
      \] (new-token (bump lexer) :tk/rbracket)
      \, (new-token (bump lexer) :tk/comma)
      \; (new-token (bump lexer) :tk/semicolon)
      \& (new-token (bump lexer) :tk/amp)
      \_ (new-token (bump lexer) :tk/any)
      \space  (next (bump-while lexer #(= % \space)))
      \tab    (next (bump-while lexer #(= % \tab)))
      \return (next (bump-while lexer #(= % \return)))
      \newline (let [lexer (bump lexer)
                     lexer (update lexer :line inc)]
                 (next lexer))
      \# (next (bump-while lexer #(not= % \newline)))
      \" (string lexer)
      (cond
        (digit? char) (number lexer)
        (alpha? char) (ident lexer)
        (symbolic? char) (operator lexer)
        :else (new-token lexer :tk/error "unexpected character")))))

(defn next
  [lexer]
  (next-token (assoc lexer :prev (:pos lexer))))

(defn tokenize
  [source]
  (loop [lexer (new-lexer source)
         tokens []]
    (if (done? lexer)
      tokens
      (let [lexer (next lexer)
            tokens (conj tokens (:token lexer))]
        (recur lexer tokens)))))