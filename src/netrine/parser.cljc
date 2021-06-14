(ns netrine.parser)

(defn new-parser
  [tokens]
  {:tokens tokens
   :token  nil
   :prev   nil
   :peek   nil
   :index  0
   :node   nil
   :errors []})

(defn- node
  [parser kind & items]
  (let [node [kind]
        node (apply conj node items)]
    (assoc parser :node node)))

(defn- bump
  [parser]
  (let [prev   (:token parser)
        token  (:peek  parser)
        peek   (nth (:tokens parser) (:index parser) {:kind :tk/eof})
        parser (update parser :index inc)]
    (assoc parser :prev  prev
                  :token token
                  :peek  peek)))

(defn- error
  [parser msg]
  (let [error  {:span (-> parser :token :span)
                :message msg}
        errors (conj (:errors parser) error)
        parser (bump parser)]
    (assoc parser :node   nil
                  :errors errors)))

(defn- done?
  [parser]
  (= :tk/eof (some-> parser :token :kind)))

(defn- matches?
  [parser kind]
  (and (not (done? parser))
       (= (-> parser :token :kind) kind)))

(defn- match-lines?
  [parser]
  (let [curr (-> parser :token :span :line)
        prev (-> parser :prev  :span :line)]
    (= curr prev)))

(defn- eat
  [parser kind]
  (if (matches? parser kind)
    (bump parser)
    (error parser (str "Expected "
                       (name kind)
                       ", but found "
                       (name (-> parser :token :kind))))))

(defn- span
  [start end]
  (let [line  (:line start)
        start (:start start)
        end   (:end end)]
    {:line line :start start :end end}))

(declare parse-expr)
(declare parse-term)

(defn- parse-block-of
  [parser consumer opening closing sep]
  (loop [parser (eat parser opening)
         items  []]
    (if (matches? parser closing)
      (let [parser (eat parser closing)]
        (assoc parser :node items))
      (let [parser (consumer parser)
            node   (:node parser)
            items  (conj items node)
            parser (if (or (nil? sep)
                           (not (matches? parser sep)))
                     parser
                     (eat parser sep))]
        (recur parser items)))))

(defn- parse-while
  [parser consumer pred]
  (loop [parser parser
         nodes  []]
    (if (not (pred parser))
      (assoc parser :node nodes)
      (let [parser (consumer parser)
            node   (:node parser)
            nodes  (conj nodes node)]
        (recur parser nodes)))))

(defn- parse-literal
  [parser kind node-kind]
  (let [parser (eat parser kind)
        value  (-> parser :prev :value)]
    (node parser node-kind value)))

(defn- parse-string
  [parser]
  (let [parser (eat parser :tk/string)
        value  (-> parser :prev :value)]
    (node parser :string value)))

(defn- parse-number
  [parser]
  (let [parser (eat parser :tk/number)
        value  (-> parser :prev :value)]
    (node parser :number value)))

(defn- parse-name
  [parser]
  (let [parser (eat parser :tk/lower)
        value  (-> parser :prev :value)]
    (node parser :name value)))

(defn- start-term?
  [parser]
  (case (-> parser :token :kind)
    (:tk/lower :tk/upper
      :tk/string :tk/number
      :tk/lparen :tk/lbrace :tk/lbracket) true
    false))

(defn- parse-arguments
  [parser]
  (parse-while parser parse-term #(and (start-term? %)
                                       (match-lines? %))))

(defn- parse-symbol
  [parser]
  (let [parser (eat parser :tk/upper)
        name   (-> parser :prev :value)
        parser (parse-arguments parser)
        values (:node parser)]
    (case name
      "True"  (node parser :true)
      "False" (node parser :false)
      (node parser :symbol name values))))

(defn- parse-def
  [parser]
  (let [parser (parse-name parser)
        name   (:node parser)
        parser (parse-expr (eat parser :tk/equals))
        value  (:node parser)]
    (node parser :def name value)))

(defn- parse-set
  [parser]
  (let [parser (parse-term parser)
        source (:node parser)
        parser (parse-expr (eat parser :tk/walrus))
        value  (:node parser)]
    (node parser :set source value)))

(defn- parse-do
  [parser]
  (let [parser (eat parser :kw/do)
        parser (parse-block-of parser parse-expr :tk/lbrace :tk/rbrace :tk/semi)
        items  (:node parser)]
    (node parser :do items)))

(defn- parse-if
  [parser]
  (let [parser (parse-expr (eat parser :kw/if))
        pred   (:node parser)
        parser (if (matches? parser :kw/do)
                 (parse-do parser)
                 (parse-expr (eat parser :kw/then)))
        then   (:node parser)
        parser (parse-expr (eat parser :kw/else))
        else   (:node parser)]
    (node parser :if pred then else)))

(defn- parse-fn
  [parser]
  (let [parser (parse-block-of parser parse-name :kw/fn :tk/arrow nil)
        params (:node parser)
        parser (parse-expr parser)
        body   (:node parser)]
    (node parser :fn params body)))

(defn- parse-apply
  [parser]
  (let [parser (parse-arguments parser)
        [callee & args] (:node parser)]
    (apply node parser :call callee args)))

(def precedence
  {:op/mul   [6, :left]
   :op/div   [6, :left]
   :op/mod   [6, :left]
   :op/add   [5, :left]
   :op/sub   [5, :left]
   :op/is    [4, :left]
   :op/eq    [4, :left]
   :op/ne    [4, :left]
   :op/lt    [4, :left]
   :op/le    [4, :left]
   :op/gt    [4, :left]
   :op/ge    [4, :left]
   :op/and   [3, :left]
   :op/or    [2, :left]
   :op/range [1, :left]
   :op/pipe  [0, :left]})

(defn- parse-unary
  [parser]
  (parse-apply parser))

(defn- parse-binary
  [parser minimum]
  (parse-apply parser))

(defn- parse-term
  [parser]
  (let [token (-> parser :token :kind)]
    (case token
      :tk/lower  (parse-name parser)
      :tk/upper  (parse-symbol parser)
      :tk/string (parse-string parser)
      :tk/number (parse-number parser)
      (error parser (str "Unexpected " (name token))))))

(defn- parse-expr
  [parser]
  (let [token (-> parser :token :kind)
        peek  (-> parser :peek  :kind)]
    (case token
      :kw/fn (parse-fn parser)
      :kw/do (parse-do parser)
      :kw/if (parse-if parser)
      (case peek
        :tk/equals (parse-def parser)
        :tk/walrus (parse-set parser)
        (parse-binary parser 0)))))

(defn parse
  [tokens]
  (loop [parser (-> tokens
                    new-parser                              ; create new parser
                    bump                                    ; initialize peek
                    bump)                                   ; initialize token
         nodes  []]
    (let [parser (parse-expr parser)
          node   (:node parser)
          nodes  (if (nil? node)
                   nodes
                   (conj nodes node))]
      (if (done? parser)
        {:nodes  nodes
         :errors (:errors parser)}
        (recur parser nodes)))))