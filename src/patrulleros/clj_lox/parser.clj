(ns patrulleros.clj-lox.parser
  (:require [patrulleros.clj-lox.error :as error]
            [patrulleros.clj-lox.token :as token]))

(declare
 parse
 parse-expression
 parse-equality
 parse-comparison
 parse-term
 parse-factor
 parse-binary
 parse-unary
 parse-primary
 parse-grouping)

(defn error! [tokens message]
  (error/report-error! (first tokens) message)
  (ex-info "Parse error." {:type :parsing
                           :tokens tokens}))

(defn consume! [tokens type message]
  (if (token/matches? (first tokens) type)
    (rest tokens)
    (throw (error! tokens message))))

(defn parse [tokens]
  (try
    (first (parse-expression tokens))
    (catch Exception _
      nil)))

(defn parse-expression [tokens]
  (parse-equality tokens))

(defn parse-equality [tokens]
  (parse-binary tokens [:EQUAL-EQUAL :BANG-EQUAL] parse-comparison))

(defn parse-comparison [tokens]
  (parse-binary tokens [:LESS :LESS-EQUAL :GREATER :GREATER-EQUAL] parse-term))

(defn parse-term [tokens]
  (parse-binary tokens [:PLUS :MINUS] parse-factor))

(defn parse-factor [tokens]
  (parse-binary tokens [:STAR :SLASH] parse-unary))

(defn parse-binary [tokens types descendant-parser]
  (let [[expr tokens] (descendant-parser tokens)]
    (loop [left expr
           [op? & lts] tokens]
      (if (apply token/matches? op? types)
        (let [[right rts] (descendant-parser lts)]
          (recur [:BINARY op? left right] rts))
        [left (conj lts op?)]))))

(defn parse-unary [[token & ts]]
  (if (token/matches? token :BANG :MINUS)
    (let [[right rts] (parse-unary ts)]
      [[:UNARY token right] rts])
    (parse-primary (conj ts token))))

(defn parse-primary [[token & ts]]
  (cond
    (token/matches? token :TRUE)
    [[:LITERAL true] ts]

    (token/matches? token :FALSE)
    [[:LITERAL false] ts]

    (token/matches? token :NIL)
    [[:LITERAL nil] ts]

    (token/matches? token :NUMBER :STRING)
    [[:LITERAL (token/literal token)] ts]

    (token/matches? token :LEFT-PAREN)
    (parse-grouping (conj ts token))

    :else
    (throw (error! (conj ts token) "Expect expression."))))

(defn parse-grouping [tokens]
  (let [[expr ts] (parse-expression (rest tokens))]
    [[:GROUPING expr] (consume! ts :RIGHT-PAREN "Expect ')' after expression.")]))
