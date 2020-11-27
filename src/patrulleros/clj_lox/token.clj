(ns patrulleros.clj-lox.token
  (:require [malli.core :as malli]
            [malli.error]))

(def token-types
  #{;; Single-character tokens.
    :LEFT-PAREN
    :RIGHT-PAREN
    :LEFT-BRACE
    :RIGHT-BRACE
    :COMMA
    :DOT
    :SEMICOLON
    :MINUS
    :PLUS
    :SLASH
    :STAR

    ;; One or two character tokens.
    :BANG
    :BANG-EQUAL
    :EQUAL
    :EQUAL-EQUAL
    :GREATER
    :GREATER-EQUAL
    :LESS
    :LESS-EQUAL

    ;; Literals.
    :IDENTIFIER
    :STRING
    :NUMBER

    ;; Keywords.
    :AND
    :CLASS
    :ELSE
    :FALSE
    :FUN
    :FOR
    :IF
    :NIL
    :OR
    :PRINT
    :RETURN
    :SUPER
    :THIS
    :TRUE
    :VAR
    :WHILE

    :EOF
    :ERROR})

(def token-schema
  [:map {:closed true}
   [:type (into [:enum] token-types)]
   [:lexeme string?]
   [:literal {:optional true} any?]
   [:line nat-int?]])

(defn create
  ([type lexeme line]
   (let [token {:type type
                :lexeme (if (sequential? lexeme)
                          (apply str lexeme)
                          (str lexeme))
                :line line}
         errors (->> token (malli/explain token-schema) malli.error/humanize)]
     (if-not (seq errors)
       token
       (throw (ex-info "Illegal argument(s)." (assoc token :errors errors))))))
  ([type lexeme literal line]
   (let [token (create type lexeme line)]
     (if literal
       (assoc token :literal literal)
       token))))

(defn lexeme [token]
  (:lexeme token))

(defn literal [token]
  (:literal token))

(defn eof
  ([]
   (eof 1))
  ([line]
   (create :EOF "" line)))
