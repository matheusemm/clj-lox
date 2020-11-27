(ns patrulleros.clj-lox.scanner
  (:require [patrulleros.clj-lox.token :as token]))

(def whitespace-chars
  #{\space \tab \return})

(def simple-single-char-tokens
  {\( :LEFT-PAREN
   \) :RIGHT-PAREN
   \{ :LEFT-BRACE
   \} :RIGHT-BRACE
   \. :DOT
   \, :COMMA
   \; :SEMICOLON
   \- :MINUS
   \+ :PLUS
   \* :STAR})

(def without-equal-tokens
  {\! :BANG
   \= :EQUAL
   \< :LESS
   \> :GREATER})

(def with-equal-tokens
  {"!=" :BANG-EQUAL
   "==" :EQUAL-EQUAL
   "<=" :LESS-EQUAL
   ">=" :GREATER-EQUAL})

(def keyword-tokens
  {"and" :AND
   "class" :CLASS
   "else" :ELSE
   "false" :FALSE
   "for" :FOR
   "fun" :FUN
   "if" :IF
   "nil" :NIL
   "or" :OR
   "print" :PRINT
   "return" :RETURN
   "super" :SUPER
   "this" :THIS
   "true" :TRUE
   "var" :VAR
   "while" :WHILE})

(defn create-context [source]
  {:source (seq source)
   :line 1
   :errors []})

(defn create-error [{:keys [source line]} message & args]
  {:source (apply str source)
   :line line
   :message (apply format message args)})

(defn error-line [error]
  (:line error))

(defn error-message [error]
  (:message error))

(defn digit? [c]
  (<= (int \0) (int c) (int \9)))

(defn alpha? [c]
  (or (<= (int \a) (int c) (int \z))
      (<= (int \A) (int c) (int \Z))
      (= c \_)))

(defn alphanumeric? [c]
  (or (alpha? c) (digit? c)))

(declare
 scan-tokens
 scan-token
 scan-simple
 scan-maybe-with-equal
 scan-slash
 scan-string
 scan-number
 scan-newline
 scan-identifier
 scan-whitespace

 unrecognized-char)

(defn scan-tokens
  ([source]
   (scan-tokens [] (create-context source)))
  ([tokens {:keys [source line errors] :as context}]
   (if (seq source)
     (let [[token ctx] (scan-token context)]
       (if token
         (recur (conj tokens token) ctx)
         (recur tokens ctx)))
     (let [tokens (conj tokens (token/eof line))]
       (if (seq errors)
         {:tokens tokens, :errors errors}
         tokens)))))

(defn scan-token [{:keys [source] :as context}]
  (let [c (first source)]
    (cond
      (simple-single-char-tokens c)
      (scan-simple context)

      (without-equal-tokens c)
      (scan-maybe-with-equal context)

      (= c \/)
      (scan-slash context)

      (= c \")
      (scan-string context)

      (digit? c)
      (scan-number context)

      (alpha? c)
      (scan-identifier context)

      (= c \newline)
      (scan-newline context)

      (whitespace-chars c)
      (scan-whitespace context)

      :else
      (unrecognized-char context))))

(defn scan-simple [context]
  (let [c (-> context :source first)]
    [(token/create (simple-single-char-tokens c) c (:line context))
     (update context :source rest)]))

(defn scan-maybe-with-equal [{:keys [line] :as context}]
  (let [[c eq?] (:source context)]
    (if (= eq? \=)
      (let [lexeme (str c eq?)]
        [(token/create (with-equal-tokens lexeme) lexeme line)
         (update context :source (partial drop 2))])
      [(token/create (without-equal-tokens c) c line)
       (update context :source rest)])))

(defn scan-slash [context]
  (let [slash? (-> context :source second)]
    (if (not= slash? \/)
      [(token/create :SLASH \/ (:line context))
       (update context :source rest)]

      ;; discard the text of a single line comment
      [nil
       (update context :source
               (fn [source]
                 (drop-while #(not= % \newline) source)))])))

(defn scan-string [context]
  (let [[text src] (split-with #(not= % \") (-> context :source rest))]
    (if (seq src)
      (let [text (apply str text)
            lexeme (str \" text \")
            breaks (count (filter #(= % \newline) text))]
        [(token/create :STRING lexeme text (:line context))
         (-> context
             (assoc :source (rest src))
             (update :line + breaks))])
      (let [error (create-error context "Unterminated string: %s." text)]
        [nil (-> context
                 (assoc :source (rest src))
                 (update :errors conj error))]))))

(defn scan-number [{:keys [line] :as context}]
  (let [[int-part src] (split-with digit? (:source context))]
    (if (= (first src) \.)
      (let [[frac-part src] (split-with digit? (rest src))
            lexeme (str (apply str int-part) \. (apply str frac-part))
            literal (Double/parseDouble lexeme)]
        [(token/create :NUMBER lexeme literal line)
         (assoc context :source src)])
      (let [lexeme (apply str int-part)
            literal (Double/parseDouble lexeme)]
        [(token/create :NUMBER lexeme literal line)
         (assoc context :source src)]))))

(defn scan-identifier [{:keys [line] :as context}]
  (let [[id src] (split-with alphanumeric? (:source context))
        lexeme (apply str id)
        type (keyword-tokens lexeme)]
    (if type
      [(token/create type lexeme line)
       (assoc context :source src)]
      [(token/create :IDENTIFIER lexeme line)
       (assoc context :source src)])))

(defn scan-newline [context]
  [nil (-> context
           (update :source rest)
           (update :line inc))])

(defn scan-whitespace [context]
  [nil (update context :source rest)])

(defn unrecognized-char [{:keys [source] :as context}]
  (let [error (create-error context "Unrecognized character: %c" (first source))]
    [nil (-> context
             (update :source rest)
             (update :errors conj error))]))
