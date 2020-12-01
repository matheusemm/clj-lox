(ns patrulleros.clj-lox.error
  (:require [clojure.string :as str]
            [patrulleros.clj-lox.token :as token]))

(def had-error (atom false))

(defn reset-error! []
  (reset! had-error false))

(defn error? []
  @had-error)

(defn report [line where message]
  (let [where (if (str/blank? where)
                ""
                (->> where str/trim (str " ")))]
    (binding [*out* *err*]
      (println (format "[%d] Error%s: %s" line where (str/trim message))))))

(defn report-error! [token-or-line message]
  (if (int? token-or-line)
    (report token-or-line "" message)
    (let [where (if (token/matches? token-or-line :EOF)
                  "at end"
                  (format "at '%s" (token/lexeme token-or-line)))]
      (report (token/line token-or-line) where message)))
  (reset! had-error true))
