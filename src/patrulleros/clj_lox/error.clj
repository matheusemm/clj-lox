(ns patrulleros.clj-lox.error
  (:require [clojure.string :as str]))

(def had-error (atom false))

(defn reset-error! []
  (reset! had-error false))

(defn error? []
  @had-error)

(defn report [line where message]
  (let [where (if (str/blank? where)
                ""
                (->> where str/trim (str " ")))]
    (format "[%d] Error%s: %s" line where (str/trim message))))

(defn report-error! [line message]
  (report line "" message)
  (reset! had-error true))
