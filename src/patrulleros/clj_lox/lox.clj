(ns patrulleros.clj-lox.lox
  (:require [clojure.pprint :as pprint]
            [patrulleros.clj-lox.error :as error]
            [patrulleros.clj-lox.parser :as parser]
            [patrulleros.clj-lox.scanner :as scanner])
  (:import (java.nio.file Files Paths)
           (java.nio.charset Charset)))

(defn run [source]
  (let [tokens (scanner/scan-tokens source)]
    (when-not (error/error?)
      (let [ast (parser/parse tokens)]
        (when-not (error/error?)
          (pprint/pprint ast)
          (println))))))

(defn run-file [path]
  (let [bytes (-> path (Paths/get (make-array String 0)) Files/readAllBytes)]
    (run (String. bytes (Charset/defaultCharset)))
    (when (error/error?)
      (System/exit 65))))

(defn run-prompt []
  (print "> ")
  (when-let [line (.readLine *in*)]
    (run line)
    (error/reset-error!)
    (recur)))

(defn -main [& args]
  (condp = (count args)
    0 (run-prompt)

    1 (run-file (first args))

    (do
      (println "Usage: lox [script]")
      (System/exit 64))))
