(ns patrulleros.clj-lox.lox
  (:require [patrulleros.clj-lox.error :as error]
            [patrulleros.clj-lox.scanner :as scanner])
  (:import (java.nio.file Files Paths)
           (java.nio.charset Charset)))

(defn run [source]
  (let [res (scanner/scan-tokens source)]
    (if (and (map? res) (seq (:errors res)))
      (let [{:keys [tokens errors]} res]
        (println tokens)
        (doseq [e errors]
          (error/report-error! (scanner/error-line e) (scanner/error-message e))))
      (println res))))

(defn run-file [path]
  (let [bytes (-> path (Paths/get (make-array String 0)) Files/readAllBytes)]
    (run (String. bytes (Charset/defaultCharset)))
    (when (error/error?)
      (System/exit 65))))

(defn run-prompt []
  (print "> ")
  (let [line (.readLine *in*)]
    (when line
      (run line)
      (error/reset-error!)
      (recur))))

(defn -main [& args]
  (condp = (count args)
    0 (run-prompt)

    1 (run-file (first args))

    (do
      (println "Usage: lox [script]")
      (System/exit 64))))
