(ns patrulleros.clj-lox.util)

(defmacro with-err-str [& body]
  `(let [s# (java.io.StringWriter.)]
     (binding [*err* s#]
       ~@body
       (str s#))))
