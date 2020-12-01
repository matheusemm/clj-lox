(ns patrulleros.clj-lox.parser-test
  (:require [clojure.pprint :as pprint]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [com.gfredericks.test.chuck.clojure-test :refer [checking]]
            [patrulleros.clj-lox.parser :as sut]
            [patrulleros.clj-lox.scanner :as scanner]
            [patrulleros.clj-lox.token :as token]))

(doseq [[type lexeme] [[:BANG \!]
                       [:PLUS \+]
                       [:MINUS \-]
                       [:STAR \*]
                       [:SLASH \/]
                       [:LESS \<]
                       [:LESS-EQUAL "<="]
                       [:GREATER \>]
                       [:GREATER-EQUAL ">="]
                       [:EQUAL-EQUAL "=="]
                       [:BANG-EQUAL "!="]]]
  (intern *ns* (-> type name symbol) (token/create type lexeme 1)))

(defn parse [source]
  (-> source scanner/scan-tokens sut/parse))

(def integer-generator
  (gen/large-integer* {:min 0}))

(def double-generator
  (gen/such-that #(not(str/starts-with? (Double/toString %) "-"))
                 (gen/double* {:min 0.0 :infinite? false :NaN? false})))

(def string-generator
  (gen/fmap #(str/replace % "\"" "") gen/string))

(deftest literals-test
  (testing "booleans"
    (is (= [:LITERAL false] (parse "false")))
    (is (= [:LITERAL true] (parse "true"))))

  (testing "nil"
    (is (= [:LITERAL nil] (parse "nil"))))

  (testing "non-negative numbers"
    (checking "integers" 100
      [n integer-generator]
      (is (= [:LITERAL (double n)] (parse (str n)))))

    (checking "doubles" 100
      [n double-generator]
      (is (= [:LITERAL n] (parse (pprint/cl-format nil "~F" n))))))

  (checking "strings" 100
    [s string-generator]
    (is (= [:LITERAL s] (parse (str \" s \"))))))

(deftest unary-test
  (doseq [operator [BANG MINUS]
          [name gen lfn sfn]
          [["booleans" gen/boolean identity str]
           ["integers" integer-generator double str]
           ["doubles" double-generator identity #(pprint/cl-format nil "~F" %)]
           ["strings" string-generator identity #(str \" % \")]]]
    (testing (token/lexeme operator)
      (testing "nil"
        (is (= [:UNARY operator [:LITERAL nil]]
               (parse (str (token/lexeme operator) "nil")))))

      (checking name 10
        [value gen]
        (is (= [:UNARY operator [:LITERAL (lfn value)]]
               (parse (str (token/lexeme operator) (sfn value)))))
        (is (= [:UNARY operator
                [:UNARY operator [:LITERAL (lfn value)]]]
               (parse (str (token/lexeme operator)
                           (token/lexeme operator)
                           (sfn value)))))))))

(deftest factor-test
  (doseq [operator [STAR SLASH]]
    (let [lexeme (token/lexeme operator)]
      (testing lexeme
        (is (= [:BINARY operator [:LITERAL 2.0] [:LITERAL 3.5]]
               (parse (format "2 %s 3.5" lexeme))))
        (is (= [:BINARY operator
                [:BINARY operator [:LITERAL 2.0] [:LITERAL 3.5]]
                [:LITERAL 18.3]]
               (parse (format "2 %1$s 3.5 %1$s 18.3" lexeme))))
        (is (= [:BINARY operator
                [:BINARY operator
                 [:BINARY operator
                  [:BINARY operator
                   [:BINARY operator [:LITERAL 2.0] [:UNARY MINUS [:LITERAL 3.5]]]
                   [:UNARY BANG [:LITERAL true]]]
                  [:LITERAL false]]
                 [:LITERAL nil]]
                [:LITERAL "abc"]]
               (parse (format "2 %1$s -3.5 %1$s !true %1$s false %1$s nil %1$s \"abc\"" lexeme)))))))

  (testing "both"
    (is (= [:BINARY SLASH
            [:BINARY SLASH
             [:BINARY STAR
              [:BINARY STAR
               [:BINARY SLASH [:LITERAL 2.0] [:UNARY MINUS [:LITERAL 3.5]]]
               [:UNARY BANG [:LITERAL true]]]
              [:LITERAL false]]
             [:LITERAL nil]]
            [:LITERAL "abc"]]
           (parse "2 / -3.5 * !true * false / nil / \"abc\"")))))

(deftest term-test
  (doseq [operator [PLUS MINUS]]
    (let [lexeme (token/lexeme operator)]
      (testing lexeme
        (is (= [:BINARY operator [:LITERAL 6.0] [:LITERAL 7.0]]
               (parse (format "6.0 %s 7" lexeme))))
        (is (= [:BINARY operator
                [:BINARY operator [:LITERAL 6.0] [:LITERAL 7.0]]
                [:LITERAL 34.98]]
               (parse (format "6.0 %1$s 7 %1$s 34.98" lexeme))))
        (is (= [:BINARY operator
                [:BINARY operator
                 [:BINARY operator
                  [:BINARY operator
                   [:UNARY MINUS [:LITERAL 6.0]]
                   [:BINARY STAR [:LITERAL 7.0] [:LITERAL 34.98]]]
                  [:BINARY SLASH
                   [:UNARY MINUS [:LITERAL nil]]
                   [:UNARY BANG [:LITERAL false]]]]
                 [:LITERAL true]]
                [:LITERAL "abc"]]
               (parse (format "-6.0 %1$s 7 * 34.98 %1$s -nil / !false %1$s true %1$s \"abc\"" lexeme)))))))

  (testing "both"
    (is (= [:BINARY PLUS
            [:BINARY MINUS
             [:BINARY MINUS
              [:BINARY PLUS
               [:BINARY PLUS
                [:BINARY MINUS
                 [:UNARY MINUS [:LITERAL 6.0]]
                 [:LITERAL 7.0]]
                [:UNARY BANG [:LITERAL true]]]
               [:LITERAL false]]
              [:LITERAL nil]]
             [:LITERAL "abc"]]
            [:BINARY STAR
             [:BINARY SLASH [:LITERAL 2.0] [:LITERAL 3.0]]
             [:LITERAL 4.0]]]
           (parse "-6.0 - 7 + !true + false - nil - \"abc\" + 2 / 3 * 4")))))

(deftest comparison-test
  (doseq [operator [LESS LESS-EQUAL GREATER GREATER-EQUAL]]
    (let [lexeme (token/lexeme operator)]
      (testing lexeme
        (is (= [:BINARY operator [:LITERAL 1.0] [:LITERAL 3.0]]
               (parse (format "1 %s 3" lexeme))))
        (is (= [:BINARY operator
                [:BINARY operator [:LITERAL 1.0] [:LITERAL 3.0]]
                [:LITERAL 10.3]]
               (parse (format "1 %1$s 3 %1$s 10.3" lexeme))))
        (is (= [:BINARY operator
                [:BINARY operator
                 [:BINARY operator
                  [:BINARY operator
                   [:BINARY operator
                    [:LITERAL 1.0]
                    [:UNARY MINUS [:LITERAL 3.0]]]
                   [:LITERAL false]]
                  [:UNARY BANG [:LITERAL true]]]
                 [:LITERAL nil]]
                [:LITERAL "abc"]]
               (parse (format "1 %1$s -3 %1$s false %1$s !true %1$s nil %1$s \"abc\"" lexeme))))
        (is (= [:BINARY operator
                [:BINARY operator
                 [:BINARY operator
                  [:BINARY PLUS [:LITERAL 1.0] [:LITERAL 2.0]]
                  [:BINARY STAR [:LITERAL 3.0] [:LITERAL 4.0]]]
                 [:BINARY MINUS [:LITERAL 5.0] [:LITERAL 7.0]]]
                [:BINARY SLASH [:LITERAL 4.0] [:LITERAL 5.0]]]
               (parse (format "1 + 2 %1$s 3 * 4 %1$s 5 - 7 %1$s 4 / 5" lexeme)))))))

  (testing "all together"
    (is (= [:BINARY GREATER
            [:BINARY LESS-EQUAL
             [:BINARY GREATER-EQUAL
              [:BINARY LESS [:LITERAL 1.0] [:LITERAL "abc"]]
              [:LITERAL false]]
             [:LITERAL true]]
            [:LITERAL nil]]
           (parse "1 < \"abc\" >= false <= true > nil")))))

(deftest equality-test
  (doseq [operator [EQUAL-EQUAL BANG-EQUAL]]
    (let [lexeme (token/lexeme operator)]
      (testing lexeme
        (is (= [:BINARY operator [:LITERAL 9.0] [:LITERAL 9.2]]
               (parse (format "9 %s 9.2" lexeme))))
        (is (= [:BINARY operator
                [:BINARY operator [:LITERAL 9.0] [:LITERAL 9.2]]
                [:LITERAL 0.75]]
               (parse (format "9.0 %1$s 9.2 %1$s 0.75" lexeme))))
        (is (= [:BINARY operator
                [:BINARY operator
                 [:BINARY operator
                  [:BINARY operator
                   [:BINARY operator [:LITERAL 9.0] [:LITERAL 9.2]]
                   [:UNARY BANG [:LITERAL nil]]]
                  [:UNARY MINUS [:LITERAL false]]]
                 [:LITERAL true]]
                [:LITERAL "abc"]]
               (parse (format "9 %1$s 9.2 %1$s !nil %1$s -false %1$s true %1$s \"abc\"" lexeme))))
        (is (= [:BINARY operator
                [:BINARY PLUS [:LITERAL 1.0] [:LITERAL 2.0]]
                [:BINARY STAR [:LITERAL 3.0] [:LITERAL 4.0]]]
               (parse (format "1 + 2 %s 3 * 4" lexeme))))
        (is (= [:BINARY operator
                [:BINARY MINUS [:LITERAL 1.0] [:LITERAL 2.0]]
                [:BINARY SLASH [:LITERAL 3.0] [:LITERAL 4.0]]]
               (parse (format "1 - 2 %s 3 / 4" lexeme)))))))

  (testing "both"
    (is (= [:BINARY EQUAL-EQUAL
            [:BINARY BANG-EQUAL
             [:BINARY BANG-EQUAL
              [:BINARY EQUAL-EQUAL [:LITERAL 1.0] [:LITERAL false]]
              [:LITERAL true]]
             [:LITERAL "abc"]]
            [:LITERAL nil]]
           (parse "1 == false != true != \"abc\" == nil")))))
