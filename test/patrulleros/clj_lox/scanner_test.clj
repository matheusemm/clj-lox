(ns patrulleros.clj-lox.scanner-test
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [patrulleros.clj-lox.scanner :as sut]
            [patrulleros.clj-lox.token :as token]))

(defn gen-tokens
  ([choices]
   (gen-tokens choices {}))
  ([choices options]
   (let [{:keys [min-s max-s line]} (merge {:min-s 2 :max-s 10 :line 1} options)
         size (+ min-s (rand-int (- (inc max-s) min-s)))
         choices (if (coll? choices) (vec choices) [choices])]
     (conj (into []
                 (repeatedly size #(let [[lexeme type] (rand-nth choices)]
                                     (token/create type lexeme line))))
           (token/eof line)))))

(defn gen-source [tokens]
  (->> tokens (map token/lexeme) (str/join " ")))

(deftest empty-source-test
  (is (= [(token/eof)] (sut/scan-tokens ""))))

(deftest whitespaces-test
  (doseq [c sut/whitespace-chars]
    (is (= [(token/eof)] (sut/scan-tokens (str c)))))

  (dotimes [_ 10]
    (let [choices (vec sut/whitespace-chars)
          source (str/join (repeatedly 5 #(rand-nth choices)))]
      (is (= [(token/eof)] (sut/scan-tokens source))))))

(deftest simple-single-char-tokens-test
  (doseq [[c type] sut/simple-single-char-tokens]
    (is (= [(token/create type c 1) (token/eof)]
           (sut/scan-tokens (str c)))))

  (dotimes [_ 10]
    (let [tokens (gen-tokens sut/simple-single-char-tokens)
          source (gen-source tokens)]
      (is (= tokens (sut/scan-tokens source))))))

(deftest maybe-with-equal-tokens-test
  (testing "without '='"
    (doseq [[c type] sut/without-equal-tokens]
      (is (= [(token/create type c 1) (token/eof)]
             (sut/scan-tokens (str c)))))

    (dotimes [_ 10]
      (let [tokens (gen-tokens sut/without-equal-tokens)
            source (gen-source tokens)]
        (is (= tokens (sut/scan-tokens source))))))

  (testing "with '='"
    (doseq [[c type] sut/with-equal-tokens]
      (is (= [(token/create type c 1) (token/eof)]
             (sut/scan-tokens (str c)))))

    (dotimes [_ 10]
      (let [tokens (gen-tokens sut/with-equal-tokens)
            source (gen-source tokens)]
        (is (= tokens (sut/scan-tokens source))))))

  (testing "both"
    (dotimes [_ 10]
      (let [tokens (gen-tokens
                    (set/union
                     sut/with-equal-tokens
                     sut/without-equal-tokens))
            source (gen-source tokens)]
        (is (= tokens (sut/scan-tokens source))))))

  (testing "'===' should be scanned as '==' and '='"
    (is (= [(token/create :EQUAL-EQUAL "==" 1)
            (token/create :EQUAL "=" 1)
            (token/eof)]
           (sut/scan-tokens "===")))))

(deftest newline-test
  (is (= [(token/eof 2)] (sut/scan-tokens "\n")))
  (is (= [(token/eof 9)] (sut/scan-tokens (str/join (repeat 8 \newline)))))

  (is (= [(token/create :LEFT-PAREN \( 1)
          (token/create :RIGHT-PAREN \) 2)
          (token/create :GREATER-EQUAL ">=" 3)
          (token/eof 4)]
         (sut/scan-tokens "(\n)\n>=\n"))))

(deftest slash-and-comment-test
  (testing "slash"
    (is (= [(token/create :SLASH \/ 1) (token/eof)]
           (sut/scan-tokens "/"))))

  (testing "single line comment"
    (is (= [(token/eof)]
           (sut/scan-tokens "// this is a comment")))
    (is (= [(token/eof 2)]
           (sut/scan-tokens "// this is another comment\n// comment again"))))

  (testing "'///' should be scanned as a single line comment"
    (is (= [(token/eof)] (sut/scan-tokens "/// this is also a comment")))))

(deftest string-test
  (testing "single line strings"
    (doseq [[source token] [["\"\"" (token/create :STRING "\"\"" "" 1)]
                            ["\"a\"" (token/create :STRING "\"a\"" "a" 1)]
                            ["\"abc\"" (token/create :STRING "\"abc\"" "abc" 1)]
                            ]]
      (is (= [token (token/eof 1)]
             (sut/scan-tokens source)))))

  (testing "multiline strings"
    (doseq [[source token] [["\"a\nbc\"" (token/create :STRING "\"a\nbc\"" "a\nbc" 1)]
                            ["\"abc\n\"" (token/create :STRING "\"abc\n\"" "abc\n" 1)]
                            ["\"a\nb\nc\n\"" (token/create :STRING "\"a\nb\nc\n\"" "a\nb\nc\n" 1)]]]
      (let [breaks (count (filter #(= \newline %) (token/literal token)))]
        (is (= [token (token/eof (inc breaks))]
               (sut/scan-tokens source))))))

  (testing "unterminated string should result in error"
    (let [{:keys [tokens errors]} (sut/scan-tokens "\"this string doesn't terminate")]
      (is (= tokens [(token/eof)]))
      (let [message (get-in errors [0 :message])]
        (is (str/starts-with? message "Unterminated string"))))))

(deftest numbers-test
  (testing "integers"
    (dotimes [_ 100]
      (let [n (rand-int 10000)]
        (is (= [(token/create :NUMBER (str n) (double n) 1) (token/eof)]
               (sut/scan-tokens (str n)))))))

  (testing "floating points"
    (dotimes [_ 100]
      (let [n (rand 10000)]
        (is (= [(token/create :NUMBER (str n) n 1) (token/eof)]
               (sut/scan-tokens (str n))))))))

(deftest keywords-test
  (doseq [[lexeme type] sut/keyword-tokens]
    (is (= [(token/create type lexeme 1) (token/eof)]
           (sut/scan-tokens lexeme)))))

(deftest identifiers-test
  (doseq [id ["a" "a_" "a1" "a_1" "abc" "_" "__" "_a" "_a_b_" "_123"]]
    (is (= [(token/create :IDENTIFIER id 1) (token/eof)]
           (sut/scan-tokens id)))))

(deftest fib-test
  (is (= [(token/create :FUN "fun" 1)
          (token/create :IDENTIFIER "fib" 1)
          (token/create :LEFT-PAREN \( 1)
          (token/create :IDENTIFIER "n" 1)
          (token/create :RIGHT-PAREN \) 1)
          (token/create :LEFT-BRACE \{ 1)

          (token/create :IF "if" 2)
          (token/create :LEFT-PAREN \( 2)
          (token/create :IDENTIFIER "n" 2)
          (token/create :LESS-EQUAL "<=" 2)
          (token/create :NUMBER "1" 1.0 2)
          (token/create :RIGHT-PAREN \) 2)
          (token/create :LEFT-BRACE \{ 2)

          (token/create :RETURN "return" 3)
          (token/create :IDENTIFIER "n" 3)
          (token/create :SEMICOLON \; 3)

          (token/create :RIGHT-BRACE \} 4)

          (token/create :RETURN "return" 5)
          (token/create :IDENTIFIER "fib" 5)
          (token/create :LEFT-PAREN \( 5)
          (token/create :IDENTIFIER "n" 5)
          (token/create :MINUS \- 5)
          (token/create :NUMBER "2" 2.0 5)
          (token/create :RIGHT-PAREN \) 5)
          (token/create :PLUS \+ 5)
          (token/create :IDENTIFIER "fib" 5)
          (token/create :LEFT-PAREN \( 5)
          (token/create :IDENTIFIER "n" 5)
          (token/create :MINUS \- 5)
          (token/create :NUMBER "1" 1.0 5)
          (token/create :RIGHT-PAREN \) 5)
          (token/create :SEMICOLON \; 5)

          (token/create :RIGHT-BRACE \} 6)

          (token/create :FOR "for" 8)
          (token/create :LEFT-PAREN \( 8)
          (token/create :VAR "var" 8)
          (token/create :IDENTIFIER "i" 8)
          (token/create :EQUAL "=" 8)
          (token/create :NUMBER "0" 0.0 8)
          (token/create :SEMICOLON \; 8)
          (token/create :IDENTIFIER "i" 8)
          (token/create :LESS "<" 8)
          (token/create :NUMBER "20" 20.0 8)
          (token/create :SEMICOLON \; 8)
          (token/create :IDENTIFIER "i" 8)
          (token/create :EQUAL "=" 8)
          (token/create :IDENTIFIER "i" 8)
          (token/create :PLUS \+ 8)
          (token/create :NUMBER "1" 1.0 8)
          (token/create :RIGHT-PAREN \) 8)
          (token/create :LEFT-BRACE \{ 8)

          (token/create :PRINT "print" 9)
          (token/create :IDENTIFIER "fib" 9)
          (token/create :LEFT-PAREN \( 9)
          (token/create :IDENTIFIER "i" 9)
          (token/create :RIGHT-PAREN \) 9)
          (token/create :SEMICOLON \; 9)

          (token/create :RIGHT-BRACE \} 10)
          (token/eof 10)]
         (sut/scan-tokens "fun fib(n) {
  if (n <= 1) {
    return n;
  }
  return fib(n - 2) + fib (n - 1);
}

for (var i = 0; i < 20; i = i + 1) {
  print fib(i);
}"))))
