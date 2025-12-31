(ns geometric-algebra.calculator-test
  (:require [clojure.test :refer [deftest is testing]]
            [geometric-algebra.calculator :as calc]))

(defn output [x]
  (str "Basis multivectors: e0 e1 e2 e3 e01 e02 e03 e12 e13 e23 "
       "e012 e013 e023 e123 e0123\n"
       "Signature: e0e0=1 e1e1=-1 e2e2=-1 e3e3=-1\n"
       "Functions: proj inv invol dual rej norm pow exp rev grade\n"
       "Operators: + - * / · ∧ ∨ × ⌋ ⌊ ∘ •\n"
       "> " x "\n"
       "> "))

(deftest calc-test
  (let [sig (calc/args->signature ["sta"])
        calc #(with-out-str (with-in-str % (calc/calc sig)))]
    (testing "Calculator usage (infix)"
      (testing "with simple value"
        (is (= (calc "3") (output "3"))))
      (testing "with standard operations"
        (is (= (calc "1 + 2") (output "3")))
        (is (= (calc "4 * 5") (output "20")))
        (is (= (calc "1 / 5") (output "1/5"))))
      (testing "with multivectors"
        (is (= (calc "2 * (7 e3 + 6 e12 + 4 e23)")
               (output "14 e3 + 12 e12 + 8 e23")))
        (is (= (calc "pow(7 e3 + 6 e12 + 4 e23, 3)")
               (output "-336 e1 + -1211 e3 + -1194 e12 + -404 e23"))))
      (testing "requesting help"
        (is (= (calc "help") (output "Usage: help <function>")))
        (is (= (calc "help proj")
               (output (str "  proj ([a b])\n"
                            "  Return P_b(a), the projection of multivector `a` on `b`."))))
        (is (= (calc "help *")
               (output (str "  * ([] [a] [a b] [a b & more])\n"
                            "  Return a * b, the geometric product of multivectors `a` and `b`.")))))
      (testing "assigning variables"
        (is (= (calc "a = e2 + e1\na") (output "> e1 + e2")))))))
