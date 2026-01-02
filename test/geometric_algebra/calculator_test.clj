(ns geometric-algebra.calculator-test
  (:require [clojure.test :refer [deftest is testing]]
            [geometric-algebra.calculator :as calc]))

(defn output [x]
  (str "Basis multivectors: e0 e1 e2 e3 e01 e02 e03 e12 e13 e23 "
       "e012 e013 e023 e123 e0123\n"
       "Signature: e0e0=1 e1e1=-1 e2e2=-1 e3e3=-1\n"
       "Functions: proj inv invol dual rej norm pow exp rev grade\n"
       "Operators: + - * / · ∧ ∨ × ⌋ ⌊ ∘ •\n"
       "Type :help for help, :exit to exit.\n"
       "> " x "\n"
       "> "))

(deftest calc-test
  (let [sig (calc/args->signature ["sta"])
        calc #(with-out-str (with-in-str % (calc/calc sig)))
        result? (fn [x y] (is (= (calc x) (output y))))]
    (testing "Calculator usage (infix)"
      (testing "with simple value"
        (result? "3" "3"))
      (testing "with standard operations"
        (result? "1 + 2" "3")
        (result? "4 * 5" "20")
        (result? "1 / 5" "1/5"))
      (testing "with multivectors"
        (result? "2 * (7 e3 + 6 e12 + 4 e23)"
                 "14 e3 + 12 e12 + 8 e23")
        (result? "pow(7 e3 + 6 e12 + 4 e23, 3)"
                 "-336 e1 + -1211 e3 + -1194 e12 + -404 e23"))
      (testing "requesting help"
        (result? ":help" (str "Type any expression to get its value. "
                              "Use assignments like 'a = 2' to create new variables.\n"
                              "Special commands:\n"
                              "  :help <symbol>  - provides help for functions and operators\n"
                              "  :env            - shows the variables in the environment\n"
                              "  :exit, :quit    - exits the calculator"))
        (result? ":help proj"
                 (str "  proj ([a b])\n"
                      "  Return P_b(a), the projection of multivector `a` on `b`."))
        (result? ":help *"
                 (str "  * ([] [a] [a b] [a b & more])\n"
                      "  Return a * b, the geometric product of multivectors `a` and `b`.")))
      (testing "assigning variables"
        (result? "a = e2 + e1\na" "> e1 + e2")
        (result? "1 = 1" "Invalid name: 1")
        (result? "1 != 1" "Unable to resolve symbol: !")))))
