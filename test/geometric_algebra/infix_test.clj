(ns geometric-algebra.infix-test
  (:require [clojure.test :refer [deftest is testing]]
            [geometric-algebra.infix :as infix]))

(deftest reduce-stack-test
  (testing "Reduction of stacks of values and operations"
    (is (= (#'infix/reduce-stack '(2) 3) '(2)))
    (is (= (#'infix/reduce-stack '(2 + 1) 3) '(2 + 1)))
    (is (= (#'infix/reduce-stack '(2 + 1) 0) '((+ 1 2))))
    (is (= (#'infix/reduce-stack '(3 * 2 + 1) 2) '((* 2 3) + 1)))
    (is (= (#'infix/reduce-stack '(3 * 2 + 1) 1) '((+ 1 (* 2 3)))))
    (is (= (#'infix/reduce-stack '(4 • 3 * 2 + 1) 0) '((+ 1 (* 2 (• 3 4))))))
    (is (= (#'infix/reduce-stack '(4 • 3 * 2 + 1) 1) '((+ 1 (* 2 (• 3 4))))))
    (is (= (#'infix/reduce-stack '(4 • 3 * 2 + 1) 2) '((* 2 (• 3 4)) + 1)))
    (is (= (#'infix/reduce-stack '(4 • 3 * 2 + 1) 3) '((• 3 4) * 2 + 1)))
    (is (= (#'infix/reduce-stack '(4 • 3 * 2 + 1) 4) '(4 • 3 * 2 + 1)))))

(deftest infix->sexpr-test
  (testing "Infix to S-expression"
    (is (= (infix/infix->sexpr 1) 1))
    (is (= (infix/infix->sexpr '(1)) 1))
    (is (= (infix/infix->sexpr '(1 + 2)) '(+ 1 2)))
    (is (= (infix/infix->sexpr '((1 + 2))) '(+ 1 2)))
    (is (= (infix/infix->sexpr '(1 + 2 * 3)) '(+ 1 (* 2 3))))
    (is (= (infix/infix->sexpr '(1 * 2 + 3)) '(+ (* 1 2) 3)))
    (is (= (infix/infix->sexpr '(1 * (2 + 3))) '(* 1 (+ 2 3))))
    (is (= (infix/infix->sexpr '(1 2)) '(* 1 2)))
    (is (= (infix/infix->sexpr '(1 2 + 3)) '(+ (* 1 2) 3)))
    (is (= (infix/infix->sexpr '((1 + 2) * 3)) '(* (+ 1 2) 3)))))

(deftest infix-test
  (testing "Infix evaluation"
    (is (= (infix/infix 1) 1))
    (is (= (infix/infix 1 + 2) 3))
    (is (= (infix/infix 1 + 2 + 3) 6))
    (is (= (infix/infix 1 + 2 * 3) 7))
    (is (= (infix/infix 2 * 3 + 4) 10))
    (is (= (infix/infix 2 * (3 + 4)) 14))
    (is (= (infix/infix 2 4 + 5) 13))))
