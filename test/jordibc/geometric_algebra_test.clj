(ns jordibc.geometric-algebra-test
  (:require [clojure.test :refer [deftest is testing]]
            [jordibc.geometric-algebra :as ga]))

;; Some multivectors that we will use as example.

(def a (ga/multivector [[4 [2 3]]
                        [7 [3]]
                        [1 [1 4]]
                        [0 [1 2 3]]
                        [5 [1 4]]]
                       {1 +1, 2 -1, 3 +1, 4 +1, 5 +1}))

(def a2 (ga/multivector [[11 [2]] [1 [2 3]]] {0 1, 1 1, 2 1, 3 1}))


;; The tests themselves.

(deftest representation-test
  (testing "Multivector representation"
    (testing "from basic constructor"
      (is (= a (ga/->MultiVector [[7 [3]] [6 [1 4]] [4 [2 3]]]
                                 {1 1, 2 -1, 3 1, 4 1, 5 1}))))
    (testing "from reader"
      (is (= a2
             #jordibc.geometric_algebra.MultiVector{:blades [[11 [2]] [1 [2 3]]]
                                                    :signature {0 1, 1 1, 2 1, 3 1}})))
    (testing "from map"
      (is (= a2 (ga/map->MultiVector {:blades [[11 [2]] [1 [2 3]]]
                                      :signature {0 1, 1 1, 2 1, 3 1}}))))
    (testing "from constructor"
      (is (= a (ga/multivector [[7 [3]] [6 [1 4]] [4 [2 3]]]
                               {1 1, 2 -1, 3 1, 4 1, 5 1}))))))

(deftest to-string-test
  (testing "Transforming to string"
    (is (= (str a) "7*e3 + 6*e14 + 4*e23"))
    (is (= (str a2) "11*e2 + e23"))))

(deftest addition-test
  (testing "Geometric addition"
    (let [+ ga/add]
      (is (= (str (+ a a)) "14*e3 + 12*e14 + 8*e23"))
      (is (= (str (+ a2 a2 a2)) "33*e2 + 3*e23"))
      (is (thrown? java.lang.AssertionError (+ a "")))
      (is (thrown? java.lang.AssertionError (+ a a2))))))

(deftest subtraction-test
  (testing "Geometric subtraction"
    (let [- ga/sub]
      (is (= (- 3) -3))
      (is (= (- 3 2) 1))
      (is (= (str (- a)) "-7*e3 + -6*e14 + -4*e23"))
      (is (= (- a) (- (ga/multivector 0 (:signature a)) a)))
      (is (thrown? java.lang.AssertionError (- ""))))))

(deftest product-test
  (testing "Geometric product"
    (let [* ga/prod]
      (is (= (str (* a 2)) "14*e3 + 12*e14 + 8*e23"))
      (is (= (str (* a a)) "29 + -84*e134 + 48*e1234"))
      (is (thrown? java.lang.AssertionError (* a ""))))))

(deftest reversion-test
  (testing "Multivector reversion"
    (is (= (str (ga/rev a)) "7*e3 + -6*e14 + -4*e23"))
    (is (= (str (ga/rev (ga/multivector 1 {}))) "1"))))

(deftest involution-test
  (testing "Multivector involution"
    (let [[+ - *] [ga/add ga/sub ga/prod]
          [e e1 e2 e12] (ga/basis [2 0])]
      (is (= (ga/invol (+ 1 e1)) (+ 1 (- e1)))))))

(deftest scalar-test
  (testing "Multivector as a scalar"
    (let [[+ - *] [ga/add ga/sub ga/prod]
          [e e1 e2 e12] (ga/basis [2 0])]
      (is (ga/scalar? e))
      (is (ga/scalar? (+ (* 8 e) 5)))
      (is (ga/scalar? (* e12 e12)))
      (is (false? (ga/scalar? (* e1 e12))))
      (is (= (ga/scalar (+ (* 8 e) 5)) 13))
      (is (= (ga/scalar (* (* 8 e) 5)) 40)))))

(deftest inverse-test
  (testing "Geometric inversion"
    (let [[+ - *] [ga/add ga/sub ga/prod]
          [e e1 e2 e12] (ga/basis [2 0])]
      (is (= (ga/inv 4) (/ 1 4)))
      (is (= (ga/inv e1) e1))
      (is (= (ga/inv e12) (- e12)))
      (is (= (str (ga/inv (+ e1 (* 2 e2)))) "1/5*e1 + 2/5*e2"))
      (is (thrown? java.lang.AssertionError (ga/inv (+ e1 e12)))))))

(deftest division-test
  (testing "Geometric division"
    (let [/ ga/div]
      (is (= (/ 2) 1/2))
      (is (= (/ 2 3) 2/3))
      (is (= (str (/ a (ga/multivector [[4 [3]]] (:signature a))))
             "7/4 + e2 + -3/2*e134")))))

(deftest pseudoscalar-unit-test
  (testing "Pseudoscalar unit"
    (let [[+ - *] [ga/add ga/sub ga/prod]
          [e e1 e2 e12] (ga/basis [2 0])]
      (is (= (ga/pseudoscalar-unit (:signature e1))
             (ga/pseudoscalar-unit (:signature e12))))
      (is (= (ga/pseudoscalar-unit (:signature e1)) e12)))))

(deftest dual-test
  (testing "Dual of multivector"
    (is (= (str (ga/dual a)) "7*e1245 + 6*e235 + 4*e145"))
    (is (= (ga/dual (ga/dual a)) a))))

(deftest grade-selection-test
  (testing "Grade selection"
    (is (= (str (ga/grade a 2)) "6*e14 + 4*e23"))))

(deftest power-test
  (testing "Multivector to integer power"
    (is (= (str (ga/pow a 3)) "-301*e3 + 954*e14 + -172*e23"))))

(deftest norm-test
  (testing "Multivector norm"
    (let [[+ - *] [ga/add ga/sub ga/prod]
          [e e1 e2 e12] (ga/basis [2 0])
          u (+ e1 (* 3 e2))
          w (+ (* 2 e2) e1)]
      (is (< 3 (ga/norm u) 4))
      (is (< 2 (ga/norm w) 3)))))

(deftest dot-test
  (testing "Inner product"
    (let [[+ - * ·] [ga/add ga/sub ga/prod ga/dot]
          [e e1 e2 e12] (ga/basis [2 0])
          u (+ e1 (* 3 e2))
          w (+ (* 2 e2) e1)]
      (is (= (str (· u w)) "7")))))

(deftest wedge-test
  (testing "Outer product"
    (let [[+ - * ∧] [ga/add ga/sub ga/prod ga/wedge]
          [e e1 e2 e12] (ga/basis [2 0])
          u (+ e1 (* 3 e2))
          w (+ (* 2 e2) e1)]
      (is (= (str (∧ u w)) "-1*e12")))))

(deftest lcontract-test
  (testing "Left contraction"
    (let [[+ - * ⌋] [ga/add ga/sub ga/prod ga/lcontract]
          [e e1 e2 e12] (ga/basis [2 0])]
      (is (= (str (⌋ (+ e1 (* 3 e2)) (* 2 (+ e2 e1)))) "8"))
      (is (= (str (⌋ (+ e1 e2 e12) (+ 1 e2))) "1"))
      (is (= (str (⌋ (+ e1 e2 e12) (+ 1 e12))) "-1 + -1*e1 + e2")))))

(deftest rcontract-test
  (testing "Right contraction"
    (let [[+ - * ⌊] [ga/add ga/sub ga/prod ga/rcontract]
          [e e1 e2 e12] (ga/basis [2 0])]
      (is (= (str (⌊ (+ e1 (* 3 e2)) (* 2 (+ e2 e1)))) "8"))
      (is (= (str (⌊ (+ e1 e2 e12) (+ 1 e2))) "1 + 2*e1 + e2 + e12"))
      (is (= (str (⌊ (+ e1 e2 e12) (+ 1 e12))) "-1 + e1 + e2 + e12")))))

(deftest scalar-prod-test
  (testing "Scalar product"
    (let [[+ - * ∘] [ga/add ga/sub ga/prod ga/scalar-prod]
          [e e1 e2 e12] (ga/basis [2 0])]
      (is (= (str (∘ (+ e1 (* 3 e2)) (* 2 (+ e2 e1)))) "8"))
      (is (= (str (∘ (+ e1 e2 e12) (+ 1 e2))) "1"))
      (is (= (str (∘ (+ e1 e2 e12) (+ 1 e12))) "-1")))))

(deftest fat-dot-test
  (testing "Fat-dot product"
    (let [[+ - * •] [ga/add ga/sub ga/prod ga/fat-dot]
          [e e1 e2 e12] (ga/basis [2 0])]
      (is (= (str (• (+ e1 (* 3 e2)) (* 2 (+ e2 e1)))) "8"))
      (is (= (str (• (+ e1 e2 e12) (+ 1 e2))) "1 + 2*e1 + e2 + e12"))
      (is (= (str (• (+ e1 e2 e12) (+ 1 e12))) "-1 + 2*e2 + e12")))))

(deftest commutator-test
  (testing "Commutator product"
    (let [[+ - * ×] [ga/add ga/sub ga/prod ga/commutator]
          [e e1 e2 e12] (ga/basis [2 0])
          u (+ e1 (* 3 e2))
          w (+ (* 2 e2) e1)]
      (is (= (str (× u w)) "-1*e12"))
      (is (= (str (× w u)) "e12"))
      (is (= (str (× e1 e12)) "e2"))
      (is (= (str (× e12 e12)) "0"))
      (is (= (str (× e12 e1)) "-1*e2")))))

(deftest antiwedge-test
  (testing "Regressive product"
    (let [[+ - * ∨] [ga/add ga/sub ga/prod ga/antiwedge]
          [e e1 e2 e12] (ga/basis [2 0])
          u (+ e1 (* 3 e2))
          w (+ (* 2 e2) e1)]
      (is (= (str (∨ u w)) "1")))))

(deftest projection-test
  (testing "Multivector projection"
    (let [[+ - *] [ga/add ga/sub ga/prod]
          [e e1 e2 e12] (ga/basis [2 0])
          u (+ e1 (* 3 e2))
          w (+ (* 2 e2) e1)]
      (is (= (ga/proj u e12) u))
      (is (= (ga/proj u (- e12)) u))
      (is (= (ga/proj w e12) w))
      (is (= (str (ga/proj u w)) "7/5*e1 + 14/5*e2")))))

(defn- approx?
  "Return true if multivectors a and b are approximately equal."
  [a b]
  (let [small? #(< (abs %) 1e-10)]
    (every? small? (for [[x _] (:blades (ga/sub a b))] x))))

(deftest exp-test
  (testing "Multivector exponentiation"
    (let [[+ - *] [ga/add ga/sub ga/prod]
          [e e1 e2 e3 e12 e13 e23 e123] (ga/basis [1 1 1])]
      (testing "of basis elements"
        (is (approx? (ga/exp e1) (+ (Math/cosh 1) (* e1 (Math/sinh 1)))))
        (is (approx? (ga/exp e2) (+ (Math/cos  1) (* e2 (Math/sin  1)))))
        (is (approx? (ga/exp e3) (+ 1 e3))))
      (testing "of simple multivectors"
        (is (approx? (ga/exp (* e1 (Math/log 2))) (+ 1.25 (* 0.75 e1))))
        (is (approx? (ga/exp (* e2 (/ Math/PI 2))) e2))
        (is (approx? (ga/exp (ga/pow e3 0)) (* Math/E e)))) ; e is the scalar 1
      (testing "of all-commuting blades"
        (is (approx? (ga/exp (+ 1 (* 3 e1) e123)) ; all commute
                     (+ 27.36674265819 (* 27.23140737495 e1)
                        (* 27.36674265819 e123) (* 27.23140737495 e23)))))
      (testing "of all-anticommuting blades"
        (is (approx? (ga/exp (+ (* 2 e1) e2 (* 1.2 e3))) ; all anticommute
                     (+ 2.9145774401759 (* 3.161173127133 e1)
                        (* 1.5805865635667 e2) (* 1.89670387628 e3)))))
      (testing "comparing exact values with sum of Taylor series"
        (is (approx? (ga/exp (+ 1 (* 3 e1) e123))
                     (ga/sum-exp-series (+ 1 (* 3 e1) e123) 1e-10 30)))
        (is (approx? (ga/exp (+ (* 2 e1) e2 (* 1.2 e3)))
                     (ga/sum-exp-series (+ (* 2 e1) e2 (* 1.2 e3)) 1e-10 20))))
      (testing "of multivector with no commuting symmetries"
        (is (approx? (ga/exp (+ 1 (* 2 e1) (* 3 e2) (* 0.5 e12)))
                     (+ -1.5542129560579239 (* 2.04650730667839 e1)
                        (* 3.0697609600175846 e2) (* 0.5116268266695978 e12))))
        (is (approx? (ga/exp (+ 1 e1 e2))
                     (+ 2.7182818284467594 (* 2.7182818282861687 e1)
                        (* 2.7182818282861687 e2))))))))

(deftest basis-test
  (testing "Using basis elements"
    (let [[+ - * /] [ga/add ga/sub ga/prod ga/div]]
      (let [[e e1 e2 e12] (ga/basis {1 +1, 2 +1})]
        (is (= (str (+ e1 (* 3 e2))) "e1 + 3*e2")))
      (let [[e e0 e1 e01] (ga/basis [1 1] 0)]
        (is (= (str (+ e0 (* 3 e1)))) "e0 + 3*e1")
        (is (= (str (* e1 e1)) "-1"))))))

(deftest simplify-element-test
  (testing "Simplification of basis elements"
    (let [sig {1 1, 2 1, 3 1, 4 1, 5 1}]
      (is (= (#'ga/simplify-element [3 2 3] sig)
             [[2] -1]))
      (is (= (#'ga/simplify-element [5 4 1 2 3] sig)
             [[1 2 3 4 5] -1]))
      (is (= (#'ga/simplify-element [5 4 2 2 3] {2 -1, 3 1, 4 1, 5 1})
             [[3 4 5] 1]))
      (is (= (#'ga/simplify-element [1 1 2 2 3] sig)
             [[3] 1])))))

(deftest simplify-blades-test
  (testing "Simplification of blades"
    (is (= (#'ga/simplify-blades [[4 [2 3]]
                                  [7 [3]]
                                  [1 [1 4]]
                                  [0 [1 2 3]]
                                  [5 [1 4]]])
           [[7 [3]] [6 [1 4]] [4 [2 3]]]))))

(deftest reduce-stack-test
  (testing "Reduction of stacks of values and operations"
    (let [[+ - * •] [ga/add ga/sub ga/prod ga/fat-dot]]
      (is (= (#'ga/reduce-stack '(2) 3) '(2)))
      (is (= (#'ga/reduce-stack '(2 + 1) 3) '(2 + 1)))
      (is (= (#'ga/reduce-stack '(2 + 1) 0) '((+ 1 2))))
      (is (= (#'ga/reduce-stack '(3 * 2 + 1) 2) '((* 2 3) + 1)))
      (is (= (#'ga/reduce-stack '(3 * 2 + 1) 1) '((+ 1 (* 2 3)))))
      (is (= (#'ga/reduce-stack '(4 • 3 * 2 + 1) 0) '((+ 1 (* 2 (• 3 4))))))
      (is (= (#'ga/reduce-stack '(4 • 3 * 2 + 1) 1) '((+ 1 (* 2 (• 3 4))))))
      (is (= (#'ga/reduce-stack '(4 • 3 * 2 + 1) 2) '((* 2 (• 3 4)) + 1)))
      (is (= (#'ga/reduce-stack '(4 • 3 * 2 + 1) 3) '((• 3 4) * 2 + 1)))
      (is (= (#'ga/reduce-stack '(4 • 3 * 2 + 1) 4) '(4 • 3 * 2 + 1))))))

(deftest infix->sexpr-test
  (testing "Infix to S-expression"
    (is (= (#'ga/infix->sexpr 1) 1))
    (is (= (#'ga/infix->sexpr '(1)) 1))
    (is (= (#'ga/infix->sexpr '(1 + 2)) '(+ 1 2)))
    (is (= (#'ga/infix->sexpr '((1 + 2))) '(+ 1 2)))
    (is (= (#'ga/infix->sexpr '(1 + 2 * 3)) '(+ 1 (* 2 3))))
    (is (= (#'ga/infix->sexpr '(1 * 2 + 3)) '(+ (* 1 2) 3)))
    (is (= (#'ga/infix->sexpr '(1 * (2 + 3))) '(* 1 (+ 2 3))))
    (is (= (#'ga/infix->sexpr '(1 2)) '(* 1 2)))
    (is (= (#'ga/infix->sexpr '(1 2 + 3)) '(+ (* 1 2) 3)))
    (is (= (#'ga/infix->sexpr '((1 + 2) * 3)) '(* (+ 1 2) 3)))))

(deftest infix-test
  (testing "Infix evaluation"
    (is (= (ga/infix 1) 1))
    (is (= (ga/infix 1 + 2) 3))
    (is (= (ga/infix 1 + 2 + 3) 6))
    (is (= (ga/infix 1 + 2 * 3) 7))
    (is (= (ga/infix 2 * 3 + 4) 10))
    (is (= (ga/infix 2 * (3 + 4)) 14))
    (is (= (ga/infix 2 4 + 5) 13))))
