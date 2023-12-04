(ns ga-test
  (:require
   [clojure.test :as t :refer [deftest is testing]]
   [geometric-algebra :as ga]))


;; Some multivectors that we will use as example.

(def a (ga/multivector [[4 [2 3]]
                        [7 [3]]
                        [1 [1 4]]
                        [0 [1 2 3]]
                        [5 [1 4]]]
                       {1 +1, 2 -1, 3 +1, 4 +1, 5 +1}))

(def a2 (ga/multivector [[11 [2]] [1 [2 4]]] {0 1, 1 1, 2 1, 3 1, 4 1}))


;; The tests themselves.

(deftest representation
  (testing "multivector representation"
    (is (= a #geometric_algebra.MultiVector{:blades [[7 [3]] [6 [1 4]] [4 [2 3]]]
                                            :signature {1 1, 2 -1, 3 1, 4 1, 5 1}}))
    (is (= a2 #geometric_algebra.MultiVector{:blades [[11 [2]] [1 [2 4]]]
                                             :signature {0 1, 1 1, 2 1, 3 1, 4 1}}))))
(deftest to-string
  (testing "transforming to string"
    (is (= (str a) "7*e3 + 6*e14 + 4*e23"))
    (is (= (str a2) "11*e2 + e24"))))

(deftest addition
  (testing "geometric addition"
    (let [+ ga/add]
      (is (= (str (+ a a)) "14*e3 + 12*e14 + 8*e23"))
      (is (= (str (+ a2 a2 a2)) "33*e2 + 3*e24")))))

(deftest subtraction
  (testing "geometric subtraction"
    (let [- ga/sub]
      (is (= (- 3) -3))
      (is (= (- 3 2) 1))
      (is (= (str (- a)) "-7*e3 + -6*e14 + -4*e23"))
      (is (= (- a) (- (ga/multivector 0 (:signature a)) a))))))

(deftest product
  (testing "geometric product"
    (let [* ga/prod]
      (is (= (str (* a 2)) "14*e3 + 12*e14 + 8*e23"))
      (is (= (str (* a a)) "29 + -84*e134 + 48*e1234")))))

(deftest reversion
  (testing "multivector reversion"
    (is (= (str (ga/rev a)) "7*e3 + -6*e14 + -4*e23"))
    (is (= (str (ga/rev (ga/multivector 1 {}))) "1"))))

(deftest division
  (testing "geometric division"
    (let [/ ga/div]
      (is (= (/ 2) 1/2))
      (is (= (/ 2 3) 2/3))
      (is (= (str (/ a (ga/multivector [[4 [3]]] (:signature a))))
             "7/4 + e2 + -3/2*e134")))))

(deftest grade-selection
  (testing "grade selection"
    (is (= (str (ga/grade a 2)) "6*e14 + 4*e23"))))

(deftest power
  (testing "multivector to integer power"
    (is (= (str (ga/pow a 3)) "-301*e3 + 954*e14 + -172*e23"))))

(deftest norm
  (testing "multivector norm"
    (let [[+ - *] [ga/add ga/sub ga/prod]
          [e e1 e2 e12] (ga/basis [2 0])
          u (+ e1 (* 3 e2))
          w (+ (* 2 e2) e1)]
      (is (< 3 (ga/norm u) 4))
      (is (< 2 (ga/norm w) 3)))))

(deftest dot
  (testing "inner product"
    (let [[+ - * ·] [ga/add ga/sub ga/prod ga/dot]
          [e e1 e2 e12] (ga/basis [2 0])
          u (+ e1 (* 3 e2))
          w (+ (* 2 e2) e1)]
      (is (= (str (· u w)) "7")))))

(deftest wedge
  (testing "outer product"
    (let [[+ - * ∧] [ga/add ga/sub ga/prod ga/wedge]
          [e e1 e2 e12] (ga/basis [2 0])
          u (+ e1 (* 3 e2))
          w (+ (* 2 e2) e1)]
      (is (= (str (∧ u w)) "-1*e12")))))

(deftest lcontract
  (testing "left contraction"
    (let [[+ - * ⌋] [ga/add ga/sub ga/prod ga/lcontract]
          [e e1 e2 e12] (ga/basis [2 0])]
      (is (= (str (⌋ (+ e1 (* 3 e2)) (* 2 (+ e2 e1)))) "8"))
      (is (= (str (⌋ (+ e1 e2 e12) (+ 1 e2))) "1"))
      (is (= (str (⌋ (+ e1 e2 e12) (+ 1 e12))) "-1 + -1*e1 + e2")))))

(deftest rcontract
  (testing "right contraction"
    (let [[+ - * ⌊] [ga/add ga/sub ga/prod ga/rcontract]
          [e e1 e2 e12] (ga/basis [2 0])]
      (is (= (str (⌊ (+ e1 (* 3 e2)) (* 2 (+ e2 e1)))) "8"))
      (is (= (str (⌊ (+ e1 e2 e12) (+ 1 e2))) "1 + 2*e1 + e2 + e12"))
      (is (= (str (⌊ (+ e1 e2 e12) (+ 1 e12))) "-1 + e1 + e2 + e12")))))

(deftest scalar-prod
  (testing "scalar product"
    (let [[+ - * ∘] [ga/add ga/sub ga/prod ga/scalar-prod]
          [e e1 e2 e12] (ga/basis [2 0])]
      (is (= (str (∘ (+ e1 (* 3 e2)) (* 2 (+ e2 e1)))) "8"))
      (is (= (str (∘ (+ e1 e2 e12) (+ 1 e2))) "1"))
      (is (= (str (∘ (+ e1 e2 e12) (+ 1 e12))) "-1")))))

(deftest fat-dot
  (testing "fat-dot product"
    (let [[+ - * •] [ga/add ga/sub ga/prod ga/fat-dot]
          [e e1 e2 e12] (ga/basis [2 0])]
      (is (= (str (• (+ e1 (* 3 e2)) (* 2 (+ e2 e1)))) "8"))
      (is (= (str (• (+ e1 e2 e12) (+ 1 e2))) "1 + 2*e1 + e2 + e12"))
      (is (= (str (• (+ e1 e2 e12) (+ 1 e12))) "-1 + 2*e2 + e12")))))

(deftest commutator
  (testing "commutator product"
    (let [[+ - * ×] [ga/add ga/sub ga/prod ga/commutator]
          [e e1 e2 e12] (ga/basis [2 0])
          u (+ e1 (* 3 e2))
          w (+ (* 2 e2) e1)]
      (is (= (str (× u w)) "-1*e12")))))

(deftest antiwedge
  (testing "regressive product"
    (let [[+ - * ∨] [ga/add ga/sub ga/prod ga/antiwedge]
          [e e1 e2 e12] (ga/basis [2 0])
          u (+ e1 (* 3 e2))
          w (+ (* 2 e2) e1)]
      (is (= (str (∨ u w)) "1")))))

(deftest projection
  (testing "multivector projection"
    (let [[+ - *] [ga/add ga/sub ga/prod]
          [e e1 e2 e12] (ga/basis [2 0])
          u (+ e1 (* 3 e2))
          w (+ (* 2 e2) e1)]
      (is (= (ga/proj u e12) u))
      (is (= (ga/proj u (- e12)) u))
      (is (= (ga/proj w e12) w))
      (is (= (str (ga/proj u w)) "7/5*e1 + 14/5*e2")))))

(deftest basis
  (testing "using basis elements"
    (let [[+ - * /] [ga/add ga/sub ga/prod ga/div]]
      (let [[e e1 e2 e12] (ga/basis {1 +1, 2 +1})]
        (is (= (str (+ e1 (* 3 e2))) "e1 + 3*e2")))
      (let [[e e0 e1 e01] (ga/basis [1 1] 0)]
        (is (= (str (+ e0 (* 3 e1)))) "e0 + 3*e1")
        (is (= (str (* e1 e1)) "-1"))))))

(deftest simplify-element
  (testing "simplification of basis elements"
    (let [sig {1 1, 2 1, 3 1, 4 1, 5 1}]
      (is (= (ga/simplify-element [3 2 3] sig) [[2] -1]))
      (is (= (ga/simplify-element [5 4 1 2 3] sig) [[1 2 3 4 5] -1]))
      (is (= (ga/simplify-element [5 4 2 2 3] {2 -1, 3 1, 4 1, 5 1}) [[3 4 5] 1]))
      (is (= (ga/simplify-element [1 1 2 2 3] sig) [[3] 1])))))

(deftest simplify-blades
  (testing "simplification of blades"
    (is (= (ga/simplify-blades [[4 [2 3]]
                                [7 [3]]
                                [1 [1 4]]
                                [0 [1 2 3]]
                                [5 [1 4]]])
           [[7 [3]] [6 [1 4]] [4 [2 3]]]))))
