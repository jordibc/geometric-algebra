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
                       {1 1, 2 -1, 3 1, 4 1, 5 1}))

(def a2 (ga/multivector [[11 [2]] [1 [2 4]]]))


;; The tests themselves.

(deftest representation
  (testing "multivector representation"
    (is (= a #geometric_algebra.MultiVector{:blades [[7 [3]] [6 [1 4]] [4 [2 3]]]
                                            :signature {1 1, 2 -1, 3 1, 4 1, 5 1}}))
    (is (= a2 #geometric_algebra.MultiVector{:blades [[11 [2]] [1 [2 4]]]
                                             :signature nil}))))
(deftest to-string
  (testing "transforming to string"
    (is (= (str a) "7*e3 + 6*e14 + 4*e23"))
    (is (= (str a2) "11*e2 + e24"))))

(deftest addition
  (testing "basic addition"
    (is (= (str (ga/+ a a)) "14*e3 + 12*e14 + 8*e23"))
    (is (= (str (ga/+ a2 a2 a2)) "33*e2 + 3*e24"))))

(deftest subtraction
  (testing "basic subtraction"
    (is (= (str (ga/- a)) "-7*e3 + -6*e14 + -4*e23"))
    (is (= (ga/- a) (ga/- (ga/multivector 0 (:signature a)) a)))))

(deftest simplify-element
  (testing "simplification of basis elements"
    (is (= (ga/simplify-element [3 2 3] nil) [[2] -1]))
    (is (= (ga/simplify-element [5 4 1 2 3] nil) [[1 2 3 4 5] -1]))
    (is (= (ga/simplify-element [5 4 2 2 3] {2 -1, 3 1, 4 1, 5 1}) [[3 4 5] 1]))
    (is (= (ga/simplify-element [1 1 2 2 3] nil) [[3] 1]))))

(deftest simplify-blades
  (testing "simplification of blades"
    (is (= (ga/simplify-blades [[4 [2 3]]
                                [7 [3]]
                                [1 [1 4]]
                                [0 [1 2 3]]
                                [5 [1 4]]])
           [[7 [3]] [6 [1 4]] [4 [2 3]]]))))
