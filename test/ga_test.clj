(ns ga-test
  (:require
   [clojure.test :as t :refer [deftest is testing]]
   [geometric-algebra :as ga]))

(def a (ga/multivector [[4 [2 3]]
                        [7 [3]]
                        [1 [1 4]]
                        [0 [1 2 3]]
                        [5 [1 4]]]
                       {1 1, 2 -1, 3 1, 4 1, 5 1}))

(deftest representation
  (testing "multivector representation"
    (is (= a #geometric_algebra.MultiVector{:blades [[7 [3]] [6 [1 4]] [4 [2 3]]]
                                            :signature {1 1, 2 -1, 3 1, 4 1, 5 1}}))))
(deftest to-string
  (testing "transforming to string"
    (is (= (str a) "7*e3 + 6*e14 + 4*e23"))))

(deftest addition
  (testing "basic addition"
    (is (= (str (ga/+ a a)) "14*e3 + 12*e14 + 8*e23"))))

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
