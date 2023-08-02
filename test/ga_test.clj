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
