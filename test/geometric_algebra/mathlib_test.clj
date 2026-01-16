(ns geometric-algebra.mathlib-test
  (:require [clojure.test :refer [deftest is testing]]
            [geometric-algebra.mathlib :as ga-math]))

(deftest atanh-test
  (testing "Inverse hyperbolic tangent"
    (testing "normal cases"
      (let [approx? (fn [x y] (< (abs (- x y)) 1e-10))]
        (doseq [[x y] [[0.0001    0.000100000000333]
                       [0.004     0.004000021333538]
                       [0.1       0.100335347731075]
                       [0.2       0.202732554054082]
                       [0.3       0.309519604203111]
                       [0.4       0.423648930193601]
                       [0.5       0.549306144334054]
                       [0.7       0.867300527694053]
                       [0.8       1.098612288668109]
                       [0.9       1.472219489583220]
                       [0.999     3.800201167250199]
                       [0.9999343 5.161762981247449]]]
          (is (approx? (ga-math/atanh x) y))
          (is (approx? (ga-math/atanh (- x)) (- y))))))
    (testing "special cases"
      (is (= (ga-math/atanh 1) ##Inf))
      (is (= (ga-math/atanh -1) ##-Inf))
      (is (NaN? (ga-math/atanh 2)))
      (is (NaN? (ga-math/atanh ##NaN))))))
