(ns geometric-algebra.mathlib
  "Missing floating-point arithmetic functions in Clojure/Java."
  (:require [clojure.math :as math]))

;; IEEE 754 is the standard for floating-point arithmetic.
;; Clojure/Java lacks quite a few of its functions, some needed in core.clj.

;; atanh adapted from glibc-2.42/sysdeps/ieee754/dbl-64/e_atanh.c
;;
;; Method:
;; - For x < 0, reduced to positive by atanh(-x) = -atanh(x)
;; - For 0 <= x < 0.5:
;;     atanh(x) = 0.5 * log(1 + 2*x + 2*x*x / (1-x))
;; - For 0.5 <= x < 1:
;;     atanh(x) = 0.5 * log(1 + 2*x / (1-x))
;;
;; Special cases:
;; - atanh(+-1) = +-Inf
;; - atanh(x) = NaN if |x| > 1 or x = NaN

(defn atanh
  "Return atanh(x), the inverse hyperbolic tangent of number x."
  [x]
  (let [ax (abs x)
        ax2 (+ ax ax)
        log (cond ; NOTE: log1p(x) = log(1+x)  with better precision
              (< ax 0.5) (math/log1p (+ ax2 (/ (* ax2 ax) (- 1.0 ax))))
              (< ax 1.0) (math/log1p (/ ax2 (- 1.0 ax)))
              (== ax 1.0) ##Inf
              :else ##NaN)]
    (math/copy-sign (* 0.5 log) x)))
