;; References:
;;   [Dorst 2002] - Dorst et al. (2002). "Applications of Geometric
;;     Algebra in Computer Science and Engineering".
;;   [Hestenes 1984] - Hestenes and Sobczyk (1984). "Clifford Algebra
;;     to Geometric Calculus".

(ns geometric-algebra.core
  "The multivector type and basic operations defined on multivectors."
  (:require [geometric-algebra.mathlib :as ga-math]
            [clojure.string :as str]
            [clojure.math :as math]))

;; The MultiVector record, and how to convert it to string.

(defn- blade->str
  "Return a string that represents the blade. Examples: 3 e1, e23, 5."
  [[x e] e-separator]
  (let [show-e (seq e) ; show the basis element for scalars (4, not 4 e)
        show-x (or (not show-e) (not (== x 1)))] ; to write e1 instead of 1 e1
    (str (when show-x x)                          ; "7"
         (when (and show-x show-e) " ")           ; " "
         (when show-e
           (str "e" (str/join e-separator e)))))) ; "e134"

(defrecord MultiVector [blades signature]
  Object
  (toString [_] ; "7 e3 - 6 e14 + 4 e23"
    (if (empty? blades)
      "0"
      (let [max-e (apply max (or (keys signature) '(0))) ; highest basis element
            e-separator (if (< max-e 10) "" "_") ; differentiate e12 and e1_2
            abs->str (fn [[x e]] (blade->str [(abs x) e] e-separator))
            [b0 & b-rest] blades]
        (str (when (< (first b0) 0) "-") (abs->str b0) ; add "-" to first blade
             (apply str (for [[x e] b-rest] ; properly +/- the rest
                          (str (if (> x 0) " + " " - ") (abs->str [x e])))))))))

(defmethod print-method MultiVector [a w]
  (.write w (str a))) ; so on the console the multivectors will look nice too


;; Simplifying a collection of blades to construct a "normalized" multivector.

(defn- add-values
  "Return blade with the sum of the values of the given blades.
  It assumes (and doesn't check) that all blades have the same basis element."
  [blades]
  (let [[_ element] (first blades)] ; common element, like e13
    [(reduce + (map first blades)) element]))

(def ^:private merge-and-clean ; transducer to merge and clean blades
  (comp
   (partition-by second) ; group blades with same basis element
   (map add-values) ; merge blades with same basis element
   (remove #(zero? (first %))))) ; remove terms that are 0

(defn- simplify-blades
  "Return the blades of a multivector simplified.
  Example: 3 e24 + 5 e7 + 0 e4 + e24  ->  5 e7 + 4 e24"
  [blades]
  (let [sorted-blades (sort-by second blades)] ; sorted by basis element
    (into [] merge-and-clean sorted-blades)))

(defn- simplify-element
  "Return the simplification of a basis element, and the factor it carries.
   Example: e13512  ->  e235, +1  (if  e1 e1 = +1)"
  [element signature]
  (loop [[e0 & e-rest] element
         result []
         factor +1]
    (let [e1 (first e-rest)]
      (cond
        (nil? e-rest) [(if e0 (conj result e0) result) factor] ; we are done!
        (= e0 e1) (recur (rest e-rest) ; repeated element -> contract
                         result
                         (* factor (signature e0)))
        (> e0 e1) (if (empty? result) ; unsorted order -> swap
                    (recur (concat [e0] (rest e-rest))
                           [e1]
                           (* factor -1)) ; these vectors anticommute
                    (recur (concat [(last result) e1 e0] (rest e-rest))
                           (vec (butlast result)) ; so we keep comparing this
                           (* factor -1)))
        :else (recur e-rest ; nothing to do at this position -> advance
                     (conj result e0)
                     factor)))))

(defn multivector ; constructor
  "Create a new multivector."
  [blades-or-num signature]
  (let [blades (if (number? blades-or-num)
                 (if (zero? blades-or-num)
                   [] ; 0 has no blades
                   [[blades-or-num []]]) ; "upgrade" number to single blade
                 (simplify-blades blades-or-num))] ; use given blades
    (->MultiVector blades signature)))

(defn multivector?
  "Return true if `a` is a multivector (numbers are multivectors too)."
  [a]
  (or (number? a) (instance? MultiVector a)))

(defn same-algebra?
  "Return true if the arguments belong to the same geometric algebra."
  [a b]
  {:pre [(multivector? a) (multivector? b)]}
  (or (number? a) (number? b) ; numbers operate as themselves in any algebra
      (= (:signature a) (:signature b)))) ; multivectors must share signature


;; Operations.

(defn add
  "Return a + b."
  ([] 0)
  ([a] a)
  ([a b]
   {:pre [(same-algebra? a b)]}
   (cond
     (and (number? a) (number? b)) (+ a b)
     (number? a) (if (NaN? a) a (add (multivector a (:signature b)) b))
     (number? b) (if (NaN? b) b (add a (multivector b (:signature a))))
     :else (multivector (concat (:blades a) (:blades b)) (:signature a))))
  ([a b & more] (reduce add (add a b) more)))

(defn sub
  "Return -a for one argument, a - b for two, a - b - c, etc."
  ([a]
   {:pre [(multivector? a)]}
   (if (number? a)
     (- a)
     (->MultiVector (mapv (fn [[x e]] [(- x) e]) (:blades a))
                    (:signature a))))
  ([a b] (add a (sub b)))
  ([a b & more] (reduce sub (sub a b) more)))

(defn prod
  "Return a * b, the geometric product of multivectors `a` and `b`."
  ([] 1)
  ([a] a)
  ([a b]
   {:pre [(same-algebra? a b)]}
   (cond
     (and (number? a) (number? b)) (* a b)
     (number? a) (if (NaN? a) a (prod (multivector a (:signature b)) b))
     (number? b) (if (NaN? b) b (prod a (multivector b (:signature a))))
     :else (let [sig (:signature a)
                 blades (for [[x ei] (:blades a)
                              [y ej] (:blades b)]
                          (let [eij (concat ei ej)
                                [elem factor] (simplify-element eij sig)]
                            [(* factor x y) elem]))]
             (multivector blades sig))))
  ([a b & more] (reduce prod (prod a b) more)))

(defn rev
  "Return a^~, the reverse of multivector `a`. Example: e12 -> e21 = -e12."
  [a]
  {:pre [(multivector? a)]}
  (if (number? a)
    a
    (let [keeps-sign? #(-> (count %) (quot 2) even?)
          transform-blade (fn [[x e]] [(if (keeps-sign? e) x (- x)) e])]
      (->MultiVector (mapv transform-blade (:blades a))
                     (:signature a)))))

(defn invol
  "Return a^^, the involution of multivector `a`. Example: 1 + e1 -> 1 - e1."
  [a]
  {:pre [(multivector? a)]}
  (if (number? a)
    a
    (let [keeps-sign? #(-> (count %) even?)
          transform-blade (fn [[x e]] [(if (keeps-sign? e) x (- x)) e])]
      (->MultiVector (mapv transform-blade (:blades a))
                     (:signature a)))))

(defn scalar?
  "Return true if `a` is a number or a multivector with only a scalar blade."
  [a]
  (or
   (number? a)
   (let [blades (:blades a)
         [_ elem] (first blades)]
     (assert (instance? MultiVector a) (str "not a multivector: " a))
     (or
      (empty? blades) ; a is like the number 0
      (and (= (count blades) 1) (= elem [])))))) ; a is like [[x []]]

(defn scalar
  "Return the given multivector `a` as a number (if it is a scalar)."
  [a]
  (if (number? a)
    a
    (let [blades (:blades a)
          [x _] (first blades)]
      (assert (instance? MultiVector a) (str "not a multivector: " a))
      (if (empty? blades) 0 x))))

(defn- try-scalar [a f] ; try to get a scalar from  a * f(a)
  (let [c (f a) ; candidate
        ac (prod a c)] ; a * c (will it be a scalar?)
    (when (scalar? ac)
      [c (scalar ac)])))

(defn inv
  "Return a^-1, the inverse of multivector `a` if it exists."
  [a]
  (if (number? a)
    (/ a)
    (let [[c ac] (or (try-scalar a rev) (try-scalar a invol))]
      (assert c (str "cannot find inverse of multivector: " a))
      (assert ((complement zero?) ac) (str "multivector has 0 norm: " a))
      (prod c (/ 1 ac)))))

(defn div
  "Return a / b = a * b^-1 (if `b` has an inverse)."
  ([a] (div 1 a))
  ([a b] (prod a (inv b)))
  ([a b & more] (reduce div (div a b) more)))

(defn pseudoscalar-unit
  "Return the pseudoscalar unit corresponding to signature `sig`."
  [sig]
  (->MultiVector [[1 (vec (keys sig))]] sig))

(defn dual
  "Return the dual of multivector `a`. Example: 2 e12 + e024 -> 2 e034 + e13.
  There are other types of duals, for example i*a or a*i. The dual in
  this function works even for degenerate algebras (algebras with i*i = 0)."
  [a]
  {:pre [(instance? MultiVector a)]}
  (let [indices (vec (keys (:signature a))) ; indices of all basis vectors
        e-dual #(vec (remove (set %) indices))] ; dual of basis multivector
    (->MultiVector (mapv (fn [[x e]] [x (e-dual e)]) (:blades a))
                   (:signature a))))

(defn grade
  "Grade-projection operator <a>_r (select only blades of the given grade `r`).
  Example: (grade (+ e1 e2 e0245) 1) -> (+ e1 e2)."
  [a r]
  {:pre [(multivector? a)]}
  (if (number? a)
    (if (zero? r) a 0)
    (->MultiVector (filterv (fn [[_ e]] (= (count e) r)) (:blades a))
                   (:signature a))))

(defn grades
  "Return the grades present in multivector `a`.
  Example: e1 + e2 + e0245 -> (1, 4)."
  [a]
  {:pre [(multivector? a)]}
  (if (number? a)
    [0]
    (distinct (map (comp count second) (:blades a)))))

;; For the following products, see [Dorst 2002] p. 37-39.

(defn dot
  "Return the dot product (inner product) of multivectors `a` and `b`."
  [a b]
  (if (or (number? a) (number? b))
    0
    (reduce add
            (for [r (grades a)
                  s (grades b)]
              (if (or (zero? r) (zero? s))
                0
                (-> (prod (grade a r) (grade b s)) ; <a>_r * <b>_s
                    (grade (abs (- r s))))))))) ; <  >_|r-s|

(defn- graded-prod
  "Return a \"graded product\" of multivectors `a` and `b`.
  That is a product that looks like sum_r,s ( < <a>_r * <b>_s >_g )
  where `g` is the grade obtained from r,s with (select-grade r s)."
  [a b select-grade]
  (cond
    (and (number? a) (number? b)) (* a b)
    (number? a) (graded-prod (multivector a (:signature b)) b select-grade)
    (number? b) (graded-prod a (multivector b (:signature a)) select-grade)
    :else (reduce add
                  (for [r (grades a)
                        s (grades b)]
                    (let [g (select-grade r s)]
                      (-> (prod (grade a r) (grade b s)) ; <a>_r * <b>_s
                          (grade g))))))) ; < >_g

(defn wedge
  "Return the wedge product (also exterior/outer) of multivectors `a` and `b`."
  [a b]
  (graded-prod a b +)) ; sum  < <a>_r * <b>_s >_(r+s)

(defn lcontract
  "Return the left contraction of multivectors `a` and `b`."
  [a b]
  (graded-prod a b (fn [r s] (- s r)))) ; sum  < <a>_r * <b>_s >_(s-r)

(defn rcontract
  "Return the right contraction of multivectors `a` and `b`."
  [a b]
  (graded-prod a b -)) ; sum  < <a>_r * <b>_s >_(r-s)

(defn scalar-prod
  "Return the scalar product of multivectors `a` and `b`."
  [a b]
  (scalar (graded-prod a b (fn [r s] 0)))) ; sum  < <a>_r * <b>_s >_0

(defn fat-dot
  "Return the \"fat dot\" product of multivectors `a` and `b`."
  [a b]
  (graded-prod a b (fn [r s] (abs (- r s))))) ; sum  < <a>_r * <b>_s >_|r-s|

(defn commutator
  "Return a x b, the commutator product of multivectors `a` and `b`."
  [a b]
  (-> (prod a b) (sub (prod b a)) (div 2))) ; (a * b - b * a) / 2

(defn antiwedge
  "Return the antiwedge product (also regressive/meet) of `a` and `b`."
  [a b]
  (dual (wedge (dual a) (dual b)))) ; (a* ^ b*)*

(defn proj ; see [Dorst 2002] p. 42
  "Return P_b(a), the projection of multivector `a` on `b`."
  [a b]
  (-> (lcontract a (inv b)) (lcontract b))) ; ( a _| b^-1 ) _| b

(defn rej
  "Return the rejection of multivector `a` on `b`."
  [a b]
  (sub a (proj a b))) ; a - P_b(a)

(defn norm ; "magnitude", see [Dorst 2002] p. 38, or [Hestenes 1984] p. 13
  "Return the magnitude of multivector `a`."
  [a]
  (math/sqrt (abs (scalar-prod (rev a) a)))) ; sqrt(|a^~ o a|)


;; More advanced operations.

(defn- blade-combos
  "Return all the different pairs of blades extracted from multivector `a`."
  [a]
  (let [blades (:blades a)
        sig (:signature a)
        blade (fn [i] (->MultiVector [(blades i)] sig)) ; 1-blade multivector
        n (count blades)]
    (for [i (range (dec n)) ; i = 0, 1, ..., n-2
          j (range (inc i) n)] ; j = i+1, i+2, ..., n-1
      [(blade i) (blade j)])))

(defn- all-blades-commute?
  "Return true if all the blades of multivector `a` commute."
  [a]
  (every? true? (for [[bi bj] (blade-combos a)]
                  (= (prod bi bj) (prod bj bi))))) ; bi * bj == bj * bi

(defn- exp-squared-scalar
  "Return the exponential of multivector `a` whose square is a scalar."
  [a]
  (let [a2 (scalar (prod a a)) ; a * a  (can be < 0)
        norm (math/sqrt (abs a2))]
    (cond
      (pos? a2) (add (math/cosh norm) (prod (/ (math/sinh norm) norm) a))
      (neg? a2) (add (math/cos  norm) (prod (/ (math/sin  norm) norm) a))
      :else (add 1 a))))

(defn- exp-blade
  "Return the exponential of a single blade, given its signature too."
  [blade signature]
  (exp-squared-scalar (->MultiVector [blade] signature))) ; blade as multivector

(defn sum-exp-series
  "Return exp(a) by adding the terms in its expansion in powers of `a`."
  [a & {:keys [precision max-terms] :or {precision 1e-8, max-terms 20}}]
  (loop [term-last 1 ; last term in the series evaluated
         sum-last 1 ; the sum of all the terms so far
         i 1] ; index of current term
    (let [term (prod term-last a (double (/ i))) ; next term
          size (apply + (for [[x _] (:blades term)] (abs x))) ; how big it is
          sum (add sum-last term)] ; our best approximation of exp(a) so far
      (if (< size precision)
        sum ; we are done! precise enough, so return the value
        (if (< i max-terms)
          (recur term sum (inc i)) ; keep adding terms from the expansion
          (do
            (printf (str "Warning: max terms reached (%d), but error (~ %g) "
                         "is bigger than the desired precision (%g)")
                    i size precision)
            sum)))))) ; return the best we could do with the max number of terms

(defn exp
  "Return exp(a), the exponential of multivector `a`."
  [a]
  (cond
    (number? a) (math/exp a)
    (scalar? (prod a a)) (exp-squared-scalar a) ; works for anticommuting blades
    (all-blades-commute? a) (let [sig (:signature a)] ; exp(b1 + b2) =
                              (reduce prod            ; exp(b1) * exp(b2)
                                      (for [b (:blades a)] (exp-blade b sig))))
    :else (sum-exp-series a)))

(defn log
  "Return log(a), the natural logarithm of multivector `a`."
  [a]
  (if (number? a)
    (math/log a) ; normal log, the easy case
    (let [x (scalar (grade a 0)) ; x = <a>  ("real part")
          ey (sub a x) ; e y = a - <a>  ("imaginary part" with imaginary unit)
          eyey (prod ey ey)]
      (assert (scalar? eyey) "unknown log") ; I haven't figured out other cases
      (let [ey2 (scalar eyey)] ; (ey)^2  (as a number)
        (if (zero? ey2) ; (ey)^2 = 0 ?  then it is  a = x exp(e y/x)
          (add (math/log x) (div ey x)) ; so  log(a) = log(x) + e y/x
          (let [y (math/sqrt (abs ey2)) ; sqrt(|(ey)^2|) = y  (but for a sign)
                e (div ey y)] ; "imaginary unit" (also absorbs sign)
            (if (pos? ey2) ; (ey)^2 > 0 -> a = sqrt(x^2-y^2) exp(e atanh(y/x))
              (if (neg? x)
                ##NaN ; x < 0 can't be reached with e^2 > 0 (same as normal log)
                (add (/ (math/log (- (* x x) ey2)) 2) ; log(x^2-y^2)/2 +
                     (prod e (ga-math/atanh (/ y x))))) ; e atanh(y/x)
              (add (/ (math/log (- (* x x) ey2)) 2) ; log(x^2+y^2)/2 +
                   (prod e (math/atan2 y x)))))))))) ; e atan2(y, x)

(defn- pow-to-int [a n] ; slow way to raise to an int (auxiliar function)
  (loop [a-pow-n 1 ; temporary value of a^|n|
         i (abs n)] ; we'll iterate |n| times to compute a^|n|
    (if (zero? i)
      (if (>= n 0) a-pow-n (inv a-pow-n)) ; a^|n|  or  1/a^|n|
      (recur (prod a-pow-n a) (dec i)))))

(defn- pow-special-cases [a b] ; raise power to easy numbers (fast and clean)
  (when (scalar? b) ; return nil if we are not in any of the easy cases
    (let [n (scalar b)] ; maybe small integer (note that (int? 1.0) is false)
      (when (and (== (int n) n) (< (abs n) 10)) ; small integer
        (pow-to-int a (int n))))))

(defn pow
  "Return a^b (`a` raised to the power of `b`)."
  [a b]
  (if (scalar? a)
    (if (scalar? b)
      (math/pow (scalar a) (scalar b)) ; the easy case
      (exp (prod b (math/log (scalar a))))) ; a^b  means  exp(b log(a))
    (or
     (pow-special-cases a b)
     (try
       (let [c (exp (prod b (log a)))] ; exp(b log(a))  but log(a) may fail
         (assert (not (and (number? c) (NaN? c)))) ; or log(a) may be NaN
         c)
       (catch AssertionError e ; last resort: we can if b is an integer
         (assert (and (scalar? b) (int? (scalar b)))
                 "can only raise this multivector to an integer")
         (pow-to-int a (scalar b)))))))

(defn cosh
  "Return cosh(a), the hyperbolic cosine of multivector `a`."
  [a]
  (if (scalar? a)
    (math/cosh (scalar a))
    (-> (add (exp a) (exp (sub a))) (div 2)))) ; (exp(a) + exp(-a)) / 2

(defn sinh
  "Return sinh(a), the hyperbolic sine of multivector `a`."
  [a]
  (if (scalar? a)
    (math/sinh (scalar a))
    (-> (sub (exp a) (exp (sub a))) (div 2)))) ; (exp(a) - exp(-a)) / 2

(defn tanh
  "Return tanh(a), the hyperbolic tangent of multivector `a`."
  [a]
  (if (scalar? a)
    (math/tanh (scalar a))
    (div (sinh a) (cosh a)))) ; sinh(a) / cosh(a)

(defn cos
  "Return cos(a), the cosine of scalar `a` (otherwise, use cos(a)=cosh(ia))."
  [a]
  (assert (scalar? a) "for non-scalars, choose \"i\" and use cos(a)=cosh(ia)")
  (math/cos (scalar a)))

(defn sin
  "Return sin(a), the sine of scalar `a` (otherwise, use sin(a)=sinh(ia)/i)."
  [a]
  (assert (scalar? a) "for non-scalars, choose \"i\" and use sin(a)=sinh(ia)/i")
  (math/sin (scalar a)))

(defn tan
  "Return tan(a), the tangent of scalar `a` (otherwise, use tan(a)=tanh(ia)/i)."
  [a]
  (assert (scalar? a) "for non-scalars, choose \"i\" and use tan(a)=tanh(ia)/i")
  (math/tan (scalar a)))


;; Basis.

(def algebra->signature
  (array-map
   "real" {},
   "complex" {1 -1},
   "quaternion" {1 +1, 2 +1, 3 +1} ; with i=e23, j=e13, k=e12
   "hyperbolic" {1 +1}
   "dual" {0 0}
   "2d" {1 +1, 2 +1}
   "3d" {1 +1, 2 +1, 3 +1}
   "aps" {1 +1, 2 +1, 3 +1} ; algebra of physical space, same as "3d"
   "sta" {0 +1, 1 -1, 2 -1, 3 -1} ; spacetime algebra
   "relativist" {0 -1, 1 +1, 2 +1, 3 +1} ; at times used by relativists
   "pga2d" {0 0, 1 +1, 2 +1} ; projective geometric algebra
   "pga3d" {0 0, 1 +1, 2 +1, 3 +1}
   "conformal2d" {1 +1, 2 +1, 3 +1, 4 -1}
   "conformal3d" {1 +1, 2 +1, 3 +1, 4 +1, 5 -1}))

(def algebra-synonyms
  {"r" "real", "c" "complex", "h" "quaternion", "d" "dual",
   "r2" "2d", "r3" "aps", "pga" "pga2d", "pga2" "pga2d",
   "conformal" "conformal2d", "conformal2" "conformal2d",
   "conformal3" "conformal3d"})

(defn name->signature
  "Return the signature (as a map) corresponding to the named algebra."
  [name]
  (let [n (str/lower-case name)]
    (algebra->signature (algebra-synonyms n n))))

(defn vector->signature
  "Return the signature as a map, corresponding to the given vector signature."
  [signature & {:keys [start]}]
  (let [[p q r_] signature
        r (or r_ 0)
        i0 (or start 1)]
    (zipmap (range i0 (+ i0 p q r))
            (concat (repeat p +1) (repeat q -1) (repeat r 0)))))

(defn- last?
  "Is `e` the last of the blades with number of vectors `n`?"
  [e n start]
  (= e (vec (range (- (+ start n) (count e)) (+ start n)))))

(defn- next-element
  "Return the multivector (in dim `n`) basis element next to `e`."
  [e n start]
  (if (last? e n start)
    (if (< (count e) n)
      (vec (range start (+ start (count e) 1)))
      nil) ; will stop when we iterate over next-element
    (let [pos (first
               (for [i (range (count e))
                     :when (not= (e (- (count e) (inc i)))
                                 (- (+ start n) (inc i)))]
                 (- (count e) (inc i))))]
      (loop [e-next (update e pos inc)
             i (inc pos)]
        (if (< i (count e-next))
          (recur (assoc e-next i (+ (e-next (dec i)) 1)) (inc i))
          e-next)))))

(defn basis
  "Return basis elements of a geometric algebra with the given signature."
  [signature & {:keys [start]}]
  {:pre [(or (vector? signature) (and (map? signature) (nil? start)))]}
  (let [sigmap (if (map? signature)
                 signature
                 (vector->signature signature :start start))
        n (count sigmap)
        i0 (when (> n 0) (apply min (keys sigmap)))]
    (take (math/pow 2 n)
          (for [e (iterate #(next-element % n i0) [])] ; e: basis element
            (->MultiVector [[1 e]] sigmap))))) ; as multivector


;; Macros to define basis elements and operators (great for the repl).

(defmacro def-basis
  "Create global vars with the names of the multivector basis."
  [signature & {:keys [start]}]
  (let [elems (rest (basis signature :start start))] ; exclude 1 (not a symbol)
    `(do
       ~@(for [e elems]
           `(def ~(symbol (str e)) ~e)) ; looks like (def e1 #object[e1])
       (println "Defined basis multivectors:" ~(str/join " " elems)))))

(def operators
  (array-map ; so they appear in order
   "+" #'add
   "-" #'sub
   "*" #'prod ; NOTE: "natural", but the similar ∗ is often scalar-prod in print
   "/" #'div
   "**" #'pow
   "·" #'dot
   "∧" #'wedge
   "∨" #'antiwedge
   "×" #'commutator
   "⌋" #'lcontract
   "⌊" #'rcontract
   "∘" #'scalar-prod ; NOTE: not standard, but we use the similar "*" for prod
   "∗" #'scalar-prod ; NOTE: often used in print, but looks similar to *
   "•" #'fat-dot))

(defmacro def-ops
  "Create global vars with multivector operators, replacing some core ones."
  []
  `(do
     (println "Replacing operators + - * / with generalized versions."
              "You may see the corresponding warnings.")
     ~@(for [[op f] operators]
         `(def ~(symbol op) ~f)) ; (def ~(symbol "+") add)  and so on
     (println "Defined operators:" ~(str/join " " (keys operators)))))


;; "Rich comment", with small examples on how to use the functions.

(comment

  (def a (multivector [[4 [2 3]]
                       [7 [3]]
                       [1 [1 4]]
                       [0 [1 2 3]]
                       [5 [1 4]]] {1 1, 2 -1, 3 1, 4 1, 5 1}))
  a
  (str a) ; => "7 e3 + 6 e14 + 4 e23"

  (def + add)
  (str (+ a a)) ; => "14 e3 + 12 e14 + 8 e23"

  (def - sub)
  (str (- a)) ; => "-7 e3 - 6 e14 - 4 e23"
  (str (- a a)) ; => "0"

  (def * prod)
  (str (* a 2)) ; => "14 e3 + 12 e14 + 8 e23"
  (str (* a a)) ; => "29 - 84 e134 + 48 e1234"

  (str (rev a)) ; => "7 e3 - 6 e14 - 4 e23"

  (str (div a (multivector [[4 [3]]] (:signature a)))) ; => "7/4 + e2 - 3/2 e134"

  (str (grade a 2)) ; => "6 e14 + 4 e23"

  (str (pow a 3)) ; => "-301 e3 + 954 e14 - 172 e23"

  (str (commutator a (multivector [[4 [3]]] (:signature a)))) ; => "16 e2"

  (str/join ", " (map str (basis {1 1, 2 1}))) ; => "1, e1, e2, e12"

  (let [[e e1 e2 e12] (basis {1 1, 2 1})]
    (str (add e1 (prod 3 e2)))) ; => "e1 + 3 e2"

  (let [[+ - * /] [add sub prod div]
        [e e1 e2 e12] (basis {1 1, 2 1} :start 3)]
    (str (+ e1 (* 3 e2)))) ; => "e1 + 3 e2"

  (let [[+ - * /] [add sub prod div]
        [e e0 e1 e01] (basis [1 1] :start 0)]
    (str (+ e0 (* 3 e1)))) ; => "e0 + 3 e1"

  )
