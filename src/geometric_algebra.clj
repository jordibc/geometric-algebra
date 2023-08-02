(ns geometric-algebra
  (:require [clojure.string :as str])
  (:refer-clojure :exclude [+ - * /]))


;; The operations we are going to define for multivectors.

(declare + - * /)


;; The MultiVector record, and how to convert it to string.

(defn blade->str [[v e]]
  "Return a string that represents the blade. Examples: 3*e1, e23, 5."
  (let [hide-e (empty? e) ; hide the basis element for scalars (4, not 4*e)
        hide-v (and (= v 1) (not hide-e))] ; so we write e1 instead of 1*e1
    (str (if (not hide-v) v)                          ; 7
         (if (not (or hide-e hide-v)) "*")            ; *
         (if (not hide-e) (str "e" (apply str e)))))) ; e134

(defrecord MultiVector [blades signature]
  Object
  (toString [_]
    (if (empty? blades)
      "0"
      (str/join " + " (map blade->str blades))))) ; "7*e3 + 6*e14 + 4*e23"


;; Simplifying a collection of blades to construct a "normalized" multivector.

(defn add-values
  "Return blade with the sum of the values of the given blades.
  It assumes (and doesn't check) that all blades have the same basis element."
  [blades]
  (let [[_ element] (first blades)] ; common element, like e13
    [(reduce clojure.core/+ (map first blades)) element]))

(defn merge-same-elements
  "Return the blades, with the ones that have the same basis element combined.
  Example: e1 + 2*e2 + 3*e2  ->  e1 + 5*e2"
  [blades]
  (->> blades
       (partition-by second) ; group blades with same element
       (map add-values)))

(defn simplify-blades
  "Return the blades of a multivector simplified.
  Example: 3*e24 + 5*e7 + 0*e4 + e24  ->  5*e7 + 4*e24"
  [blades]
  (->> blades
       (sort-by second) ; sort by element
       (merge-same-elements) ; 2*e12 + 3*e12  ->  5*e12
       (filterv #(not= (first %) 0)))) ; remove terms that are 0

(defn simplify-element
  "Return the simplification of a basis element, and the factor it carries.
   Example: e13512  ->  e235, +1  (if  e1*e1 = +1)"
  [element signature]
  (loop [[e0 & e-rest] element
         result []
         factor +1]
    (let [e1 (first e-rest)]
      (cond
        (nil? e-rest) [(if e0 (conj result e0) result) factor] ; we are done!
        (= e0 e1) (recur (rest e-rest) ; repeated element -> contract
                         result
                         (* factor (if signature (signature e0) +1)))
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
  ([blades-or-num] (multivector blades-or-num nil))
  ([blades-or-num signature]
   (->MultiVector (if (number? blades-or-num)
                    [[blades-or-num []]] ; "upgrade" number to single blade
                    (simplify-blades blades-or-num)) ; just blades
                  signature)))


;; Operations.

(defn add
  "Return a + b."
  [a b]
  (multivector (concat (:blades a) (:blades b)) (:signature a)))

(defn sub
  "Return  -a  for one argument, and  a - b  for two."
  ([a] (->MultiVector
        (for [[value element] (:blades a)] [(- value) element])
        (:signature a)))
  ([a b] (+ a (- b))))

(defn prod
  "Return  a * b , the geometric product of multivectors a and b."
  [a b]
  {:pre [(or (number? b) (= (:signature a) (:signature b)))]}
  (let [signature (:signature a)]
    (if (number? b)
      (->MultiVector (for [[x e] (:blades a)] [(* x b) e])
                     signature)
      (multivector
       (for [[x ei] (:blades a), [y ej] (:blades b)]
         (let [[elem factor] (simplify-element (concat ei ej) signature)]
           [(* factor x y) elem]))
       signature))))

(defn rev
  "Return the reverse of multivector. For example: e12 -> e21 = -e12."
  [a]
  (if (number? a)
    a
    (let [keeps-sign #(-> (count %) (quot 2) even?)]
      (->MultiVector
       (for [[x e] (:blades a)] [(if (keeps-sign e) x (- x)) e])
       (:signature a)))))

(defn scalar?
  "Return true if it is a number or a multivector with only a scalar blade."
  [a]
  (or
   (number? a)
   (let [blades (:blades a)
         [_ elem] (first blades)]
     (or
      (empty? blades) ; a is like the number 0
      (and (= (count blades) 1) (= elem [])))))) ; a is like [[x []]]

(defn scalar
  "Return the given multivector as a number (if it is a scalar)."
  [a]
  {:pre [(scalar? a)]}
  (if (number? a)
    a
    (let [blades (:blades a)
          [x _] (first blades)]
      (if (empty? blades) 0 x))))

(defn div
  "Return  a / b = a * b-inv  (if b has an inverse)."
  [a b]
  (if (number? b)
    (->MultiVector (for [[x e] (:blades a)] [(/ x b) e])
                   (:signature a))
    (let [b-r (rev b)
          b-norm2 (scalar (* b b-r)) ; will fail if b*br is not a scalar
          b-inv (/ b-r b-norm2)]
      (* a b-inv))))

(defn grade
  "Grade-projection operator  <a>_r  (select only blades of the given grade)."
  [a r]
  (->MultiVector (filter #(= (count (second %)) r) (:blades a))
                 (:signature a)))

(defn pow
  "Return  a^n  (a raised to the nth power)."
  [a n]
  {:pre [(or (scalar? a) (int? n))]}
  (if (scalar? a)
    (Math/pow (scalar a) n)
    (loop [v (multivector 1 (:signature a))
           i (abs n)]
      (if (zero? i)
        (if (>= n 0) v (/ (multivector 1 (:signature a)) v))
        (recur (* v a) (dec i))))))

(defn commutator
  "Return  a x b , the commutator product of multivectors a and b."
  [a b]
  (-> (* a b) (- (* b a)) (/ 2))) ; (a * b - b * a) / 2


(defprotocol GAProto
  (+ [a] [a b] [a b c]) ; cannot do [a b & rest] in a protocol :(
  (- [a] [a b])
  (* [a] [a b] [a b c])
  (/ [a] [a b]))

(extend-protocol GAProto
  Number
  (+
    ([x] x)
    ([x y] (clojure.core/+ x y))
    ([x y z] (+ (+ x y) z)))
  (-
    ([x] (clojure.core/- x))
    ([x y] (clojure.core/- x y)))
  (*
    ([x] x)
    ([x y] (clojure.core/* x y))
    ([x y z] (* (* x y) z)))
  (/
    ([x] (/ 1 x))
    ([x y] (clojure.core// x y)))

  MultiVector
  (+
    ([a] a)
    ([a b] (add a b))
    ([a b c] (+ (+ a b) c)))
  (-
    ([a] (sub a))
    ([a b] (sub a b)))
  (*
    ([a] a)
    ([a b] (prod a b))
    ([a b c] (* (* a b) c)))
  (/
    ([a] (/ (multivector 1 (:signature a)) a))
    ([a b] (div a b))))



(comment

  (def a (multivector [[4 [2 3]]
                       [7 [3]]
                       [1 [1 4]]
                       [0 [1 2 3]]
                       [5 [1 4]]] {1 1, 2 -1, 3 1, 4 1, 5 1}))
  a
  (str a) ; => "7*e3 + 6*e14 + 4*e23"
  (str (+ a a))
  (str (- a))
  (str (- a a))
  (str (* a 2))
  (str (* a a)) ; => "29 + -84*e134 + 48*e1234"
  (str (rev a))

  (str (/ a (multivector [[4 [3]]] (:signature a))))
  (str (grade a 2))
  (str (pow a 3))

  (def a1 (multivector [[11 [2]] [1 [2 4]]]))
  (str a1)

  (commutator a (multivector [[4 [3]]] (:signature a)))

  (simplify-blades [[4 [2 3]]
                    [7 [3]]
                    [1 [1 4]]
                    [0 [1 2 3]]
                    [5 [1 4]]])


  ;; Trying to use a record for blades too.
  (defrecord Blade [value element])

  (defn sort-by-element
    "Return the same blades but sorted by their basis element."
    [blades]
    (sort-by #(let [e (:element %)] [(count e) e]) blades))

  (defn add-values
    "Return blade with the sum of the values of the given blades."
    [blades]
    (let [values (map :value blades)
          element (:element (first blades))]
      (->Blade (reduce + values) element)))

  (defn merge-same-elements
    [blades]
    (->> blades
         (partition-by :element)
         (map add-values)))

  (defn simplify-blades
    "Return the blades of a multivector simplified."
    [blades]
    (->> blades
         (sort-by-element)
         (merge-same-elements)
         (filter #(not= (:value %) 0))
         (into [])))

  ;; Alternative implementations.
  (defmulti + (fn [x y] [(class x) (class y)]))

  (defmethod + [Number multivector]
    [x y]
    (println "number + multivector"))
  (defmethod + [multivector multivector]
    [x y]
    (println "multi + multi"))
  (defmethod + :default
    [x y] (clojure.core/+ x y))

  (+ a b)

  (-> a (+ b))

  (defn merge-same-elements
    ([] [])
    ([x] [x])
    ([x y] (if (= (:element x) (:element y))
             (->Blade (clojure.core/+ (:value x) (:value y)) (:element x))
             [x y]))
    ([x y & rest] (recur (merge-same-elements x y) rest)))

  (defn as-maps [a] (->> (:blades a) (map #(merge {} %))))
  (defn as-vecs [a] (map (fn [blade] [(:value blade) (:element blade)]) (:blades a)))
  (defn as-vecs [a] (map #(vec [(:value %) (:element %)]) (:blades a)))

  (defn swap [v i j]
    "Return the given vector with the positions at the given indexes swapped."
    (-> v (assoc i (v j)) (assoc j (v i))))
  )
