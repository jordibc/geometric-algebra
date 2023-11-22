(ns geometric-algebra
  (:require [clojure.string :as str]))

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

(defmethod print-method MultiVector [a w]
  (.write w (str a))) ; so on the console the multivectors will look nice too


;; Simplifying a collection of blades to construct a "normalized" multivector.

(defn add-values
  "Return blade with the sum of the values of the given blades.
  It assumes (and doesn't check) that all blades have the same basis element."
  [blades]
  (let [[_ element] (first blades)] ; common element, like e13
    [(reduce + (map first blades)) element]))

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
  ([] 0)
  ([a] a)
  ([a b]
   (cond
     (and (number? a) (number? b)) (+ a b)
     (number? a) (add (multivector a (:signature b)) b)
     (number? b) (add a (multivector b (:signature a)))
     :else (do (assert (= (:signature a) (:signature b)) "different signatures")
               (multivector (concat (:blades a) (:blades b)) (:signature a)))))
  ([a b & more] (reduce add (add a b) more)))

(defn sub
  "Return  -a  for one argument, a - b  for two, a - b - c  etc."
  ([a] (if (number? a)
         (- a)
         (->MultiVector
          (for [[value element] (:blades a)] [(- value) element])
          (:signature a))))
  ([a b] (add a (sub b)))
  ([a b & more] (reduce sub (sub a b) more)))

(defn prod
  "Return  a * b , the geometric product of multivectors a and b."
  ([] 1)
  ([a] a)
  ([a b]
   (cond
     (and (number? a) (number? b)) (* a b)
     (number? a) (->MultiVector (for [[x e] (:blades b)] [(* a x) e])
                                (:signature b))
     (number? b) (->MultiVector (for [[y e] (:blades a)] [(* y b) e])
                                (:signature a))
     :else (let [signature (:signature a)]
             (assert (= (:signature b) signature) "different signatures")
             (multivector
              (for [[x ei] (:blades a), [y ej] (:blades b)]
                (let [[elem factor] (simplify-element (concat ei ej) signature)]
                  [(* factor x y) elem]))
              signature))))
  ([a b & more] (reduce prod (prod a b) more)))

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
  ([a] (if (number? a) (/ 1 a) (div (multivector 1 (:signature a)) a)))
  ([a b]
   (cond
     (and (number? a) (number? b)) (/ a b)
     (number? a) (div (multivector a (:signature b)) b)
     (number? b) (->MultiVector (for [[x e] (:blades a)] [(/ x b) e])
                                (:signature a))
     :else (let [b-r (rev b)
                 b-norm2 (scalar (prod b b-r)) ; fails if b*br is not a scalar
                 b-inv (div b-r b-norm2)]
             (prod a b-inv))))
  ([a b & more] (reduce div (div a b) more)))

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
        (if (>= n 0) v (div (multivector 1 (:signature a)) v))
        (recur (prod v a) (dec i))))))

(defn norm [a]
  (Math/sqrt (scalar (prod a (rev a)))))

(defn dot
  "Return the dot product (inner product) of multivectors a and b."
  [a b]
  (let [grades-a (set (for [[_ e] (:blades a)] (count e)))
        grades-b (set (for [[_ e] (:blades b)] (count e)))]
    (assert (and (= 1 (count grades-a)) (= 1 (count grades-b)))
            "can only dot blades (for the moment)")
    (let [ga (first grades-a)
          gb (first grades-b)]
      (assert (and (not= 0 ga) (not= 0 gb))
              "dot not defined (yet) for scalars")
      (-> (prod a b) (grade (abs (- ga gb)))))))

(defn wedge
  "Return the wedge product (exterior/outer product) of multivectors a and b."
  [a b]
  (let [grades-a (set (for [[_ e] (:blades a)] (count e)))
        grades-b (set (for [[_ e] (:blades b)] (count e)))]
    (assert (and (= 1 (count grades-a)) (= 1 (count grades-b)))
            "can only wedge blades (for the moment)")
    (let [ga (first grades-a)
          gb (first grades-b)]
      (assert (and (not= 0 ga) (not= 0 gb))
              "wedge not defined (yet) for scalars")
      (-> (prod a b) (grade (+ ga gb))))))

(defn commutator
  "Return  a x b , the commutator product of multivectors a and b."
  [a b]
  (-> (prod a b) (sub (prod b a)) (div 2))) ; (a * b - b * a) / 2


;; Basis.

(defn last?
  "Is e the last of the blades with that number of vectors?"
  ([e n] (last? e n 1))
  ([e n start] (= e (vec (range (- (+ start n) (count e)) (+ start n))))))

(defn next-element
  "Return the multivector (in dim n) base element next to e."
  [e n start]
  (if (last? e n start)
    (if (< (count e) n) (vec (range start (+ start (count e) 1))))
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
  ([signature] (basis signature nil))
  ([signature start]
   {:pre [(or (vector? signature) (map? signature))]}
   (if (vector? signature)
     (let [[p q r_] signature
           r (or r_ 0)
           i0 (or start 1)
           sigmap (zipmap (range i0 (+ i0 p q r))
                          (concat (repeat p +1) (repeat q -1) (repeat r 0)))]
       (basis sigmap nil))
     (let [n (count signature)
           i0 (apply min (keys signature))]
       (assert (nil? start) "cannot use start when using a map as signature")
       (take (Math/pow 2 n)
             (for [e (iterate #(next-element % n i0) [])] ; e: basis element
               (->MultiVector [[1 e]] signature))))))) ; as multivector

(defmacro def-basis
  "Create global vars with the names of the multivector basis."
  [signature]
  (let [elems (rest (basis signature))] ; all but "1", which is not a symbol
    `(do
       ~@(for [e elems]
           `(def ~(symbol (str e)) ~e)) ; looks like (def e1 #object[e1])
       (println "Defined basis multivectors:" ~(str/join " " elems)))))

(defmacro def-ops
  "Create global vars with multivector operators, replacing some core ones."
  []
  (let [operators {"+" add
                   "-" sub
                   "*" prod
                   "/" div
                   "·" dot
                   "∧" wedge}]
    `(do
       ~@(for [[op f] operators]
           `(def ~(symbol op) ~f)) ; (def ~(symbol "+") add)  and so on
       (println "Defined operators:" ~(str/join " " (keys operators))))))


(comment ; "Rich comment", with small examples of how to use the functions

  (def a (multivector [[4 [2 3]]
                       [7 [3]]
                       [1 [1 4]]
                       [0 [1 2 3]]
                       [5 [1 4]]] {1 1, 2 -1, 3 1, 4 1, 5 1}))
  a
  (str a) ; => "7*e3 + 6*e14 + 4*e23"

  (def + add)
  (str (+ a a)) ; => "14*e3 + 12*e14 + 8*e23"

  (def - sub)
  (str (- a)) ; => "-7*e3 + -6*e14 + -4*e23"
  (str (- a a)) ; => "0"

  (def * prod)
  (str (* a 2)) ; => "14*e3 + 12*e14 + 8*e23"
  (str (* a a)) ; => "29 + -84*e134 + 48*e1234"

  (str (rev a)) ; => "7*e3 + -6*e14 + -4*e23"

  (str (div a (multivector [[4 [3]]] (:signature a)))) ; => "7/4 + e2 + -3/2*e134"

  (str (grade a 2)) ; => "6*e14 + 4*e23"

  (str (pow a 3)) ; => "-301*e3 + 954*e14 + -172*e23"

  (str (commutator a (multivector [[4 [3]]] (:signature a)))) ; => "16*e2"

  (str/join ", " (map str (basis {1 1, 2 1}))) ; => "1, e1, e2, e12"

  (let [[e e1 e2 e12] (basis {1 1, 2 1})]
    (str (add e1 (prod 3 e2)))) ; => "e1 + 3*e2"

  (let [[+ - * /] [add sub prod div]
        [e e1 e2 e12] (basis {1 1, 2 1} 3)]
    (str (+ e1 (* 3 e2)))) ; => "e1 + 3*e2"

  (let [[+ - * /] [add sub prod div]
        [e e0 e1 e01] (basis [1 1] 0)]
    (str (+ e0 (* 3 e1)))) ; => "e0 + 3*e1"

  )