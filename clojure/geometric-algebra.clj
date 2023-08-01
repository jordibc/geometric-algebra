(ns geometric-algebra
  (:require [clojure.string :as str])
  (:refer-clojure :exclude [+ - *]))

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
      (str/join " + " (map blade->str blades)))))

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
       (filter #(not= (first %) 0)))) ; remove terms that are 0

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
                         (clojure.core/* factor (if signature (signature e0) +1)))
        (> e0 e1) (if (empty? result) ; unsorted order -> swap
                    (recur (concat [e0] (rest e-rest))
                           [e1]
                           (clojure.core/* factor -1)) ; these vectors anticommute
                    (recur (concat [(last result) e1 e0] (rest e-rest))
                           (vec (butlast result)) ; so we keep comparing this
                           (clojure.core/* factor -1)))
        :else (recur e-rest ; nothing to do at this position -> advance
                     (conj result e0)
                     factor)))))

(defn multivector ; "constructor"
  "Create a new multivector."
  ([blades] (multivector blades nil))
  ([blades signature] (->MultiVector (into [] (simplify-blades blades)) signature)))

(defn add [a b]
  (multivector (concat (:blades a) (:blades b)) (:signature a)))

(defn sub
  ([a] (->MultiVector
        (for [[value element] (:blades a)] [(clojure.core/- value) element])
        (:signature a)))
  ([a b] (add a (sub b))))

(defn prod [a b]
  (let [signature (:signature a)]
    (multivector
     (for [[x ei] (:blades a), [y ej] (:blades b)]
       (let [[elem factor] (simplify-element (concat ei ej) signature)]
         [(clojure.core/* factor x y) elem]))
     signature)))

(defn reverse
  "Return the reverse of multivector. For example: e12 -> e21 = -e12."
  [{:keys [blades signature]}]
  (let [keeps-sign #(-> (count %) (quot 2) (mod 2) zero?)]
    (->MultiVector
     (for [[x e] blades] [(if (keeps-sign e) x (clojure.core/- x)) e])
     signature)))

(defprotocol GAProto
  (+ [a b])
  (- [a] [a b])
  (* [a b]))

(extend-protocol GAProto
  Number
  (+ [a b] (clojure.core/+ a b))
  (-
    ([a] (clojure.core/- a))
    ([a b] (clojure.core/- a b)))
  (* [a b] (clojure.core/* a b))

  MultiVector
  (+ [a b] (add a b))
  (-
    ([a] (sub a))
    ([a b] (sub a b)))
  (* [a b] (prod a b)))



(comment

  (simplify-element [3 2 3] nil)

  (def a (multivector [[4 [2 3]]
                       [7 [3]]
                       [1 [1 4]]
                       [0 [1 2 3]]
                       [5 [1 4]]] {1 1, 2 -1, 3 1, 4 1, 5 1}))
  a
  (str a)
  (str (+ a a))
  (str (- a))
  (str (- a a))
  (str (* a a))
  (str (reverse a))

  (def a1 (multivector [[11 [2]] [1 [2 4]]]))
  (str a1)

  (simplify-element [5 4 1 2 3] nil)

  (simplify-element [5 4 2 2 3] {2 -1, 3 1, 4 1, 5 1})

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
      (->Blade (reduce clojure.core/+ values) element)))

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

  ;; To visualize.
  (defn b->vec [b] [(:value b) (:element b)])
  (defn bs->vec [bs] (map b->vec bs))
  (defn v->vec [v] (bs->vec (:blades v)))

  (def a (->MultiVector [(->Blade 4 [2 3])
                         (->Blade 7 [3])
                         (->Blade 1 [1 4])
                         (->Blade 0 [1 2 3])
                         (->Blade 5 [1 4])]
                        nil))

  (v->vec a)

  (bs->vec (sort-by-element (:blades a)))

  (add-values (:blades a))

  (map bs->vec (partition-by :element (sort-by-element (:blades a))))
  (first (partition-by :element (sort-by-element (:blades a))))
  (bs->vec (merge-same-elements (sort-by-element (:blades a))))

  (bs->vec (simplify-blades (:blades a)))

  (map #(apply ->Blade %) [[1 [2]] [4 [1 3]]])
  (def a2 (multivector [[1 [1]] [4 [1 3]]]))
  (v->vec a2)

  ;; Alternative implementations.
  (defmulti + (fn [x y] [(class x) (class y)]))

  (defmethod + [java.lang.Number multivector]
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
