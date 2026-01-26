(ns geometric-algebra.infix
  "Infix notation.")

(def ^:private arities ; function arities
  {'rev 1, 'invol 1, 'inv 1, 'dual 1, 'norm 1,
   'exp 1, 'log 1, 'cosh 1, 'sinh 1, 'tanh 1, 'cos 1, 'sin 1, 'tan 1,
   'grade 2, 'pow 2, 'proj 2, 'rej 2,
   '+ 1, '- 1}) ; special ones, used to parse "-1" and similar

(def ^:private precedences ; operator precedence (as a map {op prec})
  (-> {1 ['+ '-] ; lowest precedence
       2 ['* '/]
       3 ['· '∧ '∨ '× '⌋ '⌊ '∘ '∗ '•]
       4 ['**]} ; highest precedence
      (#(into {} (for [[p ops] %, op ops] [op p])))))

(defn- reduce-stack
  "Return a reduced stack of values and operations.
  The last element will have all operations with precedence >= `prec` applied."
  [stack prec]
  (let [[y op x & stack-more] stack] ; we use a list as a stack (`y` is last)
    (if (or (nil? op) (< (precedences op) prec))
      stack ; we are done!
      (let [z (list op x y)] ; x op y -> (op x y)
        (recur (conj stack-more z) prec)))))

(declare infix->sexpr)

(defn infix->sexpr-1
  "Return expression but with the first element converted to s-expression.
  Example: '(exp 1 + 2) -> '((exp 1) + 2)."
  [expr]
  (let [[x & more] expr
        n (arities x)]
    (if (nil? n)
      (conj more (infix->sexpr x)) ; return it as it was, but with x expanded
      (let [args (map infix->sexpr (take n more)) ; expand the arguments
            f1 (conj args x)] ; first element is a function call
        (conj (nthnext more n) f1))))) ; return function call and rest

(defn infix->sexpr
  "Return the s-expression corresponding to the given infix expression `expr`."
  [expr]
  (if-not (seq? expr)
    expr
    (let [[x & expr1] (infix->sexpr-1 expr)]
      (loop [[op & expr2] expr1
             stack (list x)]
        (if (nil? op)
          (peek (reduce-stack stack 0)) ; return the top of the reduced stack
          (if-not (contains? precedences op) ; no operator? will make it `*`
            (recur (conj expr2 op '*) ; recreate expr and add `*`
                   stack)
            (let [stack-reduced (reduce-stack stack (precedences op))
                  [y & expr3] (infix->sexpr-1 expr2)
                  stack-new (conj stack-reduced op y)]
              (recur expr3
                     stack-new))))))))

(defmacro infix
  "Evaluate the expression `expr` given in infix notation."
  [& expr]
  (infix->sexpr expr))


;; "Rich comment", with small examples on how to use the functions.

(comment

  (reduce-stack '(2) 3) ; => (2)
  (reduce-stack '(2 + 1) 3) ; => (2 + 1)
  (reduce-stack '(2 + 1) 0) ; => ((+ 1 2))
  (reduce-stack '(3 * 2 + 1) 2) ; => ((* 2 3) + 1)
  (reduce-stack '(3 * 2 + 1) 1) ; => ((+ 1 (* 2 3)))
  (reduce-stack '(4 • 3 * 2 + 1) 0) ; => ((+ 1 (* 2 (• 3 4))))
  (reduce-stack '(4 • 3 * 2 + 1) 1) ; => ((+ 1 (* 2 (• 3 4))))
  (reduce-stack '(4 • 3 * 2 + 1) 2) ; => ((* 2 (• 3 4)) + 1)
  (reduce-stack '(4 • 3 * 2 + 1) 3) ; => ((• 3 4) * 2 + 1)
  (reduce-stack '(4 • 3 * 2 + 1) 4) ; => (4 • 3 * 2 + 1)
  (reduce-stack '(4 • 3 * 2 + 1 • 2) 2) ; => ((* 2 (• 3 4)) + 1 • 2)

  (infix->sexpr 1) ; => 1
  (infix->sexpr '(1)) ; => 1
  (infix->sexpr '(1 + 2)) ; => (+ 1 2)
  (infix->sexpr '((1 + 2))) ; => (+ 1 2)
  (infix->sexpr '(1 + 2 * 3)) ; => (+ 1 (* 2 3))
  (infix->sexpr '(1 * 2 + 3)) ; => (+ (* 1 2) 3)
  (infix->sexpr '(1 * (2 + 3))) ; => (* 1 (+ 2 3))
  (infix->sexpr '(1 2)) ; => (* 1 2)
  (infix->sexpr '(1 2 + 3)) ; => (+ (* 1 2) 3)
  (infix->sexpr '((1 + 2) * 3)) ; => (* (+ 1 2) 3)

  (infix->sexpr-1 '(exp 1)) ; => ((exp 1))
  (infix->sexpr-1 '(exp 1 + 2)) ; => ((exp 1) + 2)
  (infix->sexpr-1 '(pow 1 2 + 2)) ; => ((pow 1 2) + 2)
  (infix->sexpr-1 '((1 + 2) + 3)) ; => ((+ 1 2) + 3)
  (infix->sexpr-1 '(1 + 2 + 3)) ; => (1 + 2 + 3)
  (infix->sexpr-1 '((1 + 2 + 3))) ; => ((+ (+ 1 2) 3))

  (infix->sexpr '(exp 1)) ; => (exp 1)
  (infix->sexpr '(exp 1 + 2)) ; => (+ (exp 1) 2)
  (infix->sexpr '(pow 1 2 + 2)) ; => (+ (pow 1 2) 2)
  (infix->sexpr '((1 + 2) + 3)) ; => (+ (+ 1 2) 3)
  (infix->sexpr '(1 + 2 + 3)) ; => (+ (+ 1 2) 3)
  (infix->sexpr '((1 + 2 + 3))) ; => (+ (+ 1 2) 3)

  (infix 1) ; => 1
  (infix 1 + 2) ; => 3
  (infix 1 + 2 + 3) ; => 6
  (infix 1 + 2 * 3) ; => 7
  (infix 2 * 3 + 4) ; => 10
  (infix 2 * (3 + 4)) ; => 14
  (infix 2 4 + 5) ; => 13

)
