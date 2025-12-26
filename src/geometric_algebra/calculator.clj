(ns geometric-algebra.calculator
  (:require [geometric-algebra.core :as ga]
            [clojure.edn :as edn]
            [clojure.string :as str]))

(defn- eval-with-env [code env]
  (eval `(let ~(vec env) ~code)))

(defn- args->signature [args]
  (try
    (let [[a1 a2 a3 a4] args
          sig-name (ga/name->signature (or a1 "")) ; signature given as a name
          p     (parse-long (or a1 "2"))
          q     (parse-long (or a2 "0"))
          r     (parse-long (or a3 "0"))
          start (parse-long (or a4 "1"))]
      (or sig-name (ga/vector->signature [p q r] start)))
    (catch Exception e (do (println "Error: bad signature" args) {}))))

(defn calc
  "REPL to get infix GA expressions and show their values."
  [signature]
  (let [basis (rest (ga/basis signature)) ; basis multivectors
        funcs {'rev ga/rev, 'invol ga/invol, 'inv ga/inv, 'dual ga/dual,
               'grade ga/grade, 'pow ga/pow, 'norm ga/norm, 'proj ga/proj,
               'exp ga/exp}
        env (flatten (concat (for [e basis] [(symbol (str e)) e])
                             (for [[op f] ga/operators] [(symbol op) f])
                             funcs))
        op-expand (fn [s op] (str/replace s op (str " " op " ")))
        ops-expand (fn [s] (reduce op-expand s (keys ga/operators)))]
    (println "Basis multivectors:" (str/join " " basis)) ; "e1, e2, e12"
    (let [f (fn [[i sig]] (str "e" i "e" i "=" sig))] ; like "e1e1=-1"
      (println "Signature:" (str/join ", " (map f signature))))
    (println "Operators:" (str/join " " (keys ga/operators)))
    (loop []
      (print "> ")
      (flush)
      (let [line (read-line)] ; user input
        (when-not (nil? line) ; so ctrl+d exits
          (try
            (let [expr-str (str "(" (ops-expand line) ")")
                  expr (ga/infix->sexpr (edn/read-string expr-str))
                  val (eval-with-env expr env)]
              (when-not (nil? val) ; so it doesn't print "nil" if empty
                (println val))) ; result
            (catch Exception e (println (.getMessage e)))
            (catch AssertionError e (println (.getMessage e))))
          (recur))))))
