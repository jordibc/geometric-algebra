(ns jordibc.calculator
  (:require [jordibc.geometric-algebra :as ga]
            [clojure.edn :as edn]
            [clojure.string :as str]))

(defn- eval-with-env [code env]
  (eval `(let ~(vec env) ~code)))

(defn- args->signature [args]
  (let [[a1 a2 a3 a4] args
        sig-name (ga/name->signature (or a1 "")) ; if signature given as a name
        p     (parse-long (or a1 "2"))
        q     (parse-long (or a2 "0"))
        r     (parse-long (or a3 "0"))
        start (parse-long (or a4 "1"))]
    (or sig-name (ga/vector->signature [p q r] start))))

(defn calc
  [args]
  (let [signature (args->signature args)
        basis (rest (ga/basis signature)) ; basis multivectors
        env (flatten (concat
                      (for [e basis] [(symbol (str e)) e])
                      (for [[op f] ga/operators] [(symbol op) f])))
        op-expand (fn [s op] (str/replace s op (str " " op " ")))
        ops-expand (fn [s] (reduce op-expand s (keys ga/operators)))]
    (println "Basis multivectors:" (str/join " " basis))
    (let [f (fn [[ei sig]] (str "e" ei "e" ei "=" sig))]
      (println "Signature:" (str/join ", " (map f signature))))
    (println "Operators:" (str/join " " (keys ga/operators)))
    (loop []
      (print "> ")
      (flush)
      (let [line (read-line)]
        (when-not (nil? line) ; so ctrl+d exits
          (try
            (let [expr-str (str "(" (ops-expand line) ")")
                  expr (ga/infix->sexpr (edn/read-string expr-str))
                  val (eval-with-env expr env)]
              (when-not (nil? val)
                (println val)))
            (catch Exception e (println (.getMessage e))))
          (recur))))))
