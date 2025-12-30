(ns geometric-algebra.calculator
  (:require [geometric-algebra.core :as ga]
            [clojure.edn :as edn]
            [clojure.string :as str]))

(defn- eval-with-env [code env]
  (let [bindings (vec (mapcat vec env))] ; map -> flat vector (for bindings)
    (eval `(let ~bindings ~code))))

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

(defn- op-expand [s op] ; put spaces around appearances of the given operator
  (str/replace s op (str " " op " ")))

(defn- ops-expand [s] ; put spaces around all operators
  (reduce op-expand s (keys ga/operators)))

(defn- line->expr [line infix?]
  (if-not infix?
    (edn/read-string line) ; easy case, read the s-expression and return it
    (-> line ; less easy case, we have an infix expression
        (ops-expand) ; put spaces around operators
        (str/replace #"," ") (") ; function arguments as sexps
        (#(str "(" % ")")) ; make the full line a single expression
        (edn/read-string) ; read (parse) it
        (ga/infix->sexpr)))) ; and transform from infix to sexp

(defn- info [basis signature functions]
  (str
   "Basis multivectors: " (str/join " " basis) "\n" ; "e1, e2, e12"
   "Signature: " (let [f (fn [[i sig]] (str "e" i "e" i "=" sig))] ; "e1e1=-1"
                   (str/join " " (map f signature))) "\n"
   "Functions: " (str/join " " (keys functions)) "\n"
   "Operators: " (str/join " " (keys ga/operators))))

(defn- show-help [line env]
  (let [func-name (str/trim (second (str/split line #" ")))
        var (or (resolve (symbol func-name)) ; for "+" and so on
                (resolve (symbol (str "geometric-algebra.core/" func-name))))]
    (println (:doc (meta var)))))

(defn- line->val [line env infix?]
  (try
    (let [expr (line->expr line infix?)
          val (eval-with-env expr env)]
      val)
    (catch Exception e (println (.getMessage e)))
    (catch AssertionError e (println (.getMessage e)))))

(defn calc
  "REPL to get GA expressions and show their values."
  ([signature] (calc signature true))
  ([signature infix?]
   (let [basis (rest (ga/basis signature)) ; basis multivectors
         funcs {'rev ga/rev, 'invol ga/invol, 'inv ga/inv, 'dual ga/dual,
                'grade ga/grade, 'pow ga/pow, 'norm ga/norm, 'exp ga/exp,
                'proj ga/proj, 'rej ga/rej}
         env0 (into {} (concat (for [e basis] [(symbol (str e)) e])
                               (for [[op f] ga/operators] [(symbol op) f])
                               funcs))]
     (println (info basis signature funcs))
     (loop [env env0]
       (print "> ")
       (flush)
       (let [line (read-line)] ; user input
         (when-not (nil? line) ; so ctrl+d exits
           (cond
             (str/starts-with? line "help ") (do
                                               (show-help line env)
                                               (recur env))
             (str/includes? line "=") (let [[var line] (str/split line #"\W*=")
                                            val (line->val line env infix?)
                                            env-new (assoc env (symbol var) val)]
                                        (recur env-new)) ; add var to environment
             :else (let [val (line->val line env infix?)]
                     (when-not (nil? val) ; so it doesn't print "nil" if empty
                       (println val)) ; result
                     (recur env)))))))))
