(ns geometric-algebra.calculator
  (:require [geometric-algebra.core :as ga]
            [clojure.edn :as edn]
            [clojure.string :as str]))

(defn args->signature [args]
  (try
    (let [[a1 a2 a3 a4] args
          sig-name (ga/name->signature (or a1 "")) ; signature from algebra name
          p     (parse-long (or a1 "2"))
          q     (parse-long (or a2 "0"))
          r     (parse-long (or a3 "0"))
          start (parse-long (or a4 "1"))]
      (or sig-name (ga/vector->signature [p q r] start)))
    (catch Exception e (do (println "Error: bad signature" args) {}))))

(def ^:private functions
  {'rev #'ga/rev, 'invol #'ga/invol, 'inv #'ga/inv, 'dual #'ga/dual,
   'grade #'ga/grade, 'pow #'ga/pow, 'norm #'ga/norm, 'exp #'ga/exp,
   'proj #'ga/proj, 'rej #'ga/rej})

(defn- info [basis signature]
  (str
   "Basis multivectors: " (str/join " " basis) "\n" ; "e1, e2, e12"
   "Signature: " (let [f (fn [[i sig]] (str "e" i "e" i "=" sig))] ; "e1e1=-1"
                   (str/join " " (map f signature))) "\n"
   "Functions: " (str/join " " (keys functions)) "\n"
   "Operators: " (str/join " " (keys ga/operators))))

(defn- help [text env]
  (let [[_ func-name] (str/split text #"\s+")]
    (if (nil? func-name)
      (str "Type any expression to get its value. "
           "Use assignments like 'a = 2' to create new variables.\n"
           "Special commands:\n"
           "  :help <symbol>  - provides help for functions and operators\n"
           "  :env            - shows the variables in the environment\n"
           "  :exit, :quit    - exits the calculator")
      (let [mdata (meta (env (symbol func-name)))]
        (if (nil? mdata)
          (str "Cannot find documentation for function: " func-name)
          (str "  " func-name " " (:arglists mdata) "\n"
               "  " (:doc mdata)))))))

(defn- op-expand [s op] ; put spaces around appearances of the given operator
  (str/replace s op (str " " op " ")))

(defn- ops-expand [s] ; put spaces around all operators
  (reduce op-expand s (conj (keys ga/operators) "="))) ; expand around "=" too

(defn- text->expr [text infix?]
  (if-not infix?
    (edn/read-string text) ; easy case, read the s-expression and return it
    (-> text ; less easy case, we have an infix expression
        (str/replace #"," ") (") ; function arguments as sexps
        (#(str "(" % ")")) ; make the full text a single expression
        (edn/read-string) ; read (parse) it
        (ga/infix->sexpr)))) ; and transform from infix to sexp

(defn- eval-with-env [expr env]
  (let [bindings (vec (mapcat vec env))] ; map -> flat vector (for bindings)
    (eval `(let ~bindings ~expr))))

(defn- text->val
  "Return value of evaluating text with the given environment (nil on error)."
  [text env infix?]
  (try
    (let [expr (text->expr text infix?)
          val (eval-with-env expr env)]
      val)
    (catch Exception e (println (.getMessage e)))
    (catch AssertionError e (println (.getMessage e)))))

(defn- add-var
  "Return environment with the result of evaluating text like `var = expr`."
  [var-text env infix?]
  (let [[var text] (str/split var-text #"\s*=")] ; var = expr
    (if (and (re-matches #"\w+" var) (re-find #"^\D" var)) ; valid name
      (let [val (text->val text env infix?)]
        (assoc env (symbol var) val)) ; new environment
      (do
        (println "Invalid name:" var)
        env)))) ; no change in environment

(defn- entry-type [text]
  (let [parts (str/split text #"\s+")]
    (cond
      (str/starts-with? text ":") :command ; execute special command
      (= "=" (second parts))      :assign  ; assign to variable
      :else                       :eval))) ; evaluation

(defn- run-command [text env env0]
  (let [command (first (str/split text #"\s+"))]
    (case command
      ":help" (println (help text env))
      ":env"  (println (apply dissoc env (keys env0)))
      (println "Unknonw command:" command))))

(defn calc
  "REPL to get GA expressions and show their values."
  ([signature] (calc signature true))
  ([signature infix?]
   (let [basis (rest (ga/basis signature)) ; basis multivectors
         env0 (into {} (concat (for [e basis] [(symbol (str e)) e])
                               (for [[op f] ga/operators] [(symbol op) f])
                               functions))]
     (println (info basis signature))
     (println "Type :help for help, :exit to exit.")
     (loop [env env0]
       (print "> ") ; prompt
       (flush)
       (let [line (read-line)] ; user input
         (when-not (or (nil? line) (= ":exit" line) (= ":quit" line))
           (let [text (ops-expand (str/trim line))] ; spaces around operators
             (case (entry-type text)
               :command (do
                          (run-command text env env0)
                          (recur env))
               :assign (recur (add-var text env infix?))
               :eval (let [val (text->val text env infix?)]
                       (when-not (nil? val) ; don't print "nil" on empty line
                         (println val)) ; evaluation output
                       (recur (assoc env 'ans val))))))))))) ; keep answer
