(ns geometric-algebra.calculator
  "A geometric algebra interactive calculator."
  (:require [geometric-algebra.core :as ga]
            [geometric-algebra.infix :as infix]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.math :as math]))

(defn args->signature [args]
  (try
    (let [[a1 a2 a3 a4] args
          sig-name (ga/name->signature (or a1 "")) ; signature from algebra name
          p     (parse-long (or a1 "2"))
          q     (parse-long (or a2 "0"))
          r     (parse-long (or a3 "0"))
          start (parse-long (or a4 "1"))]
      (or sig-name (ga/vector->signature [p q r] :start start)))
    (catch Exception e (println "Error: bad signature" args))))

(def usage
  (str "Usage: calc <signature> (name, or p [q] [r] [start])\n"
       "Valid names: " (str/join " " (keys ga/algebra->signature)) "\n"
       "Examples: calc sta, calc 1 3 0 0"))

(def ^:private functions
  (array-map ; so they appear in order
   'rev #'ga/rev, 'invol #'ga/invol, 'inv #'ga/inv, 'dual #'ga/dual,
   'grade #'ga/grade, 'norm #'ga/norm,
   'exp #'ga/exp, 'log #'ga/log, 'pow #'ga/pow,
   'cosh #'ga/cosh, 'sinh #'ga/sinh, 'tanh #'ga/tanh,
   'cos #'ga/cos, 'sin #'ga/sin, 'tan #'ga/tan,
   'proj #'ga/proj, 'rej #'ga/rej))

(def ^:private constants
  {'pi math/PI, 'e math/E})

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
           "  :info           - shows information about the algebra\n"
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
  (let [ops (-> (keys ga/operators) ; operators to expand
                (conj "=") ; expand around "=" too
                (#(remove #{"*"} %))) ; but do not expand * (because of **)
        s-expanded (reduce op-expand s ops)] ; expand all but *
    (str/replace s-expanded #"([^\*])\*([^\*])" "$1 * $2"))) ; expand * nicely

(defn- text->expr [text infix?]
  (if-not infix?
    (edn/read-string text) ; easy case, read the s-expression and return it
    (-> text ; less easy case, we have an infix expression
        (str/replace #"," ") (") ; function arguments as sexps
        (#(str "(" % ")")) ; make the full text a single expression
        (edn/read-string) ; read (parse) it
        (infix/infix->sexpr)))) ; and transform from infix to sexp

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
  (cond
    (str/starts-with? text ":") :command ; execute special command
    (= "=" (second (str/split text #"\s+"))) :assign ; assign to variable
    :else :eval)) ; evaluation

(defn- map->str [m]
  (str/join ", " (map (fn [[k v]] (str k " = " v)) m)))

(defn- run-command [text env env0 basis signature]
  (let [command (first (str/split text #"\s+"))]
    (case command
      ":help" (println (help text env))
      ":env" (println (map->str (apply dissoc env (keys env0))))
      ":info" (println (info basis signature))
      (println "Unknonw command:" command "(use :help to see commands)"))))

(defn calc
  "REPL to get GA expressions and show their values."
  [signature & {:keys [infix?] :or {infix? true}}]
  (if-not signature
    (println "Cannot run calculator without a signature.")
    (let [basis (rest (ga/basis signature)) ; basis multivectors
          env0 (into {} (concat (for [e basis] [(symbol (str e)) e])
                                (for [[op f] ga/operators] [(symbol op) f])
                                functions
                                constants))]
      (println "Geometric Algebra Calculator")
      (println "Type :help for help, :exit to exit.")
      (loop [env env0]
        (print "> ") ; prompt
        (flush)
        (let [line (read-line)] ; user input
          (when-not (or (nil? line) (= ":exit" line) (= ":quit" line))
            (let [text (ops-expand (str/trim line))] ; spaces around operators
              (case (entry-type text)
                :command (do
                           (run-command text env env0 basis signature)
                           (recur env))
                :assign (recur (add-var text env infix?))
                :eval (let [val (text->val text env infix?)]
                        (when-not (nil? val) ; don't print "nil" on empty line
                          (println "ans =" val)) ; evaluation output
                        (recur (assoc env 'ans val))))))))))) ; keep answer
