(ns geometric-algebra.calculator
  "A geometric algebra interactive calculator."
  (:require [geometric-algebra.core :as ga]
            [geometric-algebra.infix :as infix]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.math :as math])
  (:import [org.jline.terminal TerminalBuilder]
           [org.jline.reader LineReaderBuilder]
           [org.jline.reader.impl.completer StringsCompleter])
  (:gen-class))

(def ^:private functions ; functions that can be called in the calculator
  (array-map ; so they appear in order
   'rev #'ga/rev, 'invol #'ga/invol, 'inv #'ga/inv, 'dual #'ga/dual,
   'grade #'ga/grade, 'norm #'ga/norm,
   'exp #'ga/exp, 'log #'ga/log, 'pow #'ga/pow,
   'cosh #'ga/cosh, 'sinh #'ga/sinh, 'tanh #'ga/tanh,
   'cos #'ga/cos, 'sin #'ga/sin, 'tan #'ga/tan,
   'proj #'ga/proj, 'rej #'ga/rej))

(def ^:private constants ; constants that can be used in the calculator
  {'pi math/PI, 'e math/E})

(defn args->signature [args] ; command-line arguments to proper map signature
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

(defn- info [signature]
  (str
   "Basis multivectors: " (str/join " " (rest (ga/basis signature))) "\n"
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
  (let [ops (-> (map str (keys ga/operators)) ; operators to expand
                (conj "=") ; expand around "=" too
                (#(remove #{"*"} %))) ; but do not expand * (because of **)
        s-expanded (reduce op-expand s ops)] ; expand all but *
    (str/replace s-expanded #"([^\*])\*([^\*])" "$1 * $2"))) ; expand * nicely

(defn- text->sexpr [text infix?] ; convert text into an s-expression
  (if-not infix?
    (edn/read-string text) ; easy case, read the s-expression and return it
    (-> text ; less easy case, we have an infix expression
        (str/replace #"," ") (") ; function arguments as separate expressions
        (#(str "(" % ")")) ; make the full text a single expression
        (edn/read-string) ; read (parse) it
        (infix/infix->sexpr)))) ; and transform from infix to s-expression

(defn- eval-with-env [sexpr env] ; eval s-expression in environment (as a map)
  (let [bindings (vec (mapcat vec env))] ; map -> flat vector (for bindings)
    (eval `(let ~bindings ~sexpr))))

(defn- text->val
  "Return value of evaluating text with the given environment (nil on error)."
  [text env infix?]
  (try
    (let [sexpr (text->sexpr text infix?)
          val (eval-with-env sexpr env)]
      val)
    (catch AssertionError e (println (.getMessage e)))
    (catch Exception e (println (.getMessage e)))))

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

(defn- entry-type [text] ; type of entry in text (:command, :assign, :eval)
  (cond
    (str/starts-with? text ":") :command ; execute special command
    (= "=" (second (str/split text #"\s+"))) :assign ; assign to variable
    :else :eval)) ; evaluation

(defn- map->str [m] ; {k1 v1, k2 v2} -> "k1 = v1, k2 = v2"
  (str/join ", " (map (fn [[k v]] (str k " = " v)) m)))

(defn- run-command [text env env0 signature]
  (let [command (first (str/split text #"\s+"))]
    (case command
      ":help" (println (help text env))
      ":env" (println (map->str (apply dissoc env (keys env0))))
      ":info" (println (info signature))
      (println "Unknonw command:" command "(use :help to see commands)"))))

(defn calc
  "REPL to get GA expressions and show their values."
  [signature & {:keys [infix? read-line-fn]
                :or {infix? true
                     read-line-fn (fn [] (print "> ") (flush) (read-line))}}]
  (let [basis (for [e (rest (ga/basis signature))] [(symbol (str e)) e])
        env0 (into {} (concat ga/operators functions constants basis))]
    (loop [env env0]
      (when-let [line (read-line-fn)] ; user input
        (when-not (#{":exit" ":quit"} (str/trim line))
          (let [text (ops-expand (str/trim line))] ; spaces around operators
            (case (entry-type text)
              :command (do
                         (run-command text env env0 signature)
                         (recur env))
              :assign (recur (add-var text env infix?))
              :eval (let [val (text->val text env infix?)]
                      (if (nil? val)
                        (recur env) ; just continue, no printing or saving
                        (do
                          (println "ans =" val) ; evaluation output
                          (recur (assoc env 'ans val)))))))))))) ; save value

(defn calc-with-jline [signature]
  (let [compl (-> (concat ; things to complete (+ * ... exp pow ... pi ...)
                   (keys ga/operators) (keys functions) (keys constants)
                   (rest (ga/basis signature)) [:help :env :info :exit])
                  (#(StringsCompleter. (mapv str %)))) ; the "Completer"
        term (.. (TerminalBuilder/builder) build) ; the "Terminal"
        reader (.. (LineReaderBuilder/builder) ; the "Reader"
                   (terminal term) (completer compl) build)]
    (try
      (calc signature :read-line-fn #(.readLine reader "> "))
      (catch org.jline.reader.EndOfFileException e nil) ; ctrl+d
      (catch org.jline.reader.UserInterruptException e nil)) ; ctrl+c
    (.close term)))

(defn -main [& args]
  (if (nil? args)
    (println usage)
    (when-let [signature (args->signature args)]
      (println "Geometric Algebra Calculator - signature" (str/join " " args))
      (println "Type :help for help, :exit to exit.")
      (calc-with-jline signature))))
