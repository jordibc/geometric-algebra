(ns geometric-algebra.calc-jline
  "Geometric Algebra Calculator using jline for line editing and completion."
  (:require [geometric-algebra.core :as ga]
            [geometric-algebra.calculator :as calc]
            [clojure.string :as str])
  (:import (org.jline.terminal TerminalBuilder)
           (org.jline.reader LineReaderBuilder)
           (org.jline.reader.impl.completer StringsCompleter))
  (:gen-class))

(defn calc-with-jline [signature]
  (let [words (concat ; to complete (+ ... pow ... pi ... e1 ... :help ...)
               (keys ga/operators) (keys calc/functions) (keys calc/constants)
               (rest (ga/basis signature)) [:help :env :info :exit])
        compl (StringsCompleter. (mapv str words)) ; the "Completer"
        term (.. (TerminalBuilder/builder) build) ; the "Terminal"
        reader (.. (LineReaderBuilder/builder) ; the "Reader"
                   (terminal term) (completer compl) build)]
    (try
      (calc/calc signature :read-line-fn #(.readLine reader "> "))
      (catch org.jline.reader.EndOfFileException e nil) ; ctrl+d
      (catch org.jline.reader.UserInterruptException e nil) ; ctrl+c
      (finally (.close term)))))

(defn -main [& args]
  (if (nil? args)
    (println calc/usage)
    (if-let [signature (calc/args->signature args)]
      (do
        (println "Geometric Algebra Calculator - signature" (str/join " " args))
        (println "Type :help for help, :exit to exit.")
        (calc-with-jline signature))
      (println "Bad signature:" (str/join " " args)))))
