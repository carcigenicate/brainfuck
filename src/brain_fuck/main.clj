(ns brain-fuck.main
  (:require [clojure.string :as s]
            [brain-fuck.interpreter :as i]
            [clojure.string :as string])

  (:gen-class))

(defn -main [^String mode & [^String path?-or-debug? debug?]]
  (println "h for help")

  (let [mode-lower (string/lower-case mode)
        debug-str? (str debug?) ; So it can be run from the REPL using boolean constants
        path-str? (str path?-or-debug?)
        debug-char? (when debug? (Character/toLowerCase ^Character (first debug-str?)))
        debug-mode? (and debug? (= debug-char? \t))]

    (case (first mode-lower)
      \i (let [path? (if debug? path?-or-debug? nil)
                code (if path? (slurp path?) nil)]
            (if path?
              (i/interpret code debug-mode?)
              (println "Enter a path to read from.")))

      \r (i/repl)

      \h (do
           (println "To interpret a file, type: i <path> <debug-mode?>")
           (println "To start the REPL, type r")))))

