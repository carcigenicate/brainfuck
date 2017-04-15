(ns brain-fuck.interpreter
  (:require [brain-fuck.state :as s]
            [clojure.string :as string]))

(def interpreter-state s/standard-new-state)

(def debug-state (s/new-state 30))

(def repl-state s/standard-new-state)

(def char-bindings {\+ s/inc-cell-at-pointer
                    \- s/dec-cell-at-pointer

                    \< s/move-pointer-left
                    \> s/move-pointer-right

                    \[ s/start-loop
                    \] s/close-loop

                    \, s/buffered-input-to-cell-at-pointer
                    \. s/output-cell-at-pointer})

(defn code-to-commands [^String code-str]
  (->> code-str
       (map char-bindings)
       (filter some?)
       (vec)))

(defn interpret
  "Interprets a string of Brain-Fuck code.
  Any invalid command characters are ignored."
  ([^String code debug-mode?]
   (s/run-commands (if debug-mode? debug-state interpreter-state)
                 (code-to-commands code)
                 debug-mode?))

  ([^String code]
   (interpret code false)))

(defmacro interpretM
  ([debug-mode? & code-syms]
   `(interpret ~(apply str code-syms) ~debug-mode?)))

(defn repl []
  (let [initial-state repl-state]
    (loop [state initial-state]

      (print ">: ")
      (flush)

      (let [input (read-line)]
        (case (string/lower-case input)
          "stop" nil

          "reset" (recur initial-state)

          (let [commands (code-to-commands input)
                new-state (s/run-commands state commands false)]
            (println "\n" (str new-state) "\n")
            (recur (assoc new-state :instruction-pointer 0))))))))

; ----- Test Programs -----

; ----- Working
(def hello-world "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.")

; Almost identical
(def hello-world2 "++++++++[>++++[>++>+++>+++>+<<<<-]>+>->+>>+[<]<-]>>.>>---.+++++++..+++.>.<<-.>.+++.------.--------.>+.>++.")

(def echo-input ",[.[-],]")

; ----- Not working
(def fib "+++++++++++>+>>>>++++++++++++++++++++++++++++++++++++++++++++ >++++++++++++++++++++++++++++++++<<<<<<[>[>>>>>>+>+<<<<<<<-]>>>>>>>[<<<<<<<+>>>>>>>-]<[>++++++++++[- <-[>>+>+<<<-]>>>[<<<+>>>-]+<[>[-]<[-]]>[<<[>>>+<<<-]>>[-]]<<]>>>[>>+>+<<<-]>>>[<<<+>>>-]+<[>[-]<[-]]>[<<+>>[-]]<<<<<<<]>>>>>[++++++++++++++++++++++++++++++++++++++++++++++++.[-]]++++++++++<[->-<]>++++++++++++++++++++++++++++++++++++++++++++++++.[-]<< <<<<<<<<<<[>>>+>+<<<<-]>>>>[<<<<+>>>>-]<-[>>.>.<<<[-]]<<[>>+>+<<<-]>>>[<<<+>>>-]<<[<+>-]>[<+>-]<<<-]")

(def fib2 ">++++++++++>+>+[[+++++[>++++++++<-]>.<++++++[>--------<-]+<<<]>.>>[[-]<[>+<-]>>[<<+>+>-]<[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>+<-[>[-]>+>+<<<-[>+<-]]]]]]]]]]]+>>>]<<<]")

(defn first-mult [^long n]
  (let [abs-n (Math/abs n)]
    (reduce (fn [_ f]
              (when (zero? (rem abs-n f))
                (reduced [f (/ abs-n f)])))
            nil
            (range 2 abs-n))))
#_
(defn stats []
  (let [codes (mapv int "Hello World!\n")
        diffs (mapv (fn [n m] (- n m)) codes (cons 0 codes))
        mults (mapv first-mult diffs)]
    (clojure.pprint/pprint
      [codes
       diffs
       mults])))

(def my-hello-world
  "++++++++++++++++++++++++++++++++++++ 36
  [>++<-] times 2
  >. ; print H
  <+++++++ 7
  [>++++<-]>+ times 4 plus 1
  .  print e
  +++++++
  .. print ll
  <+++++++ -7
  [>-----------<--]+
  . print o")

(def my-hello-world-exp
  "++++++[>++++++<-] 36
  [>++<-] times 2
  >. ; print H
  <+++++++ 7
  [>++++<-]>+ times 4 plus 1
  .  print e
  +++++++
  .. print ll
  <+++++++ -7
  [>-----------<-]+ times -11 plus 1
  . print o")

