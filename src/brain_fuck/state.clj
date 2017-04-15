(ns brain-fuck.state
  "A Brain-Fuck interpreter implementation.
  Cells values are limited to the range 0-255.
  The cell-pointer wraps if instructed to go left of 0, or right of the max cell.wsaq"
  (:require [helpers.general-helpers :as g]
            [clojure.string :as s]))

(declare pprint-state)
(defrecord Program-State [instruction-pointer cell-pointer loop-anchors cells]
  Object
  (toString [self] (pprint-state self)))

(defn new-state [n-cells]
  (->Program-State
    0
    0
    []
    (vec (repeat n-cells 0))))

(def standard-new-state (new-state 30000))

(defn rev-drop-zeros [cells]
  (->> cells
       (reverse)
       (drop-while zero?)
       (reverse)
       (vec)))

(defn pprint-state [state]
  (let [limited-cells (update state :cells rev-drop-zeros)]
    (str "<" (s/join " " (vals limited-cells)) ">")))

(defn syntax-error [^String cause]
  (RuntimeException.
    (str "Syntax Error: " cause)))

(defn check-anchors-non-empty [state]
  (when (empty? (:loop-anchors state))
    (throw (syntax-error "Unmatched ]."))))

; ----- Instruction Pointer

(defn inc-instruction-pointer [state]
  (update state :instruction-pointer inc))

; ----- Inc / Dec

(defn- effect-cell-at-pointer [state f]
  (let [i (:cell-pointer state)]
    (update-in state [:cells i]
      #(g/wrap (f %) 0 255))))

(defn inc-cell-at-pointer [state]
  (effect-cell-at-pointer state inc))

(defn dec-cell-at-pointer [state]
  (effect-cell-at-pointer state dec))

; ----- Pointer Left / Right

(defn- effect-pointer [state f]
  (let [cells (:cells state)
        n-cells (count cells)]
    (update state :cell-pointer
      #(g/wrap (f %) 0 (dec n-cells)))))

(defn move-pointer-left [state]
  (effect-pointer state dec))

(defn move-pointer-right [state]
  (effect-pointer state inc))

; ----- Input/Output

(defn output-cell-at-pointer
  "Assumes the current cell value is a valid character code."
  [state]
  (let [{cells :cells cp :cell-pointer} state
        raw-output (cells cp)
        output-char (char raw-output)]

    (print output-char)
    (flush)

    state))

(defn buffered-input-to-cell-at-pointer
  "Reads in a string, and sets the current cell value to the ASCII code of the first letter.
  Must enter a newline after the input."
  [state]
  (effect-cell-at-pointer state
    (constantly
      (-> (read-line)
          (first)
          (int)))))

; ----- Loop

(defn- current-loop-anchor-index [state]
  (-> state
      (:loop-anchors)
      (last)))

(defn start-loop [state]
  (update state :loop-anchors
    #(conj % (:instruction-pointer state))))

(defn- unchecked-loop-jump [state]
  (assoc state :instruction-pointer
               (current-loop-anchor-index state)))

(defn checked-loop-jump [state]
  (check-anchors-non-empty state)

  (unchecked-loop-jump state))

(defn- unchecked-loop-end [state]
  (update state :loop-anchors
    #(vec (drop-last %))))

(defn checked-loop-end [state]
  (check-anchors-non-empty state)

  (unchecked-loop-end state))

(defn close-loop [state]
  (let [{cp :cell-pointer cs :cells} state]
    (if (zero? (cs cp))
      (checked-loop-end state)
      (checked-loop-jump state))))

; ---- Run

(defn run-command
  ([state command]
   (-> state
       (command)
       (inc-instruction-pointer))))

(defn run-commands [state commands debug-mode?]
  (let [; Prints when debug mode is active, returning the original state
        debug-print #(do (when debug-mode? (println (str %)))
                         %)]
    (loop [{ip :instruction-pointer :as s} state]

      (if-let [command (get commands ip nil)]
        (recur
          (debug-print
            (run-command s command)))

        (debug-print s)))))