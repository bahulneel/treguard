(ns treguard.proc
  (:require [treguard.automaton :as a]))

(defprotocol IRunnable
  (-run [self input]))

(defrecord Proc [state automaton tasks out]
  IRunnable
  (-run [self input]
    (let [ts (sort-by #(get tasks (a/aname %))
                      (a/tasks automaton input state))]
      (if-let [task (first ts)]
        (let [tn (a/aname task)
              [state' out] (a/exec task)]
          (-> self
              (assoc :out out)
              (assoc :state state')
              (update-in [:tasks tn] (fnil inc 0))))
        self))))

(defn proc
  [a & args]
  {:pre [(a/automaton? a)]}
  (let [state (a/init a args)]
    (->Proc state automaton nil)))

(defn run
  [proc input]
  {:pre [(satisfies? IRunnable proc)]}
  (-run proc input))
