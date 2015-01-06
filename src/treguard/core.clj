(ns treguard.core
  (:require [clojure.core.async :as async]
            [datomic-schema.schema :refer [schema fields] :as s]))

;; Let's think about a system as an entity
;; a name,
;; a unique id
;; and
;; a collection of processes.
(def system
  (schema system
          (fields
           [name :keyword :indexed]
           [id :uuid :indexed :unique-value]
           [processes :ref :many])))

;; Each process is also an entity with
;; a name,
;; a unique id,
;; a context,
;; some metrics,
;; and
;; a process state
(def process
  (schema process
          (fields
           [name :keyword :indexed]
           [id :uuid :indexed :unique-value]
           [context :ref :component]
           [metrics :ref :component]
           [state :enum [:created
                         :waiting
                         :running
                         :blocked
                         :terminated] :indexed])))

;; The process scheduler is responsible for
;; selecting which processes to run
;; based on their current state
;; and some fairness calulation on some metrics.
(def metrics
  (schema metric
          (fields
           [created :instant "When the process was created"]
           [terminated :instant "When the process was terminated"]
           [paused :instant "When the process was paused"]
           [blocked :instant "When the process was blocked"])))

;; An execution environment
;; implements the core memory of the system
;; and the process scheduler
(defprotocol ISchedule
  (created [this] "Select all the new processes")
  (running [this] "Select all the running processes, in schedule order")
  (terminated [this] "Select all the blocked processes, in schedule order")
  (waiting [this] "Select all the waiting processes, in schedule order")
  (blocked [this] "Select all the blocked processes, in schedule order")
  (cores [this] "The number of parallel processes")
  (update [this updates] "Update the env state and return the new state"))

;; We can ask the execution envirionment to update the state
;; but we need the correct representation
(defprotocol IState
  (combine [this u1 u2] "Combine 2 updates into a single update")
  (set-state [this process new-state] "Update the execution state")
  (set-ctx [this process reg val] "Set the value of a register in the context")
  (clear-ctx [this process reg val] "Clears the value of a register in the context"))

;; The dispatcher
;; selects all the running processes,
;; passes them through the execution pipeline transducer
;; then applies the results of the execution.
(defn dispatcher [env pipeline-xf]
  (let [processes (running env)
        c (cores env)
        to (async/chan c)
        from (async/chan c)]
    (async/pipeline c to pipeline-xf from)
    (async/onto-chan from processes)
    (update env (async/<!! (async/into [] to)))))

;; The short term scheduler
;; selects which of the waiting processes should be run next
;; then applys the change to the exec env
(defn short-term-schedule [env selector]
  (let [run (fn [process]
              (set-state env process :running))
        processes (->> env
                   waiting
                   selector
                   (map run))]
    (update env processes)))

;; the medium-term scheduler
;; selects which of the blocked processes are no longer blocked
;; then applys the change to the exec env
(defn long-term-schedule [env blocked?]
  (let [wait (fn [process]
              (set-state env process :waiting))
        processes (->> env
                   blocked
                   (remove blocked?)
                   (map wait))]
    (update env processes)))

;; the long-term scheduler
;; selects which of the new processes should be started next
;; then applys the change to the exec env
(defn long-term-schedule [env selector]
  (let [wait (fn [process]
              (set-state env process :waiting))
        processes (->> env
                   created
                   selector
                   (map wait))]
    (update env processes)))
