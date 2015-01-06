(ns treguard.core
  (:require [datomic-schema.schema :refer [schema fields] :as s]))

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
;; a process state,
;; and
;; an execution state
(def process
  (schema process
          (fields
           [name :keyword :indexed]
           [id :uuid :indexed :unique-value]
           [state :ref :component]
           [execution-state :enum [:created
                                   :waiting
                                   :running
                                   :blocked
                                   :terminated] :indexed])))
