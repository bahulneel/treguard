(ns treguard.processor)

(defn stmt-name
  [proc stmt]
  (ffirst stmt))

(defmulti parse-stmt #'stmt-name)

(defn parse-process [def]
  (let [statments (->> def
                       (partition-by keyword?)
                       (partition-all 2)
                       (reduce parse-stmt {}))]
    (when-not (:name statments)
      (throw (IllegalArgumentException. "Processes must contain a process definition")))
    statments))

(defmethod parse-stmt :process [proc [_ args]]
  (let [[name & args] (first args)]
    (assoc proc
      :name name
      :args args
      :statements [])))

(defmethod parse-stmt :default [proc [name args]]
  (println "Unknown " name)
  proc)

(defmethod parse-stmt :when [proc [_ args]]
  (when-not (= 1 (count args))
    (throw (IllegalArgumentException. "When statements must have exactly one argumnet")))
  (update-in proc [:statements] conj {:op :when :cond (first args)}))

(defmethod parse-stmt :let [proc [_ args]]
  (update-in proc [:statements] conj {:op :let :args args}))
