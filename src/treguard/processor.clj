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
  (update-in proc [:statements] conj {:op :call
                                      :process (first name)
                                      :args args}))

(defmethod parse-stmt :when [proc [_ args]]
  (when-not (= 1 (count args))
    (throw (IllegalArgumentException. "When statements must have exactly one argumnet")))
  (let [cond (-> args first clojure.walk/macroexpand-all)]
    (update-in proc [:statements] conj {:op :when :cond cond})))

(defmethod parse-stmt :let [proc [_ args]]
  (when-not (= 1 (count args))
    (throw (IllegalArgumentException. "Let statements must have exactly one argumnet")))
  (let [bindings (first args)]
    (when-not (sequential? bindings)
      (throw (IllegalArgumentException. "A let binding must be a sequence of bindings")))
    (when-not (even? (count bindings))
      (throw (IllegalArgumentException. "A binding sequence must contain an even number of forms")))
    (let [bindings (->> args
                        first
                        destructure
                        (partition 2)
                        (map (fn [[s expr]]
                               [s (clojure.walk/macroexpand-all expr)])))]
      (update-in proc [:statements] conj {:op :let :bindings bindings}))))
