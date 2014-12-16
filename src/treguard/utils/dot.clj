(ns treguard.utils.dot
  (:require [dorothy.core :as dot]))

(defn get-process-def
  [process]
  (second process))

(defn inbound?
  [name process]
  (let [outbound (into #{} (filter keyword? process))]
    (outbound name)))

(defn get-condition
  [process]
  (second (drop-while #(not (= :when %)) process)))

(defn get-edges
  [system node]
  (let [name (first node)
        inbound (filter (partial inbound? name) system)
        nodes (map (juxt second get-condition) inbound)]
    (map (fn [[parent cond]]
           [(first parent) name {:label (str cond)}])
         nodes)))

(def global-nodes
  [[:put {:label "put(peer,m)"}]
   [:get {:label "get(peer)"}]
   [:fail {:label "fail"}]])

(defn system->dot
  [system]
  (let [nodes (->> system
                   (map get-process-def)
                   distinct
                   (map (fn [[n & args]]
                          (let [arg-list (apply str (interpose "," args))]
                            [n {:label (str (name n) "(" arg-list ")")}]))))
        nodes (concat nodes global-nodes)
        edges (->> nodes
                   (mapcat (partial get-edges system)))
        graph (dot/digraph (concat nodes edges))]
    (dot/dot graph)))
