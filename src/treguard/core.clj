(ns treguard.core
  (:require [clojure.core.async :refer [go-loop <! put!] :as async]))

(defn make-msg
  [method destination body & headers]
  {:method method
   :destination destination
   :headers (apply hash-map headers)
   :body body})

(defn make-service
  [name env chan-in chan-out]
  {:name name
   :env env
   :chan-in chan-in
   :chan-out chan-out})

(defmulti handle-start (fn [name _ _] name))

(defmulti handle-stop (fn [name _] name))

(defmulti handle-message (fn [name _ _ _] name))

(defn start-service!
  [service]
  (let [{:keys [name env chan-in chan-out]} service]
    (println "Starting:" name)
    (let [env (handle-start name env chan-out)]
      (assoc service
        :env env
        :handler
        (go-loop [env env]
          (if-let [msg (<! chan-in)]
            (do
              (println "Handling:" msg)
              (let [env (handle-message name env msg chan-out)]
                (recur env)))
            env))))))

(defn stop-service!
  [service]
  (let [{:keys [name env chan-in chan-out handler]} service]
    (println "Stopping:" name)
    (async/close! chan-in)
    (handle-stop name env)
    (let [env (async/<!! handler)]
      (async/close! chan-out)
      (-> service
          (dissoc :handler)
          (assoc :env env)))))

(defn send-service-msg!
  [service msg]
  (let [{:keys [chan-in]} service]
    (put! chan-in msg)
    service))

;; Service container
(defn service-container
  [id service-map chan-in chan-out]
  (make-service ::service-container
                {:id id
                 :services service-map}
                chan-in
                chan-out))

(defn add-service
  [container service]
  (let [name (:name service)]
    (assoc-in container [:env :services name] service)))

(defmethod handle-start ::service-container
  [_ env c-out]
  (reduce (fn [env [name service]]
            (println "About to start: " name)
            (let [{:keys [chan-out]} service]
              (async/pipe chan-out c-out false)
              (assoc-in env [:services name] (start-service! service))))
          env
          (:services env)))

(defmethod handle-stop ::service-container
  [_ env]
  (reduce (fn [env [name service]]
            (println "About to stop: " name)
            (assoc-in env [:services name] (stop-service! service)))
          env
          (:services env)))

(defn add-via
  [msg id]
  (update-in [:headers :via] (fnil conj #{}) id))

(defmethod handle-message ::service-container
  [_ env msg chan-out]
  (let [destination (:destination msg)]
    (if-let [chan-in (get-in env [:services destination :chan-in])]
      (put! chan-in msg)
      (put! chan-out msg))))
