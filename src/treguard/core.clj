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
    (go-loop [env (handle-start name env chan-out)]
      (when-let [msg (<! chan-in)]
        (let [env (handle-message name env msg chan-out)]
          (recur env))))
    service))

(defn stop-service!
  [service]
  (let [{:keys [name env chan-in chan-out]} service]
    (async/close! chan-in)
    (handle-stop name env)
    (async/close! chan-out)
    service))

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

(defn handle-start ::service-container
  [_ env c-out]
  (doseq [[name service] (:services env)]
    (let [{:keys [chan-out]} service]
      (async/pipe chan-out)
      (start-service! service)))
  env)

(defn handle-stop ::service-container
  [_ env]
  (doseq [[_ service] (:services env)]
    (stop-service! service))
  env)

(defn handle-message ::service-container
  [_ env msg chan-out]
  (let [destination (:destination msg)]
    (if-let [chan-in (get-in env [:services destination :chan-in])]
      (put! chan-in msg)
      (put! chan-out msg))))
