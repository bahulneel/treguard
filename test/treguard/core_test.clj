(ns treguard.core-test
  (:require [clojure.test :refer :all]
            [treguard.core :refer :all]
            [clojure.core.async :as async]))

(defmethod handle-start ::echo [_ env _] env)

(defmethod handle-stop ::echo [_ env] env)

(defmethod handle-message ::echo [_ env msg chan-out]
  (let [{:keys [reply-to msg]} (:body msg)
        reply (make-msg nil reply-to {:msg msg})]
    (async/>!! chan-out reply)
    env))

(deftest echo-service-test
  (testing "That the service handles messages"
    (let [chan-in (async/chan)
          chan-out (async/chan 1)
          service (make-service ::echo nil chan-in chan-out)
          msg (make-msg nil nil {:reply-to :me :msg "Hello World!"})
          service (start-service! service)]
      (async/>!! chan-in msg)
      (stop-service! service)
      (let [{:keys [destination body]} (async/<!! chan-out)]
        (is (= destination :me))
        (is (= "Hello World!" (:msg body)))))))

(deftest service-container-test
  (testing "That messages are passed along"
    (let [chan-in (async/chan)
          chan-out (async/chan 1)
          echo-service (make-service ::echo nil (async/chan) (async/chan))
          msg (make-msg nil ::echo {:reply-to :me :msg "Hello World!"})
          service (-> (service-container ::id {} chan-in chan-out)
                      (add-service echo-service)
                      start-service!)]
      (async/>!! chan-in msg)
      (stop-service! service)
      (let [{:keys [destination body]} (async/<!! chan-out)]
        (is (= destination :me))
        (is (= "Hello World!" (:msg body)))))))
