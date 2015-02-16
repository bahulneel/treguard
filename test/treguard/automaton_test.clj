(ns treguard.automaton-test
  (:require [treguard.automaton :refer :all]
            [midje.sweet :refer :all]))

(defaction send-m :in [:m]
  (fn [{:keys [m]} state]
    (update-in state [:queue] conj m)))

(defaction recieve-m :out [:m]
  (fn [{:keys [queue]} args]
    (when-let [m (peek queue)]
      [{:m m}]))
  (fn [{:keys [m]} state]
    (update-in state [:queue] pop)))

(defauto channel []
  {:queue clojure.lang.PersistentQueue/EMPTY}
  [send-m recieve-m]
  [])

(facts "About a channel"
       (fact "A channel is an automaton"
             (automaton? channel) => true)
       (let [state (init channel [])]
         (fact "We can initialise a channel"
               state => map?)
         (fact "A new channel is valid"
               (valid? channel state) => true)
         (fact "A new channel has no enabled tasks"
               (tasks channel nil state) => empty?)
         (let [ts (tasks channel [:send-m 0] state)]
           (fact "An input message enables the send input task"
                 (count ts) => 1
                 (-> ts first :action :name) => :send-m
                 (-> ts first :action :type) => :in)
           (let [t (first ts)
                 [state' m] (exec t)]
             (fact "Executing the task updates the state"
                   (first (:queue state')) => 0)
             (fact "The new state is valid"
                   (valid? channel state') => true)
             (fact "Executing the task returns no message"
                   m => nil?)
             (let [ts (tasks channel nil state')]
               (fact "The next active task is the recieve action"
                     (count ts) => 1
                     (-> ts first :action :name) => :recieve-m
                     (-> ts first :action :type) => :out)
               (let [t (first ts)
                     [state'' m] (exec t)]
                 (fact "Executing the task results in an empty queue"
                       (peek (:queue state'')) => nil?)
                 (fact "The new state is valid"
                   (valid? channel state'') => true)
                 (fact "Executing the task returns the recieve message"
                       m => [:recieve-m 0])))
             (let [ts (tasks channel [:send-m 1] state')]
               (fact "Sending another message enables 2 tasks"
                     (count ts) => 2
                     (map #(get-in % [:action :name]) ts) => (contains [:send-m :recieve-m] :in-any-order)
                     (map #(get-in % [:action :type]) ts) => (contains [:in :out] :in-any-order)))))))
