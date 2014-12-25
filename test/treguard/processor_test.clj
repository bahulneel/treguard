(ns treguard.processor-test
  (:require [treguard.processor :refer :all]
            [midje.sweet :refer :all]))

(unfinished)

(facts "About parsing a process"
       (fact "A process must contain a :process statement"
             (parse-process '[:process [:foo]]) => (contains {:name :foo})
             (parse-process '[:process [:foo] :when true :let [x 1]]) => (contains {:name :foo})
             (parse-process '[:when true]) => (throws IllegalArgumentException "Processes must contain a process definition"))
       (fact "Processes can have arguments"
             (parse-process '[:process [:bar a b c]]) => (contains {:name :bar :args '[a b c]}))
       (fact "Processes may be followed by a sequence of ordered statements"
             (let [proc (parse-process '[:process [:baz a] :when a :let [b a]])]
               proc => (contains {:name :baz :args ['a] :statements vector?})
               (-> proc :statements count) => 2
               (-> proc :statements first) => (contains {:op :when})
               (-> proc :statements second) => (contains {:op :let}))))

(facts "About when statements"
       (let [proc (parse-process '[:process [:w] :when a :when (true? a)])]
         (fact "they have an op of :when"
               (-> proc :statements first) => (contains {:op :when}))
         (fact "They must have exactly one argument"
               (parse-process '[:process [:w] :when a]) => map?
               (parse-process '[:process [:w] :when]) => (throws IllegalArgumentException "When statements must have exactly one argumnet")
               (parse-process '[:process [:w] :when a b]) => (throws IllegalArgumentException "When statements must have exactly one argumnet"))
         (fact "They have a cond"
               (-> proc :statements first) => (contains {:cond 'a})
               (-> proc :statements second) => (contains {:cond '(true? a)}))))
