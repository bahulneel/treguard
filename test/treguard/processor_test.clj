(ns treguard.processor-test
  (:require [treguard.processor :refer :all]
            [midje.sweet :refer :all]))

(unfinished)

(facts "About parsing a process"
       (fact "A process must contain a :process statement"
             (parse-process '[:process [:foo]]) => (contains {:name :foo})
             (parse-process '[:process [:foo] :when true :let [x 1]]) => (contains {:name :foo})
             (parse-process '[:when true]) => (throws IllegalArgumentException
                                                      "Processes must contain a process definition"))
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
         (fact "they must have exactly one argument"
               (parse-process '[:process [:w] :when a]) => map?
               (parse-process '[:process [:w] :when]) => (throws IllegalArgumentException
                                                                 "When statements must have exactly one argumnet")
               (parse-process '[:process [:w] :when a b]) => (throws IllegalArgumentException
                                                                     "When statements must have exactly one argumnet"))
         (fact "they have a cond"
               (-> proc :statements first) => (contains {:cond 'a})
               (-> proc :statements second) => (contains {:cond '(true? a)}))))

(facts "About let statements"
       (let [proc (parse-process '[:process [:l] :let [a b]])]
         (fact "they have an op of let"
               (-> proc :statements first) => (contains {:op :let}))
         (fact "they must have exactly one argument which is a sequence"
               (parse-process '[:process [:w] :let [a b]]) => map?
               (parse-process '[:process [:w] :let]) => (throws IllegalArgumentException
                                                                "Let statements must have exactly one argumnet")
               (parse-process '[:process [:w] :let a b]) => (throws IllegalArgumentException
                                                                    "Let statements must have exactly one argumnet")
               (parse-process '[:process [:w] :let a]) => (throws IllegalArgumentException
                                                                  "A let binding must be a sequence of bindings"))
         (fact "a binding sequence must contain a even number of forms"
               (parse-process '[:process [:w] :let [a]]) => (throws IllegalArgumentException
                                                                    "A binding sequence must contain an even number of forms")
               (parse-process '[:process [:w] :let [a b]]) => map?
               (parse-process '[:process [:w] :let [a b c]]) => (throws IllegalArgumentException
                                                                        "A binding sequence must contain an even number of forms")
               (parse-process '[:process [:w] :let [a b c d]]) => map?)
         (fact "they have bindings"
               (let [proc (parse-process '[:process [:w] :let [a b c d]])]
                 (-> proc :statements first)) => (contains {:bindings '[[a b] [c d]]}))
         (fact "bindings are destructured"
               (let [proc (parse-process '[:process [:w] :let [[a b] c]])]
                 (-> proc :statements first :bindings first second) => 'c
                 (-> proc :statements first :bindings second first) => 'a
                 (-> proc :statements first :bindings (nth 2) first) => 'b))
         (fact "bidings are macroexpanded"
               (let [proc (parse-process '[:process [:w] :let [a (-> b first)]])]
                 (-> proc :statements first :bindings first first) => 'a
                 (-> proc :statements first :bindings first second) => '(first b)))))

(facts "About parsing a call"
       (let [proc (parse-process '[:process [:c] :foo a b c])]
         (fact "they have an op of :call"
               (-> proc :statements first) => (contains {:op :call}))
         (fact "they have a process name of the keyword"
               (-> proc :statements first) => (contains {:process :foo}))
         (fact "they have a sequnce of arguments"
                (-> proc :statements first) => (contains {:args '[a b c]}))))

;; TODO for let and when forms need to try macro expansion and for let bindings
;; need to call out to destructure.

;; TODO deal with edge cases that would slip up partition-by, consider
;; using a state state machine
