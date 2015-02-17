(ns treguard.proc-test
  (:require [treguard.proc :refer :all]
            [treguard.automaton :as a]
            [midje.sweet :refer :all]))

(a/defaction gcd-start :in [x' y']
  (assoc state
    :x x'
    :y y'))

(a/defaction gcd-done :out [x]
  (let [{:keys [y]} state]
    (and (pos? x)
         (pos? y)
         (= x y)))
  (-> state
      (assoc :x 0)
      (assoc :y 0)))

(a/defaction step-x :int [x y]
  (and (pos? x)
       (pos? y)
       (> x y))
  (update-in state [:x] - y))

(a/defaction step-y :int [x y]
  (and (pos? x)
       (pos? y)
       (> y x))
  (update-in state [:y] - x))

(a/defauto gcd
  ([]
     {:x 0 :y 0})
  [gcd-start
   gcd-done
   step-x
   step-y]
  [(fn [{:keys [x y]}]
     (and (>= x 0)
          (>= y 0)))])

(facts "About GCD"
       (let [p0 (proc gcd)]
         (fact "x and y start ar zero"
               (get-in p0 [:state :x]) => 0
               (get-in p0 [:state :y]) => 0)
         (let [p1 (run p0 [:gcd-start 48 180])]
           (fact "We can start the process"
                 (get-in p1 [:state :x]) => 48
                 (get-in p1 [:state :y]) => 180)
           (let [p2 (run p1 nil)]
             (fact "We can step the process"
                   (get-in p2 [:state :x]) => 48
                   (get-in p2 [:state :y]) => 132)
             (let [p3 (run p2 nil)]
               (fact "We can step the process"
                     (get-in p3 [:state :x]) => 48
                     (get-in p3 [:state :y]) => 84)
               (let [p4 (run p3 nil)]
                 (fact "We can step the process"
                       (get-in p4 [:state :x]) => 48
                       (get-in p4 [:state :y]) => 36)
                 (let [p5 (run p4 nil)]
                   (fact "We can step the process"
                         (get-in p5 [:state :x]) => 12
                         (get-in p5 [:state :y]) => 36)
                   (let [p6 (run p5 nil)]
                     (fact "We can step the process"
                           (get-in p6 [:state :x]) => 12
                           (get-in p6 [:state :y]) => 24)
                     (let [p7 (run p6 nil)]
                       (fact "We can step the process"
                             (get-in p7 [:state :x]) => 12
                             (get-in p7 [:state :y]) => 12)
                       (let [p8 (run p7 nil)]
                         (fact "The process terminates"
                               (:out p8) => [:gcd-done 12]
                               (get-in p8 [:state :x]) => 0
                               (get-in p8 [:state :y]) => 0)))))))))))
