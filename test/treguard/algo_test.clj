(ns treguard.algo-test
  (:require [treguard.algo :refer :all]
            [midje.sweet :refer :all]))

(facts "about state gcd"
       (let [gcd (fn [ctx in]
                   (let [{:keys [x y]} ctx]
                     (if (= x y)
                       nil
                       (let [ctx (if (> x y)
                                   (assoc ctx :x (- x y))
                                   (assoc ctx :y (- y x)))]
                         [ctx nil]))))
             p (proc gcd {:x 1071 :y 462})
             exec (take-while (complement nil?) (iterate run p))
             res (last exec)]
         (doseq [p' exec]
           (fact "x and y are positive"
                 (-> p' :ctx :x) => pos?)
           (let [{:keys [x y]} (:ctx res)]
             (fact "x = y at result"
                   x => y)
             (fact "result is 21"
                   x => 21)))))

(facts "about message gcd"
       (let [gcd (fn [ctx in]
                   (let [{:keys [x y]} in]
                     (if-not in
                       nil
                       (if (= x y)
                         [in nil]
                         (let [out (if (> x y)
                                     (assoc in :x (- x y))
                                     (assoc in :y (- y x)))]
                           [in out])))))
             run-lo (fn [p]
                      (when-let [p (run p)]
                        (let [[p m] (dequeue p)]
                          (if m
                            (enqueue p m)
                            p))))
             p (enqueue (proc gcd nil) {:x 1071 :y 462})
             exec (take-while (complement nil?) (iterate run-lo p))
             res (last exec)]
         (doseq [p' (rest exec)]
           (fact "x and y are positive"
                 (-> p' :ctx :x) => pos?)
           (let [{:keys [x y]} (:ctx res)]
             (fact "x = y at result"
                   x => y)
             (fact "result is 21"
                   x => 21)))))
