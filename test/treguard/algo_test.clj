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

(defn broadcast-out-in-1
  [c]
  (let [ps-out (first
                (filter (fn [ps]
                          (let [[pid [p s]] ps
                                [p' m] (dequeue p)]
                            m))
                        (listp c)))]
    (if ps-out
      (let[[pid-out [p-out s-out]] ps-out
           [p-out' mout] (dequeue p-out)
           c (updatep c pid-out p-out')
           c' (reduce (fn [c' [pid [p s]]]
                        (let [p' (enqueue p mout)]
                          (updatep c' pid p')))
                      c
                      (listp c))]
        c')
      c)))

(defn run-proc-1
  [c]
  (let [[pid [p s]] (first
                     (filter (fn [[pid [p s]]]
                               (#{:running :waiting :blocked} s))
                             (listp c)))]
    (if (= :running s)
      (let [p' (run p)]
        (updatep c pid p'))
      (if p
        (updatep c pid p)
        c))))

(defn terminated?
  [s]
  (every? (fn [[pid [p s]]]
            (= :terminated s))
          (ps s)))

(def running? (complement terminated?))

(facts "about a single process systems"
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
             s (system default-configuration
                       broadcast-out-in-1
                       run-proc-1)
             p (enqueue (proc gcd nil) {:x 1071 :y 462})
             [s pid] (exec s p)
             e (take-while running? (iterate step s))
             res (last e)
             [sys [p s]] (kill res pid)]
         (let [{:keys [x y]} (:ctx p)]
             (fact "x = y at result"
                   x => y)
             (fact "result is 21"
                   x => 21))))
