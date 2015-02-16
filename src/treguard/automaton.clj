(ns treguard.automaton
  (:require [clojure.data :as d]))

(declare action? enable task)

(defprotocol IAutomaton
  (-init [self args])
  (-valid? [self state])
  (-tasks [self input state]))

(defrecord Automaton [name constructor actions invarients]
  IAutomaton
  (-init [_ args]
    (apply constructor args))
  (-valid? [_ state]
    (let [failed (->> invarients
                      (map (fn [i] (i state)))
                      (filter true?)
                      count)]
      (zero? failed)))
  (-tasks [_ input state]
    (keep (fn [action]
            (when-let [env (enable action input state)]
              (task action env state)))
          actions)))

(defn automaton
  [{:keys [name constructor actions invarients] :as spec}]
  {:pre [(keyword? name)
         (ifn? constructor)
         (every? action? actions)
         (every? ifn? invarients)]}
  (map->Automaton spec))

(defn automaton?
  [a]
  (satisfies? IAutomaton a))

(defn init
  [a args]
  {:pre [(automaton? a)]
   :post [(map? %)]}
  (-init a args))

(defn valid?
  [a state]
  {:pre [(automaton? a)]}
  (-valid? a state))

(defn tasks
  [a input state]
  {:pre [(automaton? a)]}
  (-tasks a input state))

(defprotocol IAction
  (-enable [self input state])
  (-run [self env state]))

(defrecord Action [type name args pred effect]
  IAction
  (-enable [_ input state]
    (let [env (into {} (map (juxt identity (partial get state)) args))]
      (if pred
        (when-let [env' (pred env state)]
          (merge env env'))
        (when (and input (= :in type))
          (let [[iname & iargs] input
                env' (zipmap args iargs)]
            (when (and (= iname name)
                       (= (count iargs) (count args))
                       (->> env'
                            (d/diff env)
                            first
                            vals
                            (every? nil?)))
              env'))))))
  (-run [_ env state]
    (let [msg (when-not (= :in type)
                (reduce (fn [m arg]
                          (conj m (arg env)))
                        [name]
                        args))
          state' (effect env state)]
      [state' msg])))

(defn action
  [{:keys [type name args pred effect] :as spec}]
  {:pre [(#{:in :out :int} type)
         (keyword? name)
         (every? keyword? args)
         (or (and (= :in type) (nil? pred))
             (and (#{:out :int} type) (ifn? pred)))
         (ifn? effect)]}
  (map->Action spec))

(defn action?
  [a]
  (satisfies? IAction a))

(defn enable
  [action input state]
  {:pre [(action? action)
         (or (nil? input) (sequential? input))
         (map? state)]
   :post [(or (map? %) (nil? %))]}
  (-enable action input state))

(defn run
  [action env state]
  {:pre [(action? action)
         (map? env)
         (map? state)]
   :post [(map? (first %))
          (or (nil? (second %))
              (sequential? (second %)))]}
  (-run action env state))

(defprotocol ITask
  (-exec [self]))

(defrecord Task [action env state]
  ITask
  (-exec [_]
    (run action env state)))

(defn task?
  [task]
  (satisfies? ITask task))

(defn task
  [action env state]
  {:pre [(satisfies? IAction action)
         (map? env)
         (map? state)]}
  (->Task action env state))

(defn exec
  [task]
  {:pre [(task? task)]}
  (-exec task))

(defmacro defaction
  [name type args & body]
  (let [[pred effect] (if (= type :in)
                        [nil (first body)]
                        body)
        spec# {:name (keyword name)
               :type type
               :args (mapv keyword args)
               :pred (when pred `(fn [~'env ~'state]
                                   (let [{:keys ~args} ~'env]
                                     (~@pred))))
               :effect `(fn [~'env ~'state]
                          (let [{:keys ~args} ~'env]
                            (~@effect)))}]
    `(def ~name (action ~spec#))))

(defmacro defauto
  [name cons actions invarients]
  (let [spec# {:name (keyword name)
               :constructor `(fn ~cons)
               :actions actions
               :invarients invarients}]
    `(def ~name (automaton ~spec#))))
