;; ## A Distributed System
;;
;; ### Processes
;;
;; In a distributed system
;; we can define a process
;; as a series of
;; state transitions.
;; These transitions form a
;; State Transition System (STS).
;;
;; An STS is similar to a
;; state machine except that it can have
;; an infinite number of states
;;
;; We can define such a system as
;;
;;     (p s) -> s'
;;
;; Where:
;;
;;     p  -> some process
;;     s  -> initial state
;;     s' -> next state
;;
;; We can then define the
;; running state of a process
;; as the following:
;;
;;     if s != s'  -> process is running
;;     if s  = s   -> process is blocked
;;     if s  = nil -> process has terminated
;;
;; Once a process is blocked we need to
;; wait for some external input
;; from the system.
;; These take the form of events
;; which can put the process into
;; some new state.
;;
;;     (e s) -> s'
;;
;; Where:
;;
;;     e  -> some event
;;     s  -> initial state
;;     s' -> next state
;;
;; And the states for this are:
;;
;;     if s  = s'  -> process is unchanged
;;     if s != s'  -> process is no longer blocked
;;
;; In the latter transition
;; if a process is blocked it is then waiting or
;; if a process is running or waiting it is unchanged.
;;
;;
;; If a process is waiting then we can put it into the
;; running state at any time.
;; This is up to a scheduler,
;; the default of which is to make
;; all waiting processes running.
;;
;; This gives us the following state machine for a process
;;
;; <pre><graph>
;; digraph {
;; start [shape="point"]
;; running -> running [label="(p s) != s"]
;; running -> blocked [label="(p s) = s"]
;; running -> terminated [label="(p s) = nil"]
;; blocked -> blocked [label="(e s) = s"]
;; blocked -> waiting [label="(e s) != s"]
;; waiting -> waiting [label="(e s) = s"]
;; waiting -> running
;; start -> waiting
;; }
;; </graph></pre>
;;
;; ### Message Passing
;; In order for processes to communicate with each other
;; and the outside world they need to pass messages around.
;; We can do this by modelling a process state as the
;; conjunction of
;; local state,
;; in-bound messages
;; and outbound messages
;;
;;     S -> (ctx, in, out)
;;
;; Where
;;
;;     S   -> the state
;;     ctx -> the local state (or context)
;;     in  -> a queue of inbound messages
;;     out -> a queue of outbound messages
;;
;; A process advances either by
;; modifying it's context (as above)
;; taking a message from the inbound queue
;; or adding a message to the outbound queue.
;;
;; ### Systems
;; A system is a collection of processes
;; that collaborate to provide
;; some service or services.
;; At any one point in time we can define
;; the state of a system as a configuration.
;; This a collection of
;; processes,
;; their states and
;; their execution states
;;
;;     C -> ([P1 S1 E1], [P2 S2 E2], ... , [Pn Sn En])
;;
;; A configuration can advance in one of the following ways:
;;
;; - A waiting process is marked as running
;; - A running process is executed
;; - A message is taken from an outbound queue and delivered to an
;;   inbound queue
;; - A process with an non empty inbound queue is unblocked
;;
;; Apart from message delivery
;; each of the above steps are independent
;; and so can be executed in parallel.
;; We can therefore divide the system into two steps
;; the parallel step and the sequential step:
;;
;;     (sqs C) -> C'
;;     (prs C) -> C'
;;
;; Where:
;;
;;     C   -> a configuration
;;     C'  -> a new configuration
;;     sqs -> the sequential step
;;     prs -> the parallel step
;;
;; We can then advance the system by interleaving the
;; sequential and parallel steps:
;;
;;     (prs (sqs C))  -> C'
;;     (prs (sqs C')) -> C''
;;
;; Given this model we can define a execution as
;; the (possibly infinite) sequence of configurations
;; of a given system.

(ns treguard.algo
  (:require [clojure.core.match :refer [match]]))

;; ## The Process

;; We previously defined a process as
;; a state `s`
;; and a transition function `p`.
;; We further defined the state as
;; a local context,
;; an inbound queue and
;; an outbound queue.

;; We can define a general process as a
;; protocol where we can either:
;;
;; - run the process
;; - enqueue a message to the inbound queue
;; - dequeue a message from the outbound queue
;; - get the size of the queues
(defprotocol IProcess
  (run [p] "Run the process,
            returns the new process state or nil")
  (enqueue [p m] "Enqueue an inbound message,
                  returns the new process state")
  (dequeue [p] "Dequeue and outbound message,
                returns a tuple the new process state and an oubound message (or nil)")
  (pending [p] "Returns a tuple of the number of waiting in and out messages"))

;; More concretely we have
;; a process that is made up of:
;;
;; - a transition function
;; - the local context
;; - the inbound queue
;; - the outbound queue
;;
;; Where the transition function takes
;; the local context and
;; a message in
;; and returns
;; the new context and
;; a message out
;; or nil if the process has terminated.
;;
;;     (f ctx m-in) -> [ctx' m-out]  Normal execution
;;     (f ctx m-in) -> nil           Termination
;;
;; If we don't have a message (in or out) we
;; use nil.
(defrecord Proc [f ctx in out]
  IProcess
  (run [p]
    (let [m-in (peek in)]
      (when-let [res (f ctx m-in)]
        (let [[ctx' m-out] res]
          (-> (if m-out
                (update-in p [:out] conj m-out)
                p)
              (assoc :ctx ctx')
              (update-in [:in] pop))))))
  (enqueue [p m]
    (update-in p [:in] conj m))
  (dequeue [p]
    (let [m (peek out)
          p' (update-in p [:out] pop)]
      [p' m]))
  (pending [p]
    [(count in) (count out)]))

(defn proc
  ([f ctx]
     (let [in clojure.lang.PersistentQueue/EMPTY
           out clojure.lang.PersistentQueue/EMPTY]
       (proc f ctx in out)))
  ([f ctx in out]
     {:pre [(ifn? f)
            (coll? in)
            (coll? out)]}
     (->Proc f ctx in out)))

;; ## The System

;; We earlier defined a system as
;; existing in some configuration `C`
;; which is the collection of the state
;; of all processes in that systems.

;; We can generailse this to a protocol
;; that alows us to:
;;
;; - add a process (exec)
;; - remove a process (kill)
;; - list processes (ps)
;; - run the system 1 step
(defprotocol ISystem
  (exec [s p] "Execute a process, returns the new configuration and a
             process id")
  (kill [s pid] "Kill a process, returns the new configuration and the
              killed process")
  (ps [s] "List all processes")
  (step [s] "Run the system one stepm, returns the new confiuration"))

;; We can also generalise the configuration
;; to a protocol that alows us to:
;;
;; - add a process
;; - remove a process
;; - get a process with it's state
;; - update a process
;; - list all processes
(defprotocol IConfig
  (addp [c pid p] "Add a process")
  (removep [c pid] "Removes a process")
  (getp [c pid] "Returns a tuple of a process and it's state")
  (updatep [c pid p'] "Updates a process")
  (listp [c] "Lists all processes"))

;; Concretely, a system is made up of
;;
;; - a configuration
;; - a sequential step function
;; - a parallel step function
(defrecord Sys [c sqs prs]
  ISystem
  (exec [s p]
    (let [pid (java.util.UUID/randomUUID)
          s' (update-in s [:c] addp pid p)]
      [s' pid]))
  (kill [s pid]
    (let [ps (getp c pid)
          s' (update-in s [:c] removep pid)]
      [s' ps]))
  (ps [s]
    (listp c))
  (step [s]
    (let [c' (-> c sqs prs)]
      (assoc s :c c'))))

;; And concretely a configuration is made up of
;;
;; - a process map
;; - a state transition fn
(defrecord Conf [m state]
  IConfig
  (addp [c pid p]
    (let [s (state nil nil p)
          p-s [p s]]
      (update-in c [:m] assoc pid p-s)))
  (removep [c pid]
    (update-in c [:m] dissoc pid))
  (getp [c pid]
    (get m pid))
  (updatep [c pid p']
    (let [[p s] (get m pid)
          s' (state s p p')
          p-s' [(or p' p) s']]
      (update-in c [:m] assoc pid p-s')))
  (listp [_]
    m))

;; We can also define the standard
;; process state transition function
(defn process-state
  [s p p']
  (let [[min mout] (if p (pending p) [0 0])
        [min' mout'] (if p' (pending p') [0 0])]
    (match [s   [p min mout] [p' min' mout']      ]
           ;; s = nil -> waiting
           [nil _            _                    ] :waiting

           ;; outbound messages -> waiting
           [_   _            [_ _ (m :guard pos?)]] :waiting

           ;; s = waiting -> running
           [:waiting _       _                    ] :running
           ;; s = running /\ p' = nil -> treminated

           [:running _       [nil _ _]            ] :terminated

           ;; s = running /\ p != p' -> running
           ;; s = running /\ p == p' -> blocked
           [:running [p _ _] [p' _ _]             ] (if (= p p') :blocked :running)

           ;; s = blocked /| inbound messages -> waiting
           [:blocked _       [_ (m :guard pos?) _]] :waiting

           ;; state unchanged
           :else s)))

(comment
;;  LocalWords:  STS pre ctx ns treguard algo Pn
)
