;; ## A Distributed System
;; ### Processes
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
;; if a process is running or waiting it it unchanged.
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
;; running -> running [label="(p s) != s"]
;; running -> blocked [label="(p s) = s"]
;; running -> terminated [label="(p s) = nil"]
;; blocked -> blocked [label="(e s) = s"]
;; blocked -> waiting [label="(e s) != s"]
;; waiting -> waiting [label="(e s) = s"]
;; waiting -> running
;; }
;; </graph></pre>
;;
;; ### Message Passing
;; In order for processes to communicate with each other
;; and the outside world they need to be pass messages around.
;; We can do this my modelling a process state as the
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
;; We define a systems as a configuration of
;; processes and their states.
;;
;;     C -> ([P1 S1], [P2 S2], ... , [Pn Sn])
;;
;; A configuration can advance in one of the following ways:
;;
;; - A waiting process is marked as running
;; - A running process is executed
;; - A message is taken from an outbound queue and deivered to an
;;   inbound queue
;; - A process with an non empty inbound queue is unblocked
;;
;; Apart from message deivery
;; each of the above steps are independant
;; and therefore can be executed in parallel.
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
;; We can then avance the system by interleaving the
;; sequential and parallel steps:
;;
;;     (prs (sqs C)) -> C'
;;
;; Given this model we can define a execution as
;; the sequence of configurations
;; of a given system
(ns treguard.algo)

;; ## The Process

;; We defined a process as a state `s`
;; and a transition function `p`.
;; We later defined defined the state as
;; a local context,
;; in inbound queue and
;; and outbound queue.

;; More generally a process defines a
;; protocol where we can either
;; run the process,
;; enqueue a message to the inbound queue or
;; dequeue a message from the outbound queue.
(defprotocol IProcess
  (run [p] "Run the process,
            returns the new process state or nil")
  (enqueue [p m] "Enqueue an inbound message,
                  returns the new process state")
  (dequeue [p] "Dequeue and outbound message,
                returns a tuple the new process state and an oubound message (or nil)"))

;; Concretely we have
;; a process that is made up of:
;;
;; - a function to run the process
;; - the local context
;; - the inbound queue
;; - the outbound queue
;;
;; Where the function used takes
;; the local context and a message in (or nil)
;; and returns
;; the new context and a message out (or nil)
;;
;;     (f ctx m-in) -> [ctx' m-out]
;;
(defrecord Proc [f ctx in out]
  IProcess
  (run [p]
    (let [m-in (peek in)]
      (when-let [res (f ctx m-in)]
        (let [[ctx' m-out] res]
          (-> (if m-out (update-in p [:out] conj m-out) p)
              (assoc :ctx ctx')
              (update-in [:in] pop))))))
  (enqueue [p m]
    (update-in p [:in] conj m))
  (dequeue [p]
    (let [m (peek out)
          p' (update-in p [:out] pop)]
      [p' m])))

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

(comment
;;  LocalWords:  STS pre ctx ns treguard algo
)
