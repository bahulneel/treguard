;; ## Overlay networks
;;
;; An overlay network allows nodes to communicate with each other
;; without knowing about the underlying network or how it's connected.
;;
;; For example, suppose we have nodes A, B, C and D. Where A is
;; connected to B and B is connected to both C and D.
;;
;; <graph>
;; graph {
;; A -- B;
;; B -- {C D};
;; }
;; </graph>
;;
;; If A want's to send a message to C or D then it may pass that message
;; to B which will then forward that message to the correct recipient.
;;
;; Similarly if C wants to send a message to either A or D it can also
;; pass the message to B who will pass it to the correct recipient.
;;
;; We can apply this concept to an arbitrarily complex network and can
;; guarantee that the message will be delivered to the correct recipient
;; providing that a valid path exists.
;;
;; The obvious problem with this is that, for a complex network, we
;; can't guarantee that the quickest path is taken. We can solve this by
;; laying out the nodes on a coordinate system and using this to pass
;; messages to nodes that we think are closest to our target node. To do
;; this we assign [Vivaldi
;; Coordinates](http://en.wikipedia.org/wiki/Vivaldi_coordinates) to
;; each node and pass the coordinates around in our messages.
(ns treguard.overlay)

(def system (atom []))

(defn add-process
  [system process]
  (swap! system conj process))

;; ## Peers

;; As overlay networks are most common in P2P (peer-to-peer) networks we
;; will call our nodes peers.
(defrecord Peer [id mail-in mail-out meta peers])

;; <graph>
;; digraph {
;; node [shape=record];
;; peer [ label = "&lt;p&gt; peer|{id|mail-in|mail-out|&lt;m&gt; meta|&lt;ps&gt; peers}" ];
;; peers [ label = "&lt;ps&gt; peers|{id1|&lt;p1&gt; peer1}|{id2|&lt;p2&gt; peer2}|..." ];
;; meta [ label = "&lt;m&gt; meta|{k1|v1}|{k2|v2}|..." ];
;; peer:m -> meta:m;
;; peer:ps -> peers:ps;
;; }
;; </graph>
;;
;; Each peer has an id, 2 mailboxes (one for outbound messages and one
;; for incoming massages), some meta-data and a collection of other
;; peers it knows about.
(defn make-peer
  ([id mail-in mail-out]
     (make-peer id mail-in mail-out {}))
  ([id mail-in mail-out meta]
     (make-peer id mail-in mail-out meta {}))
  ([id mail-in mail-out meta peers]
     (->Peer id mail-in mail-out meta peers)))

;; ## Messages

;; A message is something we want to be delivered to a peer.
(defrecord Message [id from to meta body])

;; <graph>
;; digraph {
;; node [shape=record];
;; message [ label = "message|{id|from|to|&lt;m&gt; meta|&lt;b&gt; body}" ];
;; body [ label = "&lt;b&gt; body|..." ];
;; meta [ label = "&lt;m&gt; meta|{k1|v1}|{k2|v2}|..." ];
;; message:m -> meta:m;
;; message:b -> body:b;
;; }
;; </graph>
;;
;; Each message has a message id, from address, to address, some meta-data,
;; and the message body.
(defn make-message
  [id from to meta body]
  (->Message id from to meta body))

;; ## The process
;; <graph>
;; digraph {
;; "new-message" [label="new-message(peer)"];
;; route [label="route(peer,m)"];
;; send [label="send(peer,remote,m)"];
;; "wait-ack" [label="wait-ack(peer,m)"];
;; "ack-msg" [label="ack-msg(peer,m)"];
;; "reject-msg" [label="reject-msg(peer,m)"];
;; deliver [label="deliver(peer,m)"];
;; "wait-deliver-ack" [label="wait-deliver-ack(peer,m,timeout)"];
;; put [label="put(peer,m)"];
;; get [label="get(peer)"];
;; fail [label=fail];
;; "new-message" -> route [label=""];
;; "wait-ack" -> route [label="(reject? m)"];
;; route -> send [label="(and remote? connected?)"];
;; route -> send [label="(and remote? (not connected?))"];
;; send -> "wait-ack" [label=remote];
;; "wait-ack" -> "ack-msg" [label="(ack? m)"];
;; send -> "reject-msg" [label="(not remote)"];
;; route -> deliver [label="local?"];
;; send -> deliver [label=remote];
;; "ack-msg" -> deliver [label=""];
;; "reject-msg" -> deliver [label=""];
;; "wait-deliver-ack" -> deliver [label="(and timeout attempts)"];
;; deliver -> "wait-deliver-ack" [label="remote?"];
;; deliver -> put [label="local?"];
;; deliver -> put [label="remote?"];
;; "wait-deliver-ack" -> fail [label="(and timeout (zero? attempts))"];
;; }
;; </graph>

;; ## Routing a message

;; When a message arrives on mail-out we want to try and route it.
(add-process system
             '[:process [:new-message peer]
               :let [m [:get (:mail-out peer)]]
               :route peer m])

;; In the simplest case a message arrives which is addressed to youself,
;; in this case we just deliver it to ourselves.
(add-process system
             '[:process [:route peer m]
               :let [local? (`local? peer m)]
               :when local?
               :deliver peer m])

(defn local?
  [peer m]
  (= (:to m) (:id peer)))

(def remote? (complement local?))

;; The next simplest case is if we receive a message about a peer we
;; already are connected to. In this case we send it to the remote peer
;; fir delivery.
(add-process system
             '[:process [:route peer m]
               :let [id (:to m)
                     remote? (`remote? peer m)
                     connected? (`connected? id)]
               :when (and remote? connected?)
               :let [remote (get-in peer [:peers id])]
               :send remote m])

(defn connected?
  [peer id]
  (get-in peer [:peers id :meta :connected?]))

;; The more complicated case is that we are not able to route to a
;; connected peer. In this case we must select one and try to send it to
;; that.
(add-process system
             '[:process [:route peer m]
               :let [id (:to m)
                     remote? (not (`local? peer m))
                     connected? (`connected? id)]
               :when (and remote? (not connected?))
               :let [remote (select-nearest peer m)]
               :send peer remote m])

(declare visited? distance)

;; The nearest peer is the closest connected peer that we have not yet visited.
(defn select-nearest
  [peer m]
  (let [peers (:peers peer)
        dest (get-in peer [:peers (:to m)])]
    (-> peers
        vals
        (filter #(get-in % [:meta :connected?]))
        (remove (partial visited? m))
        (sort (partial distance peer dest))
        first)))

(defn visited?
  [m peer]
  (let [visited (get-in m [:meta :visited])
        id (:id peer)]
    (visited id)))

;; For now we have no distance function
(defn distance
  [src dest via]
  0)

;; ## Sending a message

;; Now that we have decided who to send out message to we now need to
;; pass it to the peer.

;; If for some reason we could not find a peer to send the message to we
;; need to reject it as it's not routeable from our point of view.
(add-process system
             '[:process [:send peer remote m]
               :when (not remote)
               :reject-msg peer m])

;; Otherwise we deliver the message to the remote peer, we also need to
;; note that the message visited this peer, then we wait for the message
;; to be acknowledged.
(add-process system
             '[:process [:send peer remote m]
               :when remote
               :let [m (-> m
                           (reply-to peer)
                           (via peer)
                           (state :in-flight))]
               :deliver remote m
               :wait-ack peer m])

(defn reply-to
  [m peer]
  (update-in m [:meta :reply-to] (:id peer)))

(defn via
  [m peer]
  (update-in m [:meta :via] (fnil conj #{}) (:id peer)))

(defn state
  [m state]
  (assoc-in m [:meta :state] state))

;; When a message is acknowledged we can acknowledge the sender
(add-process system
             '[:process [:wait-ack peer m]
               :when (ack? m)
               :ack-msg peer m])

(defn ack?
  [m]
  (= :ack (get-in m [:meta :state])))

;; When a message is rejected we need to try to re-route it to someone
;; else.
(add-process system
             '[:process [:wait-ack peer m]
               :when (reject? m)
               :route peer m])

(defn reject?
  [m]
  (= :reject (get-in m [:meta :state])))

;; Acking a message just involves sending a message to the reply-to
;; address with the same message id as original message and `:ack` in
;; the body
(add-process system
             '[:process [:ack-msg peer m]
               :let [reply-to (:reply-to m)
                     msg-id (:id msg)
                     peer-id (:id peer)
                     msg (make-message msg-id peer-id reply-to {} :ack)
                     remote (get-in peer [:peers peer-id])]
               :deliver mail-in msg])

;; Rejecting a message is the same as an ack but with a body of
;; `:reject` and the via set from the rejected message.
(add-process system
             '[:process [:reject-msg peer m]
               :let [reply-to (:reply-to m)
                     msg-id (:id msg)
                     peer-id (:id peer)
                     meta (select-keys (:meta m) [:via])
                     msg (make-message msg-id peer-id reply-to meta :reject)
                     remote (get-in peer [:peers peer-id])]
               :deliver remote msg])

;; ## Delivering messages

;; Message delivery is the act of passing messages between peers this
;; is distinct from sending a message as we only need to gaurentee that
;; the message arrived at the peer.

;; Again we have the simple case where we wish to deliver the message to
;; ourselves. In this case we just put the message on our incomming
;; mailbox.
(add-process system
             '[:process [:deliver peer m]
               :let [local? (`local? peer m)]
               :when local?
               :let [mail-in (:mail-in peer)]
               :put mail-in m])

(defn set-meta
  ([m k v]
     (assoc-in m [:meta k] v)))

(defn get-meta
  [m k]
  (get-in m [:meta k]))

;; Alternatively we need to pass it to a remote peer. And wait for it to
;; be acknowledged.
(add-process system
             '[:process [:deliver peer m]
               :let [remote? (`remote? peer m)]
               :when remote?
               :let [mail-in (:mail-in peer)
                     ttl 5000           ; FIXME: make this configurable
                     attempts 5
                     m (-> m
                           (set-meta :timeout ttl)
                           (set-meta :attempts attempts))
                     timeout [:timeout ttl]]
               :put mail-in m
               :wait-deliver-ack peer m timeout])

;; If a message is ack'd we can cancel the timeout and drop the message
(add-process system
             '[:process [:wait-deliver-ack peer m timeout]
               :when (ack? m)
               :cancel-timeout timeout])

;; If we timeout we can retry the message
(add-process system
             '[:process [:wait-deliver-ack peer m timeout]
               :let [attempts (get-meta m :attempts)]
               :when (and timeout attempts)
               :let [m (set-meta m :attempts (dec attempts))]
               :deliver peer m])

;; Otherwise we fail
(add-process system
             '[:process [:wait-deliver-ack peer m timeout]
               :let [attempts (get-meta m :attempts)]
               :when (and timeout (zero? attempts))
               :fail {:err :timeout :message m :peer peer}])
