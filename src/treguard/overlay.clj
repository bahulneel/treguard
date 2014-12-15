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

;; ## Peers

;; As overlay networks are most common in P2P (peer-to-peer) networks we
;; will call our nodes peers.
(defrecord Peer [id mail-in mail-out meta peers])

;; Each peer has an id, 2 mailboxes (one for outbound messages and one
;; for incomming massages), some meta-data and a collection of other
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

;; Each message has a from id, a to id, a message id, some meta-data,
;; and the mesage body.
(defn make-message
  [id from to meta body]
  (->Message id from to meta body))

;; ## Delivering a message

;; When a message arrives on mail-out we want to try and deliver it
(def new-message
  '[:process [:new-message peer]
    :let [m [:get (:mail-out peer)]]
    :deliver peer m])

;; In the simplest case arrives which is addressed to youself, in this
;; case we just deliver it to mail-in.
(def local-msg
  '[:process [:deliver peer m]
    :when (`local? peer m)
    :put (:mail-in peer) m])

(defn local?
  [peer m]
  (= (:id m) (:id peer)))

;; The next simplest case is if we recieve a message about a peer we
;; already are connected to.
(def direct-msg
  '[:process [:deliver peer m]
    :let [id (:id m)
          remote? (not (`local? peer m))
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
(def indirect-msg
  '[:process [:deliver peer m]
    :let [id (:id m)
          remote? (not (`local? peer m))
          connected? (`connected? id)]
    :when (and remote? (not connected?))
    :let [remote (select-nearest peer m)]
    :send remote m])

(declare visited? distance)

;; The nearest peer is the closest peer that we have not yet visited.
(defn select-nearest
  [peer m]
  (let [peers (:peers peer)
        dest (get-in peer [:peers (:to m)])]
    (-> peers
        (remove (partial visited? m))
        (sort (partial distance peer dest)))))

(defn visited?
  [m peer]
  (let [visited (get-in m [:meta :visited])
        id (:id peer)]
    (visited id)))

;; For now we have no distance function
(defn distance
  [src dest via]
  0)
