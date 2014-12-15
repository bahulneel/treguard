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
