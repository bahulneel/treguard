;; ## Overlay networks
;;
;; An overlay network allows nodes to communicate with each other
;; without knowing about the underlying network or how it's connected.
;;
(ns treguard.overlay)

;; <sequence>
;; Andrew->China: Says Hello
;; Note right of China: China thinks\nabout it
;; China-->Andrew: How are you?
;; Andrew->>China: I am good thanks!
;; </sequence>

;; <flowchart>
;; st=>start: Start
;; e=>end
;; op=>operation: My Operation
;; cond=>condition: Yes or No?
;; sub=>subroutine: My sub
;; io=>inputoutput: Write result
;; st->op->cond
;; cond(yes)->sub->e
;; cond(no,right)->io->op
;; </flowchart>
