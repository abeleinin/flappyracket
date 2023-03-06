#lang racket
(require 2htdp/image)
(require 2htdp/universe)
(require lang/posn)

(struct world (tick bird vel rot state stack-img stack-psn))

; Images
(define pipe (scale 0.2 (bitmap "img/pipe.png")))
(define background (bitmap "img/background.jpeg"))
(define bird (scale 0.12 (bitmap "img/bird.png")))
(define tube (scale 0.2 (bitmap "img/tube.png")))
(define stack
  (above (rotate 180 tube)
         (rotate 180 pipe)
         (rectangle 40 100 "solid" "transparent")
         pipe))

; Acceleration 
(define ACC 0.5)

; Bird rotation function
(define (max-rot x)
  (if (> x 30)
      30
      x))

(define (update w)
  (cond [(equal? (modulo (world-tick w) 250) 0)
         (world
          (+ (world-tick w) 5)
          (make-posn (posn-x (world-bird w))
                     (+ (posn-y (world-bird w))
                        (+ (world-vel w) ACC)))
          (+ (world-vel w) ACC)
          (- (max-rot (world-rot w)) 2)
          "play"
          (append (world-stack-img w) (list (rand-stack 1)))
          (map (lambda (psn)
                 (make-posn (- (posn-x psn) 5)
                            (posn-y psn)))
               (append (world-stack-psn w) (list (make-posn 500 700)))))]
          [(detection (world-bird w) (world-stack-img w) (world-stack-psn w))
         (world (world-tick w)
                     (make-posn 250 674)
                     (world-vel w)
                     (world-rot w)
                     "pause"
                     (world-stack-img w)
                     (world-stack-psn w))]
        [(> (posn-y (world-bird w)) 674)
         (world (world-tick w)
                     (world-bird w)
                     (world-vel w)
                     (world-rot w)
                     "pause"
                     (world-stack-img w)
                     (world-stack-psn w))]
        [(string=? (world-state w) "play")
         (world
          (+ (world-tick w) 5)
          (make-posn (posn-x (world-bird w))
                     (+ (posn-y (world-bird w))
                        (+ (world-vel w) ACC)))
          (+ (world-vel w) ACC)
          (- (max-rot (world-rot w)) 2)
          "play"
          (world-stack-img w)
          (map (lambda (psn)
                 (make-posn (- (posn-x psn) 5)
                            (posn-y psn)))
               (world-stack-psn w)))]
        [(string=? (world-state w) "pause")
         w]))

(define (draw w)
  (cond [(string=? (world-state w) "play")
         (place-image (text (number->string (- (length (world-stack-img w)) 1)) 45 "white")
                      250 100
                      (place-image (rotate (max-rot (world-rot w)) bird)
                                   (posn-x (world-bird w))
                                   (posn-y (world-bird w))
                                   (place-stack (world-stack-img w)
                                                (world-stack-psn w))))]
        [(string=? (world-state w) "pause")
         (place-image (text (number->string (- (length (world-stack-img w)) 1)) 45 "white")
                      250 100
                      (place-image (rotate 30 (triangle 20 "solid" "black"))
                                   250
                                   400
                                   (place-image (rotate (max-rot (world-rot w)) bird)
                                                (posn-x (world-bird w))
                                                (posn-y (world-bird w))
                                                (place-stack (world-stack-img w)
                                                             (world-stack-psn w)))))]))
                                   
(define (input w ke)
  (cond [(key=? ke " ")
         (world (world-tick w)
                     (make-posn (posn-x (world-bird w))
                                (- (posn-y (world-bird w)) 50))
                     0
                     (+ (world-rot w) 50)
                     "play"
                     (world-stack-img w)
                     (world-stack-psn w))]
        [(key=? ke "k")
         (world (world-tick w)
                     (world-bird w)
                     (world-vel w)
                     (world-rot w)
                     "pause"
                     (world-stack-img w)
                     (world-stack-psn w))]
        [(key=? ke "p")
         (world (world-tick w)
                     (world-bird w)
                     (world-vel w)
                     (world-rot w)
                     "play"
                     (world-stack-img w)
                     (world-stack-psn w))]
        [(key=? ke "r")
         initial]
        [else w]))

;; Detection file
; 49 × 300

(define (rand-stack n)
  (let ([r (/ (random 100) 100)])
    (above (rotate 180 tube)
           (rotate 180 pipe)
           (rectangle 40 150 "solid" "transparent")
           (crop 0 0 49 (* (+ r 0.1) 300) pipe))))

(define (place-stack stacks psn)
  (place-images/align stacks
                      psn
                      "left"
                      "baseline"
                      background))

; Bottom: 300 + 150 + (* (+ r 0.1) 300)
; 700 - (- (image-height img) 450)
; (- 700 (- (image-height (rand-stack 1)) 450))

; Top: Bottom - 150

; Width of Pipe: 49

; Bird dimensions: 60 x 60
; detection: Posn [ListOf Images] [ListOf Posn] -> Boolean
(define (detection bird-posn lst-stack lst-posn)
  (cond [(empty? lst-stack) #f]
        [(<= 190 (posn-x (first lst-posn)) 250)
         (not (<= (- 972 (- (image-height (first lst-stack)) 300))
                  (posn-y bird-posn)
                  (- 972 (- (image-height (first lst-stack)) 430))))]
        [else (detection bird-posn (rest lst-stack) (rest lst-posn))]))

#;
(define (stop w)
  (detection (world-bird w) (world-stack-img w) (world-stack-psn w)))

;; End Detection

(define init-stacks-img empty)
(define init-stacks-psn empty)

(define initial (world 0 (make-posn 250 300) 0 20 "play" init-stacks-img init-stacks-psn))

(big-bang initial
  (on-tick update)
  (to-draw draw)
  (on-key input)
  (name "Flappy Racket"))


