;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname flappyIntermediate) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; A State is one of:
; - "initial"
; - "play"
; - "pause"
; - "over"

; A world is a (make-world [Number Number Number State [ListOf Image] [ListOf Posn] Number])
(define-struct world [tick bird vel rot state stack-img stack-psn score])

;;; Constants

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

; Initial State
(define initial (make-world 0 (make-posn 250 400) 0 0 "initial" empty empty -1))

; Acceleration Constant
(define ACC 0.5)

;;; Helper functions

; max-rot : Number -> Number
; Bird sprite rotation maximum function
(define (max-rot x)
  (cond [(> x 30) 30]
        [else x]))

; sine : Number -> Number
; Bird hover function: 3 * sin(0.1x)
(define (sine x)
  (inexact->exact (round (* (sin (* x 0.1)) 3))))

; random-stack : Number -> Image
; Generate a random image stack if different pipe separation
(define (random-stack n)
  (let ([r (/ (random 100) 100)])
    (above (rotate 180 tube)
           (rotate 180 pipe)
           (rectangle 40 150 "solid" "transparent")
           (crop 0 0 49 (* (+ r 0.1) 300) pipe))))

; place-stack : [ListOf Image] [ListOf Posn] -> Image
; Place stacks at given position
(define (place-stack stacks psn)
  (place-images/align stacks
                      psn
                      "left"
                      "baseline"
                      background))

; detection: Posn [ListOf Images] [ListOf Posn] -> Boolean
; Detect if bird hits stack
(define (detection bird-posn lst-stack lst-posn)
  (cond [(empty? lst-stack) #f]
        [(<= 190 (posn-x (first lst-posn)) 250)
         (not (<= (- 972 (- (image-height (first lst-stack)) 300))
                  (posn-y bird-posn)
                  (- 972 (- (image-height (first lst-stack)) 430))))]
        [else (detection bird-posn (rest lst-stack) (rest lst-posn))]))

;;; Big-Bang Functions

; update : World -> World
; on-tick function for big-bang
(define (update w)
  (cond [(string=? (world-state w) "initial")
         (make-world
          (+ (world-tick w) 5)
          (make-posn (posn-x (world-bird w))
                     (+ (posn-y (world-bird w))
                        (sine (world-tick w))))
          (world-vel w)
          (world-rot w)
          "initial"
          (world-stack-img w)
          (world-stack-psn w)
          (world-score w))]
        [(equal? (modulo (world-tick w) 250) 0)
         (make-world
          (+ (world-tick w) 5)
          (make-posn (posn-x (world-bird w))
                     (+ (posn-y (world-bird w))
                        (+ (world-vel w) ACC)))
          (+ (world-vel w) ACC)
          (- (max-rot (world-rot w)) 2)
          "play"
          (append (world-stack-img w) (list (random-stack 1)))
          (map (lambda (psn)
                 (make-posn (- (posn-x psn) 5)
                            (posn-y psn)))
               (append (world-stack-psn w) (list (make-posn 500 700))))
          (add1 (world-score w)))]
        [(detection (world-bird w) (world-stack-img w) (world-stack-psn w))
         (make-world (world-tick w)
                     (world-bird w)
                     (world-vel w)
                     (world-rot w)
                     "over"
                     (world-stack-img w)
                     (world-stack-psn w)
                     (world-score w))]
        [(> (posn-y (world-bird w)) 674)
         (make-world (world-tick w)
                     (world-bird w)
                     (world-vel w)
                     (world-rot w)
                     "over"
                     (world-stack-img w)
                     (world-stack-psn w)
                     (world-score w))]
        [(string=? (world-state w) "play")
         (make-world
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
               (world-stack-psn w))
          (world-score w))]
        [(string=? (world-state w) "pause")
         w]))
          
; draw : World -> Image         
; on-draw function for big-bang
(define (draw w)
  (cond [(string=? (world-state w) "play")
         (place-image (text (number->string (if (< (world-score w) 0) 0 (world-score w))) 45 "white")
                      250 100
                      (place-image (rotate (max-rot (world-rot w)) bird)
                                   (posn-x (world-bird w))
                                   (posn-y (world-bird w))
                                   (place-stack (world-stack-img w)
                                                (world-stack-psn w))))]
        [(string=? (world-state w) "pause")
         (place-image (text (number->string (world-score w)) 45 "white")
                      250 100
                      (place-image (rotate 30 (triangle 20 "solid" "black"))
                                   250
                                   400
                                   (place-image (rotate (max-rot (world-rot w)) bird)
                                                (posn-x (world-bird w))
                                                (posn-y (world-bird w))
                                                (place-stack (world-stack-img w)
                                                             (world-stack-psn w)))))]
        [(string=? (world-state w) "over")
         (place-image (overlay (text (number->string (world-score w)) 45 "white")
                               (place-image (overlay (text "Play again" 15 "white")
                                                     (rectangle 100 40 "solid" "black"))
                                            100 160
                                            (square 200 "solid" (color 222 217 145 255))))
                      250
                      400
                      (place-image (rotate (max-rot (world-rot w)) bird)
                                   (posn-x (world-bird w))
                                   (posn-y (world-bird w))
                                   (place-stack (world-stack-img w)
                                                (world-stack-psn w))))]
        [(string=? (world-state w) "initial")
         (place-image (rotate (max-rot (world-rot w)) bird)
                      (posn-x (world-bird w))
                      (posn-y (world-bird w))
                      (place-stack (world-stack-img w)
                                   (world-stack-psn w)))]))

; input : World KeyEvent -> World
; on-key function for big-bang
(define (input w ke)
  (cond [(key=? ke " ")
         (cond [(string=? (world-state w) "over") w]
               [else
                (make-world (world-tick w)
                            (make-posn (posn-x (world-bird w))
                                       (- (posn-y (world-bird w)) 50))
                            0
                            (+ (world-rot w) 50)
                            "play"
                            (world-stack-img w)
                            (world-stack-psn w)
                            (world-score w))])]
        [(key=? ke "k")
         (make-world (world-tick w)
                     (world-bird w)
                     (world-vel w)
                     (world-rot w)
                     "pause"
                     (world-stack-img w)
                     (world-stack-psn w)
                     (world-score w))]
        [(key=? ke "p")
         (make-world (world-tick w)
                     (world-bird w)
                     (world-vel w)
                     (world-rot w)
                     "play"
                     (world-stack-img w)
                     (world-stack-psn w)
                     (world-score w))]
        [(key=? ke "r")
         initial]
        [else w]))

; mouse : World MouseX MouseY MouseEvent -> World
; on-mouse function for big-bang
(define (mouse w x y me)
  (cond [(string=? (world-state w) "over")
         (cond [(and (mouse=? me "button-down")
                     (and (<= 200 x 300)
                          (<= 440 y 480)))
                initial]
               [else w])]
        [else w]))



; Big-Bang call
(big-bang initial
  (on-tick update)
  (to-draw draw)
  (on-key input)
  (on-mouse mouse)
  (name "Flappy Racket"))

