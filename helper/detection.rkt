;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname detection) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; 49 × 300
(define pipe (scale 0.2 (bitmap "flappy-pipe.png")))
(define back (bitmap "flappy-background.jpeg"))
(define bird (scale 0.12 (bitmap "bird1.png")))

(define (rand-stack n)
  (let ([r (/ (random 100) 100)])
    (above (rotate 180 pipe)
           (rectangle 40 150 "solid" "transparent")
           (crop 0 0 49 (* (+ r 0.1) 300) pipe))))

(define (place-stack n)
  (place-images/align (list (rand-stack 10)
                            (rand-stack 10))
                     (list (make-posn n 700)
                           (make-posn 250 700))
                     "left"
                     "baseline"
                     back))

(place-stack 400)


; Bottom: 300 + 150 + (* (+ r 0.1) 300)
; 700 - (- (image-height img) 450)
; (- 700 (- (image-height (rand-stack 1)) 450))

; Top: Bottom - 150

; Width of Pipe: 49

; Bird dimensions: 60 x 60
; detection: Posn [ListOf Images] [ListOf Posn] -> Boolean
(define (detection bird-posn lst-stack lst-posn)
  (cond [(empty? lst-stack) #f]
        [(<= 190 (posn-x (first lst-posn)) 310)
         (<= (- 700 (- (image-height (first lst-stack)) 390))
             (posn-y bird-posn)
             (- 700 (- (image-height (first lst-stack)) 300)))]
        [else (detection bird-posn (rest lst-stack) (rest lst-posn))]))

