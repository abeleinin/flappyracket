;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname sinefly) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define background (bitmap "img/background.jpeg"))
(define bird (scale 0.12 (bitmap "img/bird.png")))

; world is a posn
(define-struct world [tick posn])

(define (sine x)
  (inexact->exact (round (* (sin (* x 0.1)) 3))))

(define (update w)
  (make-world (+ (world-tick w) 3)
              (make-posn (posn-x (world-posn w))
                         (+ (posn-y (world-posn w))
                            (sine (world-tick w))))))

(define (draw w)
  (place-image bird
         (posn-x (world-posn w))
         (posn-y (world-posn w))
         background))

(define initial (make-world 0 (make-posn 250 400)))

(big-bang initial
  (on-tick update)
  (to-draw draw))


  