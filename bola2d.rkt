;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname bola2d) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;:::::::::::::::::::::::::::::::::DEFINICION DE CONSTANTES:::::::::::::::::::::::::::::
(define-struct fig (x y))
(define W 600)
(define H 600)
(define r 15)
(define snake (circle r "solid" "red"))
(define fruta (circle r "solid" "green"))
(define FONDO (empty-scene W H))

;:::::::::::::::::::::::::::::::::FUNCIONES DEL MUNDO:::::::::::::::::::::::::::::::
(define (pintar ws)
  (place-image
   (circle r "solid" "red" )
   (fig-x ws) (fig-y ws)
   FONDO))

(define (mover ws kev)
  (cond
    [(key=?  kev "up") 0]
    [(key=?  kev "down") 1]
    [(key=?  kev "left") 2]
    [(key=?  kev "right") 3]
    [else ws]))

(define (movete ws kev)
  (cond
    [(= (mover ws kev) 0) (make-fig (fig-x ws) (- (fig-y ws) r))]
    [(= (mover ws kev) 1) (make-fig (fig-x ws) (+ (fig-y ws) r))]
    [(= (mover ws kev) 3) (make-fig (+ (fig-x ws) r) (fig-y ws))]
    [(= (mover ws kev) 2) (make-fig (- (fig-x ws) r) (fig-y ws))]
    [else ws]))

;(define (tock ws)
 ; (cond
  ;  [()]
 ; (make-fig (+ (fig-x ws) r) (fig-y ws)))

(define (end? ws)
  (cond
    [(>= (fig-y ws) H) true]
    [(<= (fig-y ws) r) true]
    [(>= (fig-x ws) W) true]
    [(<= (fig-x ws) r) true]
    [else false]))

(define (last-scene ws)
  (place-image
   (text "Has muerto" 20 "black")
    540 580
   (pintar ws)))
  
  (big-bang (make-fig 50 350)
          [to-draw pintar]
          [on-key movete]
         ; [on-tick tock 0.1]
          [stop-when end? last-scene]
          )
