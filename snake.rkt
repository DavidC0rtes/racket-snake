;;Snake
(require 2htdp/universe)
(require 2htdp/image)

;;::::::::::::::::::DEFINICION DE ESTRUCTURAS::::::::::::::::::::::::::::::::::::
;world es una estructura,representa el estado del mundo, compuesta por otras estructuras; snake y fruta
(define-struct world (snake fruta))
;fruta es una estructura. Pos representa la celda que esta ocupando dentro del canvas posn(x,y)
(define-struct fruta (pos))
;snake es una estructura. dir es un string, representa la direccion de un segmento del snake.
;pos es la celda que ocupa en coordenadas posn(x,y)
(define-struct snake (segs dir))


;::::::::::::::::::::::::DEFINICION DE CONSTANTES:::::::::::::::::::::::::::::::::::

(define CELDA 15)
(define N-FILAS 35)
(define N-COLUMNAS 35)
(define TICK 0.07)

(define ANCHO (* N-COLUMNAS CELDA))
(define LARGO (* N-FILAS CELDA))

(define FONDO (empty-scene ANCHO LARGO "black"))

(define BODY (rectangle (/ CELDA 2) (/ CELDA 2) "solid" "white" ))
(define APPLE (rectangle (/ CELDA 2) (/ CELDA 2) "solid" "green"))

(define WORLD-0
  (make-world (make-snake (list (make-posn 2 6) ) "right")
  (make-posn 1 15)))
  
;::::::::::::::::::::::::::::::::::::::::VARIABLES DE TESTEO:::::::::::::::::::::::::::::::::::::::::::::::::::
(define food1 (make-posn 2 5))
(define segs1 (list (make-posn 2 6))) ; one-segment snake
(define segs2 (list (make-posn 2 5) (make-posn 3 5)))
(define segs3 (list (make-posn 2 5) (make-posn 3 5) (make-posn 3 1)))
(define snake1 (make-snake segs1 'up))
(define snake2 (make-snake segs2 'up))
(define snake3 (make-snake segs3 'up))
(define world1 (make-world snake1 food1))
(define world2 (make-world snake2 food1)) ; eating



;:::::::::::::::::::::::::::::::::::::::::FUNCIONES DEL MUNDO::::::::::::::::::::::::::::::::::::::::::::::::::::
;;FUNCIONES PARA RENDERIZAR

;Contrato: render: world -> image
;Propósito: Renderizar el estado del mundo
(define (render w)
  (snake+img (world-snake w)
             (food+img (world-fruta w)
                       FONDO)))

;Contrato: imagen-en-celda: imagen numero numero imagen -> imagen
;Proposito:  dibuja imagen1 en el centro de una celda (x,y) dada en la imagen2
(define (imagen-en-celda img1 celda-x celda-y img2)
  (place-image
   img1
   (* CELDA (+ celda-x 0.5))
   (* CELDA (- N-FILAS (+ celda-y 0.5)))
   img2))

;Contrato:snake+img: snake image -> image. Donde snake es una estructura
;Propósito: Dibujar el snake en el canvas
;Ejemplo:
(define (snake+img snake img)
  (segs+img (snake-segs snake) img))

;Contrato: segs+img: list image -> image
;Proposito: Funcion que dibuja todos los segmentos
;Ejemplo
(define (segs+img loseg img)
  (cond
    [(empty? loseg) img]
    [else
     (imagen-en-celda
      BODY
      (posn-x (first loseg)) (posn-y (first loseg))
      (segs+img (rest loseg) img))]))
  
;Contrato:food+img: fruta image -> image
;Propósito: Dibujar las fruta en el canvas
;Ejemplo: 
(define (food+img fruta img)
  (imagen-en-celda APPLE (posn-x fruta) (posn-y fruta) img))
;;____________________________FUNCIONES PARA EL MOVIMIENTO______________________________
;Contrato: snake-grow: snake -> snake. Donde snake es una estructura
;Proposito: añade un nuevo segmento a la serpiente en la "cabecera" a una direccion dada
;Ejemplo: 
(define (snake-grow snake)
  (make-snake (cons (new-seg (first (snake-segs snake)) (snake-dir snake))
              (snake-segs snake))
              (snake-dir snake)))

;Contrato: new-seg: snake-dir -> snake-seg
;Proposito: Funcion que crea un nuevo segmento
;Ejemplo:
(define (new-seg seg dir)
  (cond
    [(string=? "up" dir) (make-posn (posn-x seg) (+ (posn-y seg) 1))]
    [(string=? "down" dir) (make-posn (posn-x seg) (- (posn-y seg) 1))]
    [(string=? "left" dir) (make-posn (- (posn-x seg) 1) (posn-y seg))]
    [else (make-posn (+ (posn-x seg) 1) (posn-y seg))]))

;Contrato: snake-slither: snake -> snake. Donde snake es una estructura
;Proposito: Funcion que simula el movimiento de la serpiente eliminando el ultimo segmento
;y añadiendo otro al inicio, teniendo como referencia la direccion en la
;que se esta  moviendo
(define (snake-slither snake)
  (make-snake (cons (new-seg (first (snake-segs snake)) (snake-dir snake))
                    (nuke-last (snake-segs snake)))
              (snake-dir snake)))

;Contrato: nuke-last: snake-> snake
;Proposito: Funcion que retorna una snake sin su ultimo segmento
;Ejemplo:
(define (nuke-last loseg)
  (cond
    [(empty? (rest loseg)) empty]
    [else
     (cons (first loseg) (nuke-last (rest loseg)))]))

;_____________________________________COLISIONES____________________________________
;Contrato: comiendo?: world -> boolean. Donde w es una estructura
;Proposito: Funcion que determina si la cabecera de la serpiente colisiona con una fruta
;Ejemplo
(define (comiendo? w)
    (posn=? (first (snake-segs (world-snake w))) (world-fruta w)))

;Contrato: posn=?: posn posn -> boolean. Donde a y b son puntos 2d
;Proposito: Funcion que determina si dos puntos estan sobrelapados
;Ejemplo
(define (posn=? a b)
  (and (= (posn-x a ) (posn-x b)) (= (posn-y a) (posn-y b))))

;Contrato: self-colission?: world -> boolean. Donde w es una estructura
;Proposito: Funcion que determina si la serpiente se esta chocando con sigo misma
;Ejemplo:
(define (self-collision? w)
  (seg-collision? (first (snake-segs (world-snake w))) (rest (snake-segs (world-snake w)))))

;Contrato: seg-colission?: segs -> boolean
;Proposito: Funcion que determina si un segmento dado esta en el mismo lugar que algún otro en la lista
;Ejemplo:
(define (seg-collision? seg los)
  (cond
    [(empty? los) false]
    [else
     (or (posn=? seg (first los)) (seg-collision? seg (rest los)))]))

;Contrato: world-collision?: world -> boolean. donde w es una estructura
;Proposito: Funcion que determina si la serpiente esta chocando con uno de los bordes del mundo
;Ejemplo:
(define (world-collision? w)
  (not (in-bounds? (first (snake-segs (world-snake w))))))

;Contrato: in-bounds?: p -> boolean. Donde p es un punto 2d
;Proposito: Funcion que determina si un determinado punto esta en el borde del canvas
;Ejemplo:
(define (in-bounds? p)
  (and (>= (posn-x p) 0) (< (posn-x p) N-COLUMNAS)
       (>= (posn-y p) 0) (< (posn-y p) N-FILAS)))

;________________________________________FUNCIONES LOGICAS_____________________________________
;Contrato: next-world: world -> world. donde w es una estructura.
;Propósito: Funcion que calcula el nuevo estado del mundo cada tick del reloj
;Ejemplo: 
(define (next-world w)
  (cond
    [(world-collision? w) WORLD-0]
    [(self-collision? w) WORLD-0]
    [(comiendo? w) (make-world
                    (snake-grow (world-snake w))
                    (make-posn (random N-COLUMNAS)
                               (random N-FILAS)))]
    [else
     (make-world (snake-slither (world-snake w))
                 (world-fruta w))]))

;Contrato: tecla: world key-event -> world. Donde w es una estructura y kev
;Propósito: Funcion que determina el key-event para el movimiento de la serpiente con las teclas
;Ejemplo:
(define (tecla w kev)
  (cond
    [(or
      (key=? kev "down")
      (key=? kev "up")
      (key=? kev "left")
      (key=? kev "right"))
     (make-world (make-snake (snake-segs (world-snake w)) kev)
                 (world-fruta w))]
    [(key=? kev "f") WORLD-0]
    [else
     w]))
  
  ;Contrato: score:  serpiente number-> number
;Propósito: Crear un puntaje. Cada vez que el snake come una fruta
;tiene +1 punto.
;Ejemplos:
;(score world2) => Debe retornar 1
(define (score serpiente n)
  (cond
    [(empty? (rest serpiente)) n]
    [(<= (length serpiente) 1) n]
    [else
     (score (rest serpiente) (+ n 1))]))
;Pruebas
(check-expect (score (snake-segs snake1) 0) 0)
(check-expect (score (snake-segs snake2) 0) 1)
(check-expect (score (snake-segs snake3) 0) 2)

;;guarda el puntaje en un archivo de texto
;(define (guardar-score serpiente n)
 ; (write-file "otp.txt" (number->string (score serpiente n))))

(define (pintar-puntos w )
  (place-image
  (text (number->string (score (snake-segs (world-snake w)) 0)) 20 "white")
   510 15
  (render w)))
  
(define (main w)
  (big-bang w
  [to-draw render]
  [on-tick next-world TICK]
  [on-key tecla]
  [name "snek"]
  ))
