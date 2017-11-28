;;Snake
(require 2htdp/universe)
(require 2htdp/image)

;;::::::::::::::::::DEFINICION DE ESTRUCTURAS::::::::::::::::::::::::::::::::::::
;world es una estructura,representa el estado del mundo, compuesta por otras estructuras; snake y fruta
(define-struct world (snake fruta score bonus))
;fruta es una estructura. Pos representa la celda que esta ocupando dentro del canvas posn(x,y)
(define-struct fruta (pos))
;score es una estructura unitaria, n es un número y representa el puntaje del jugador
(define-struct score (n))


(define-struct bonus (posi))
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

(define BODY (rectangle (/ CELDA 1.5) (/ CELDA 1.5) "solid" "white" ))
(define APPLE (rectangle (/ CELDA 1.5) (/ CELDA 1.5) "solid" "green"))
(define BONO (rectangle (/ CELDA 1.5) (/ CELDA 1.5) "solid" "yellow"))

(define WORLD-0
  (make-world (make-snake (list (make-posn 2 6) ) "right")
  (make-posn 1 15) (make-score 0) (make-posn 3 3)))
  
;::::::::::::::::::::::::::::::::::::::::VARIABLES DE TESTEO:::::::::::::::::::::::::::::::::::::::::::::::::::
(define food1 (make-posn 2 5))
(define segs1 (list (make-posn 2 6))) ; one-segment snake
(define segs2 (list (make-posn 2 5) (make-posn 3 5)))
(define segs3 (list (make-posn 2 5) (make-posn 3 5) (make-posn 3 1)))
(define snake1 (make-snake segs1 'up))
(define snake2 (make-snake segs2 'up))
(define snake3 (make-snake segs3 'up))
;(define world1 (make-world snake1 food1))
;(define world2 (make-world snake2 food1)) ; eating



;:::::::::::::::::::::::::::::::::::::::::FUNCIONES DEL MUNDO::::::::::::::::::::::::::::::::::::::::::::::::::::
;;FUNCIONES PARA RENDERIZAR

;Contrato: render: world -> image
;Propósito: Renderizar el estado del mundo
(define (render w)
  (place-image
   (fig-score (snake-segs (world-snake w)))
   480 15
  (snake+img (world-snake w)
             (food+img (world-fruta w)
             (bono+img (world-bonus w)           
                       FONDO)))))

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

;Contrato: score+img: number --> image
;Propósito: Hacer que el puntaje aparezca durante el juego y se actualice
(define (score+img score img)
  (imagen-en-celda (fig-score (snake-segs score)) 510 15 img))

;Contrato: bonus+img bono --> image
;Propósito: Hacer que la fruta de bonus aparezca
(define (bono+img bonus img)
 (imagen-en-celda BONO (posn-x bonus) (posn-y bonus) img))
                               


;bono?
;Condiciones para que el bono aparezca.
(define (bono? w)
 (and (comiendo? w) (= (length (snake-segs (world-snake WORLD-0))) 4)))
;;pinta el score en el mundo
(define (fig-score x)
  (text (string-append "Score: " (number->string (calc-score x 0))) 20 "white"))

(define (calc-score serpiente n)
(cond
  [(empty? serpiente) n]
  [(<= (length serpiente) 1) n]
  [else (calc-score (rest serpiente) (+ n 1))]))

(define (score++ serpiente)
  (make-score (calc-score serpiente 0) ))
  
;dibuja la última escena
(define (last-scene w)
  (place-image
   (text/font "HAS MUERTO" 30 "red" "Times New Roman" 'default 'normal 'bold #f)
   (/ ANCHO 2) (/ LARGO 2)
   (render w)))
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
;;hay que hacerle poliformismo a esta y la de arriba
(define (comiendo-bonus? w)
  (posn=? (first (snake-segs (world-snake w))) (world-bonus w)))

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
       
  
;;evalúa si el jugador ha perdido.
;;es decir, si choca con un muro o si choca consigo mismo
(define (end? w)
  (or (world-collision? w) (self-collision? w)))

       
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
                               (random N-FILAS))
                    (calc-score (snake-segs (world-snake w)) 0)

                   (make-posn (random N-COLUMNAS)
                              (random N-FILAS)))]
    [else
     (make-world (snake-slither (world-snake w))
                 (world-fruta w) (world-score w) (world-bonus w))]))

;Contrato: tecla: world key-event -> world. Donde w es una estructura y kev
;Propósito: Funcion que determina el key-event para el movimiento de la serpiente con las teclas
;Ejemplo:
(define (tecla w kev)
  (cond
    [(and (key=? kev "up") (string=? (snake-dir (world-snake w)) "down"))
     (make-world (make-snake (snake-segs (world-snake w)) "down") (world-fruta w) (world-score w) (world-bonus w))]
    
    [(and (key=? kev "down") (string=? (snake-dir (world-snake w)) "up"))
     (make-world (make-snake (snake-segs (world-snake w)) "up") (world-fruta w) (world-score w) (world-bonus w))]
    
    [(and (key=? kev "left") (string=? (snake-dir (world-snake w)) "right"))
     (make-world (make-snake (snake-segs (world-snake w)) "right") (world-fruta w) (world-score w) (world-bonus w))]
    
    [(and (key=? kev "right") (string=? (snake-dir (world-snake w)) "left"))
     (make-world (make-snake (snake-segs (world-snake w)) "left") (world-fruta w) (world-score w) (world-bonus w))]
    
    [else
     (make-world (make-snake (snake-segs (world-snake w)) kev) (world-fruta w) (world-score w) (world-bonus w))]))
 
  
(define (main w)
  (big-bang w
  [to-draw render]
  [on-tick next-world TICK]
  [on-key tecla]
  [stop-when end? last-scene]
[name "snek"]))
(main WORLD-0)
