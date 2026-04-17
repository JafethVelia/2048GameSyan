#lang racket
(require 2htdp/image)
(require 2htdp/universe)
(require "2048GAME.rkt")

;; ============================================
;; INTERFAZ GRÁFICA
;; ============================================

;; color-segun-valor: número -> string
(define (color-segun-valor n)
  (cond [(= n 0) "light gray"]
        [(= n 2) "yellow"]
        [(= n 4) "orange"]
        [(= n 8) "red"]
        [(= n 16) "purple"]
        [(= n 32) "blue"]
        [(= n 64) "green"]
        [(= n 128) "cyan"]
        [(= n 256) "magenta"]
        [(= n 512) "pink"]
        [(= n 1024) "brown"]
        [(= n 2048) "gold"]
        [else "black"]))

;; dibujar-baldosa: número -> image
(define (dibujar-baldosa n)
  (cond [(= n 0) (overlay (square 80 "outline" "black")
                          (square 80 "solid" "light gray"))]
        [else (overlay (text (number->string n) 32 "white")
                       (square 80 "solid" (color-segun-valor n))
                       (square 80 "outline" "black"))]))

;; dibujar-fila: lista -> image
(define (dibujar-fila fila)
  (cond [(empty? fila) (square 0 "solid" "white")]
        [else (beside (dibujar-baldosa (car fila))
                      (dibujar-fila (cdr fila)))]))

;; dibujar-tablero: tablero -> image
(define (dibujar-tablero tablero)
  (cond [(empty? tablero) (square 0 "solid" "white")]
        [else (above (dibujar-fila (car tablero))
                     (dibujar-tablero (cdr tablero)))]))

;; dibujar-puntaje: número -> image
(define (dibujar-puntaje puntaje)
  (text (string-append "Puntaje: " (number->string puntaje)) 36 "black"))

;; dibujar-mensaje: string -> image
(define (dibujar-mensaje msg)
  (text msg 48 "red"))

;; dibujar-todo: estado -> image
(define (dibujar-todo estado)
  (define tablero (car estado))
  (define puntaje (cdr estado))
  (define filas (obtener-filas tablero))
  (define columnas (obtener-columnas tablero))
  (define texto-tamaño (text (string-append "Tablero " (number->string filas) "x" (number->string columnas)) 24 "black"))
  (define texto-salir (text "Cierra la ventana para salir" 18 "gray"))
  (define espacio (rectangle 1 20 "solid" "white"))
  (define victoria (contiene-2048 tablero))
  (define terminado (juego-terminado? tablero))
  (cond [victoria (above (dibujar-puntaje puntaje)
                         texto-tamaño
                         (dibujar-tablero tablero)
                         (dibujar-mensaje "¡GANASTE!")
                         texto-salir
                         espacio)]
        [terminado (above (dibujar-puntaje puntaje)
                          texto-tamaño
                          (dibujar-tablero tablero)
                          (dibujar-mensaje "GAME OVER")
                          texto-salir
                          espacio)]
        [else (above (dibujar-puntaje puntaje)
                     texto-tamaño
                     (dibujar-tablero tablero)
                     texto-salir
                     espacio)]))

;; mover-con-tecla: estado tecla -> estado
(define (mover-con-tecla estado tecla)
  (define tablero (car estado))
  (define puntaje (cdr estado))
  (define nuevo-tablero
    (cond [(key=? tecla "left") (mover-tablero-izquierda tablero)]
          [(key=? tecla "right") (mover-tablero-derecha tablero)]
          [(key=? tecla "up") (mover-tablero-arriba tablero)]
          [(key=? tecla "down") (mover-tablero-abajo tablero)]
          [else tablero]))
  (if (equal? tablero nuevo-tablero)
      estado
      (cons nuevo-tablero (+ puntaje (calcular-puntaje-ganado tablero nuevo-tablero)))))

;; insertar-nueva-ficha: estado -> estado
(define (insertar-nueva-ficha estado)
  (define tablero (car estado))
  (define puntaje (cdr estado))
  (define vacias (posiciones-vacias tablero))
  (cond [(empty? vacias) estado]
        [else (define nuevo-tablero (insertar-ficha-aleatoria tablero))
              (cons nuevo-tablero puntaje)]))

;; actualizar-juego: estado tecla -> estado
(define (actualizar-juego estado tecla)
  (define tablero (car estado))
  (define victoria (contiene-2048 tablero))
  (define terminado (juego-terminado? tablero))
  (cond [victoria estado]
        [terminado estado]
        [else (define nuevo-estado (mover-con-tecla estado tecla))
              (define nuevo-tablero (car nuevo-estado))
              (if (equal? tablero nuevo-tablero)
                  nuevo-estado
                  (insertar-nueva-ficha nuevo-estado))]))

;; crear-estado-inicial: -> estado
(define (crear-estado-inicial)
  (cons (tablero-inicial) 0))

;; iniciar-juego: -> void
(define (iniciar-juego)
  (big-bang (crear-estado-inicial)
            (to-draw dibujar-todo)
            (on-key actualizar-juego)))

(iniciar-juego)