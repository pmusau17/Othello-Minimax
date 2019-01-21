#lang racket
(require "typed-othello-game-logic.rkt")
;; to make pictures
(require 2htdp/image)
;; to visualize the output
(require 2htdp/universe) 

(provide play)
(provide visualize-play)
(provide round-robin)
(provide round-robin-full-results)

;; helper function for determining who won
(define (get-winner game-state)
  (let ([result (sgn (count-difference WHITE (game-state-board game-state)))])
    (cond
      ((equal? result -1) 'BLACK-WINS)
      ((equal? result 1) 'WHITE-WINS)
      (else 'DRAW))))

;; just play a game without visualizing it
(define (play board-layout-path white-move-func black-move-func)
  (define (ticker game-state)
      (if (equal? (game-state-player game-state) WHITE)
          (tick game-state white-move-func)
          (tick game-state black-move-func)))
  (define game-state
    (let ([board (layout-path-parser board-layout-path)])
      (make-game-state board (next-to-play board BLACK))))
  (set! WIDTH (sqrt (length (game-state-board game-state))))
  (for ([i (in-naturals 1)]
        #:break (game-ended game-state))
    (set! game-state (ticker game-state)))
  (get-winner game-state))

;; show the game being played
(define (visualize-play board-layout-path white-move-func black-move-func tock)
  (define (ticker game-state)
      (if (equal? (game-state-player game-state) WHITE)
          (tick game-state white-move-func)
          (tick game-state black-move-func)))
  (define first-game-state
    (let ([board (layout-path-parser board-layout-path)])
      (make-game-state board (next-to-play board BLACK))))
  (set! WIDTH (sqrt (length (game-state-board first-game-state))))
  (get-winner
   (big-bang first-game-state
     (on-tick ticker tock)
     (to-draw render)
     (stop-when game-ended last-scene))))

;; play a round robin to compare strategies
(define (round-robin-full-results board-layout-path strategies)
  (foldr append null
         (map (lambda (white-strategy)
                (map (lambda (black-strategy)
                       (list (object-name white-strategy)
                             (object-name black-strategy)
                             (play board-layout-path white-strategy black-strategy)))
                     strategies))
              strategies)))

(define (round-robin board-layout-path strategies)
  (let ([results (round-robin-full-results board-layout-path strategies)])
    (define (winning-strategy result)
      (cond
        ((equal? (last result) 'BLACK-WINS) (second result))
        ((equal? (last result) 'WHITE-WINS) (first result))
        (else #f)))
    (let ([winners (map winning-strategy results)])
      (map (lambda (strategy)
             (list (object-name strategy)
                   (count (lambda (elem) (equal? elem (object-name strategy))) winners)))
           strategies))))

;; constants
;; how many pixels wide is each cell
(define CELL-SIZE 30)
(define WIDTH 10)

;; what to do at each time tick
(define (tick game-state move-func)
  (let ([board (game-state-board game-state)]
        [player (game-state-player game-state)])
    (let ([move (move-func player board)])
      (let ([new-board (make-move move player board)])
        (make-game-state new-board (next-to-play new-board player))))))

;; what to do in the end
(define (last-scene game-state)
  (render game-state))

;; walls, empty cells and peices
(define EMPTY-PIC  (overlay
                    (rectangle (- CELL-SIZE 1) (- CELL-SIZE 1) "solid" "ForestGreen")
                    (rectangle CELL-SIZE CELL-SIZE "solid" "black"))) ; empty cell
(define WHITE-PIC  (overlay (circle (/ CELL-SIZE 3)  "solid" "white") EMPTY-PIC))       ; dot in cell
(define BLACK-PIC  (overlay (circle (/ CELL-SIZE 3)  "solid" "black") EMPTY-PIC))       ; dot in cell
(define WALL-PIC (rectangle CELL-SIZE CELL-SIZE "solid" "black"))  ; wall

;; draw an empty board to start out with
(define (empty-drawing board)
  (empty-scene WIDTH WIDTH "black"))

;; get the appropriate picture for each row and column
(define (get-pic board row col)
  (let ([pos-character (board-ref board (+ (* row WIDTH) col))])
    (cond
      [(equal? WALL pos-character) WALL-PIC]
      [(equal? WHITE pos-character) WHITE-PIC]
      [(equal? BLACK pos-character) BLACK-PIC]
      [else EMPTY-PIC])))

;; draw each part
(define (render game-state)
  (let ([board (game-state-board game-state)])
    (let ([drawing (empty-drawing board)])
      (for ([row (range WIDTH)])
        (for ([col (range WIDTH)])
          (set! drawing (overlay/xy (get-pic board row col) (- 0 (* CELL-SIZE col)) (- 0 (* CELL-SIZE row)) drawing))))
      drawing)))

;;(render (make-game-state (layout-path-parser "layouts/basic.lay") WHITE))