#lang racket

(provide (all-defined-out))

;; a representation of the game state for visualization
(define-struct game-state (board player))

;; make some constants
(define all-directions '(-11 -10 -9 -1 1 9 10 11))

(define all-squares
  (filter
   (lambda (i) (<= 1 (modulo i 10) 8)) (range 11 89)))

(define EMPTY #\space)
(define BLACK #\B)
(define WHITE #\W)
(define WALL #\%)

;; some stuff for evaluating positions
(define winning-value (* 200 100))
(define losing-value (- 0 (* 200 100)))

(define (final-value player board)
  "Is this a win, loss, or draw for player?"
  (let ([state (sgn (count-difference player board))])
    (cond
      ((equal? state -1) losing-value)
      ((equal? state 1) winning-value)
      (else 0))))

;; to read in the board
(define (layout-path-parser path)
  (let ([matrix (file->lines path #:mode 'text #:line-mode 'linefeed)])
    (foldr append
           null
           (map string->list matrix))))

;; some helper functions
(define (opponent player)
  (if (equal? player BLACK)
      WHITE
      BLACK))

(define (board-ref board move)
  (list-ref board move))

(define (count-difference player board)
  "Count player's pieces minus opponent's pieces."
  (let ([countFun (lambda (person)
                    (lambda (peice)
                      (equal? peice person)))])
    (- (count (countFun player) board)
       (count (countFun (opponent player)) board))))

;; some stuff for moving
(define (valid-p move)
  "Valid moves are numbers in the range 11-88 that end in 1-8."
  (and
   (and (<= 11 move) (<= move 88))
   (and (<= 1 (modulo move 10)) (<= (modulo move 10) 8))))

(define (legal-p move player board)
  "A Legal move must be into an empty square, and it must
  flip at least one opponent piece."
  (and (equal? (board-ref board move) EMPTY)
       (ormap (lambda (dir)
                (would-flip? move player board dir))
             all-directions)))

(define (would-flip? move player board dir)
  "Would this move result in any flips in this direction?
  If so, return the square number of the bracketing piece."
  ;; A flip occurs if, starting at the adjacent square, c, there
  ;; is a string of at least one opponent pieces, bracketed by 
  ;; one of player's pieces
  (let ([c (+ move dir)])
    (and (equal? (board-ref board c) (opponent player))
         (find-bracketing-piece (+ c dir) player board dir))))

(define (find-bracketing-piece square player board dir)
  "Return the square number of the bracketing piece."
  (cond
    ((equal? (board-ref board square) player)
     square)
    ((equal? (board-ref board square) (opponent player))
     (find-bracketing-piece (+ square dir) player board dir))
    (else #f)))

(define (any-legal-move? player board)
  "Does player have any legal moves in this position?"
  (ormap (lambda (move) (legal-p move player board))
        all-squares))

(define (legal-moves player board)
  "Returns a list of legal moves for player"
  ;;*** fix, segre, 3/30/93.  Was remove-if, which can share with all-squares.
  (filter (lambda (move) (legal-p move player board)) all-squares))

(define (make-move move player board)
  "Update board to reflect move by player"
  (let ([positions-claimed (get-positions-claimed move player board)])
    (map (lambda (pair)
           (if
            (member (first pair) positions-claimed)
            player
            (second pair)))
         (map list (range 0 (length board)) board))))

(define (get-positions-claimed move player board)
  "Get all positions that should be changed to player peices."
  ;; add 1 to final place in range so it includes the end
  (foldr append
         ;; the place we move to will definitely change
         (list move)
         (filter-map
          (lambda (dir)
            (let ([bracket (would-flip? move player board dir)])
              (and bracket
                   (cond
                     ((< 0 dir) (range (+ move dir) (+ 1 bracket) dir))
                     (else (range bracket (+ 1 (+ move dir)) (- 0 dir)))))))
          all-directions)))

;; who plays next
(define (next-to-play board previous-player)
  "Compute the player to move next, or NIL if nobody can move."
  (let ([opp (opponent previous-player)])
    (cond ((any-legal-move? opp board) opp)
          ;; current player has no legal moves and must pass
          ((any-legal-move? previous-player board) previous-player)
          (else #f))))

;; prettier print of board
(define (display-board board)
  (define WIDTH (sqrt (length board)))
  (display
   (list->string
    (flatten
     (map
      (lambda (pair)
        (cond
          ((equal? (- WIDTH 1) (modulo (first pair) WIDTH))
           (list (second pair) #\newline))
          (else (second pair))))
      (map list (range 0 (length board)) board))))))

;; when is the game over
(define (game-ended game-state)
    (let ([board (game-state-board game-state)]
          [player (game-state-player game-state)])
      (equal? (next-to-play board player) #f)))