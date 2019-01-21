#lang typed/racket

(provide (all-defined-out))

;; a representation of the game state for visualization
(define-struct game-state ([board : (Listof Char)] [player : Char]))
;; a representation of the squares on a board
(define-struct pos-and-peice ([pos : Integer] [peice : Char]))
;; a function to represent the board in this way
(: zip-board (-> (Listof Integer) (Listof Char) (Listof pos-and-peice)))
(define (zip-board positions b-vals)
  (map make-pos-and-peice positions b-vals))

;; make some constants
(: all-directions (Listof Integer))
(define all-directions '(-11 -10 -9 -1 1 9 10 11))

(: all-squares (Listof Integer))
(define all-squares
  (filter
   (lambda ([i : Integer]) (<= 1 (modulo i 10) 8)) (range 11 89)))

(define EMPTY #\space)
(define BLACK #\B)
(define WHITE #\W)
(define WALL #\%)

;; some stuff for evaluating positions
(: winning-value Integer)
(define winning-value (* 200 100))
(: losing-value Integer)
(define losing-value (- 0 (* 200 100)))

(: final-value (-> Char (Listof Char) Integer))
(define (final-value [player : Char] [board : (Listof Char)])
  "Is this a win, loss, or draw for player?"
  (let ([state (sgn (count-difference player board))])
    (cond
      ((equal? state -1) losing-value)
      ((equal? state 1) winning-value)
      (else 0))))

;; to read in the board
(define (layout-path-parser [path : String])
  (let ()
    (define matrix : (Listof String) (file->lines path #:mode 'text #:line-mode 'linefeed))
    (: matrix-to-board (-> (-> (Listof Char) (Listof Char) (Listof Char)) (Listof Char) (Listof String) (Listof Char)))
    (define (matrix-to-board fn seed stringList)
      (foldr fn seed (map string->list stringList)))
    (matrix-to-board append null matrix)))

;; some helper functions
(define (opponent [player : Char])
  (if (equal? player BLACK)
      WHITE
      BLACK))

(define (board-ref [board : (Listof Char)] [move : Integer])
  (list-ref board move))

(: count-difference (-> Char (Listof Char) Integer))
(define (count-difference [player : Char] [board : (Listof Char)])
  "Count player's pieces minus opponent's pieces."
  (let ([countFun (lambda (person)
                    (lambda (peice)
                      (equal? peice person)))])
    (- (count (countFun player) board)
       (count (countFun (opponent player)) board))))

;; some stuff for moving
(define (valid-p [move : Integer])
  "Valid moves are numbers in the range 11-88 that end in 1-8."
  (and
   (and (<= 11 move) (<= move 88))
   (and (<= 1 (modulo move 10)) (<= (modulo move 10) 8))))

(define (legal-p [move : Integer] [player : Char] [board : (Listof Char)])
  "A Legal move must be into an empty square, and it must
  flip at least one opponent piece."
  (and (equal? (board-ref board move) EMPTY)
       (ormap (lambda ([dir : Integer])
                (would-flip? move player board dir))
             all-directions)))

(: would-flip? (-> Integer Char (Listof Char) Integer (U Integer Boolean)))
(define (would-flip? [move : Integer] [player : Char] [board : (Listof Char)] [dir : Integer])
  "Would this move result in any flips in this direction?
  If so, return the square number of the bracketing piece."
  ;; A flip occurs if, starting at the adjacent square, c, there
  ;; is a string of at least one opponent pieces, bracketed by 
  ;; one of player's pieces
  (let ([c (+ move dir)])
    (and (equal? (board-ref board c) (opponent player))
         (find-bracketing-piece (+ c dir) player board dir))))

(: find-bracketing-piece (-> Integer Char (Listof Char) Integer (U Boolean Integer)))
(define (find-bracketing-piece [square : Integer] [player : Char] [board : (Listof Char)] [dir : Integer])
  "Return the square number of the bracketing piece."
  (cond
    ((equal? (board-ref board square) player)
     square)
    ((equal? (board-ref board square) (opponent player))
     (find-bracketing-piece (+ square dir) player board dir))
    (else #f)))

(define (any-legal-move? [player : Char] [board : (Listof Char)])
  "Does player have any legal moves in this position?"
  (ormap (lambda ([move : Integer]) (legal-p move player board))
        all-squares))

(define (legal-moves [player : Char] [board : (Listof Char)])
  "Returns a list of legal moves for player"
  ;;*** fix, segre, 3/30/93.  Was remove-if, which can share with all-squares.
  (filter (lambda ([move : Integer]) (legal-p move player board)) all-squares))

(define (make-move [move : Integer] [player : Char] [board : (Listof Char)])
  "Update board to reflect move by player"
  (let ()
    (: positions-claimed (Listof Integer))
    (define positions-claimed (get-positions-claimed move player board))
    (map (lambda ([pair : pos-and-peice])
           (if
            (member (pos-and-peice-pos pair) positions-claimed)
            player
            (pos-and-peice-peice pair)))
         (zip-board (range 0 (length board)) board))))

(: get-positions-claimed (-> Integer Char (Listof Char) (Listof Integer)))
(define (get-positions-claimed [move : Integer] [player : Char] [board : (Listof Char)])
  "Get all positions that should be changed to player peices."
  ;; add 1 to final place in range so it includes the end
  (: compute-positions (-> (-> (Listof Integer) (Listof Integer) (Listof Integer)) (Listof Integer) (Listof Integer) (Listof Integer)))
  (define (compute-positions fn seed move-group)
    (foldr fn seed
           (filter-map get-vals move-group)))
  (: get-vals (-> Integer (U False (Listof Integer))))
  (define (get-vals dir)
    (let ([bracket (would-flip? move player board dir)])
      (and (exact-integer? bracket)
           (cond
             ((and (integer? bracket) (< 0 dir)) (range (+ move dir) (+ 1 bracket) dir))
             (else (range bracket (+ 1 (+ move dir)) (- 0 dir)))))))
  (compute-positions append
         ;; the place we move to will definitely change
         (list move)
         all-directions))

;; who plays next
(define (next-to-play [board : (Listof Char)] [previous-player : Char])
  "Compute the player to move next, or NIL if nobody can move."
  (let ([opp (opponent previous-player)])
    (cond ((any-legal-move? opp board) opp)
          ;; current player has no legal moves and must pass
          ((any-legal-move? previous-player board) previous-player)
          (else #f))))

;; prettier print of board
(define (display-board [board : (Listof Char)])
  (: WIDTH Integer)
  (define WIDTH (integer-sqrt (length board)))
  (: add-newlines (-> (Listof Char) String))
  (define (add-newlines board)
    (: add-newline (-> pos-and-peice (Listof Char)))
    (define (add-newline pair)
      (cond
          ((equal? (- WIDTH 1) (modulo (pos-and-peice-pos pair) WIDTH))
           (list (pos-and-peice-peice pair) #\newline))
          (else (list (pos-and-peice-peice pair)))))
    (: insert-newline (-> (Listof Char) (Listof (Listof Char))))
    (define (insert-newline b-vals)
      (map add-newline (zip-board (range 0 (length board)) b-vals)))
    (: flatten-board (-> (-> (Listof Char) (Listof Char) (Listof Char)) (Listof Char) (Listof Char) (Listof Char)))
    (define (flatten-board fn seed b-vals)
      (foldr fn seed (insert-newline b-vals)))
    (list->string (flatten-board append null board)))
  (display (add-newlines board)))
   

;; when is the game over
(define (game-ended [game-state : game-state])
    (let ([board (game-state-board game-state)]
          [player (game-state-player game-state)])
      (equal? (next-to-play board player) #f)))
