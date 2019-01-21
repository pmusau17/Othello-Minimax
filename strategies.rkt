#lang racket
;; so we can randomly choose a move
(require racket/random)

(require "typed-othello-game-logic.rkt")
(require "utils.rkt")

(provide (all-defined-out))

(define (random-strategy player board)
  "A strategy that randomly chooses a move."
  (random-ref (legal-moves player board)))

(define (maximize-difference player board)
  "A strategy that maximizes the difference in pieces."
  ((maximizer count-difference) player board))

(define (maximize-weight player board)
  "A strategy that maximizes the weighted difference in pieces."
  ((maximizer weighted-squares) player board))



(define (minimax player board depth eval-fn)
  "Find the best move, for PLAYER, according to EVAL-FN,
  searching DEPTH levels deep and backing up values."
  (define score -1000000)
  (define best #f)
  (cond
    [(= depth 0)
     ;(define func (maximizer eval-fn))
     (values (eval-fn player board) #f)]
    [else
     ;define the initial score and best to be null
     (cond
       [(any-legal-move? player board)
        ;get the available moves
        (define moves (legal-moves player board))
        ;(println moves)
        ;create the boards resulting from those moves
        (define boards (map (lambda(i)(make-move i player board)) moves))
        ;change to opponent
        (set! player (opponent player))
        ;decrease depth
        (set! depth (- depth 1))
        (define count 0)
        (define tempScore null)
        (define useless null)
        (for ([i boards]) ; iterator binding
          ;(display-board i)
          (set!-values (tempScore useless) (minimax player i depth eval-fn))
          ;update the score correctly
          (cond
            [(> (* -1 tempScore) score) (set! score (* -1 tempScore)) (set! best (list-ref moves count))])
          (set! count (+ count 1))
          )
        (values score best)
        ]
       ;if the opponent can make a move, the current player must pass their turn by playing #f. In this case, the returned values should be
       ;the negation of the opponents minimax score and #f for the move
       ;if neither has a legal move then return a value of (final-value player board and #f for the board)
       [else (cond
                [(any-legal-move? (opponent player) board) (values (* -1 (eval-fn (opponent player) board)) #f)]
                    [else (values (final-value player board) #f)]) ])
     ])
  )


(define (minimax-searcher depth eval-fn)
  "A strategy that searches DEPTH levels and then uses EVAL-FN."
  (let ([output (lambda (player board)
                  (let-values ([(value move) (minimax player board depth eval-fn)])
                    move))])
    (set! output (procedure-rename output
                                   (string->symbol
                                    (string-append
                                     "minimax-searcher-"
                                     (number->string depth)
                                     "-"
                                     (symbol->string
                                      (object-name eval-fn))))))
    output))



(define (alpha-beta player board achievable cutoff depth eval-fn)
  "Find the best move, for PLAYER, according to EVAL-FN,
  searching DEPTH levels deep and backing up values,
  using cutoffs whenever possible."
   (define score losing-value)
   (define best #f)
  ;if n is a leaf node tehn evaluate n and return false for the move
  (cond
    [(= depth 0)
     (values (eval-fn player board) #f)]
    ;else for each child c of n set the path with the current versions of alpha and beta
    [else
     ;define the initial score and best to be null
     (cond
       ;are there any legal moves? if yes continue
       [(any-legal-move? player board)
        ;get the available moves
        (define moves (legal-moves player board))
        ;create the boards resulting from those moves
        (define boards (map (lambda(i)(make-move i player board)) moves))
        ;change to opponent
        (set! player (opponent player))
        ;decrease depth
        (set! depth (- depth 1))
        ;create a count that allows us to access the best move
        (define count 0)
        ;create a variable for storing the score computed by minimax
        (define tempScore null)
        ;the move returned by the other calls isn't really useful so just store it and then get rid of it
        (define useless null)

        ;main loop for the algorithm 
        (for ([i boards]) 
          (set!-values (tempScore useless) (alpha-beta player i (* -1 cutoff) (* -1 achievable) depth eval-fn))
          (cond
            [(> (* -1 tempScore) achievable) (set! achievable (* -1 tempScore)) (set! best (list-ref moves count))])
          ;if achievable is greater than or equal to cutoff stop searching
          #:break (>= achievable cutoff)    
          (set! count (+ count 1))
          )
        (values achievable best)
        ]
       ;if the opponent can make a move, the current player must pass their turn by playing #f. In this case, the returned values should be
       ;the negation of the opponents minimax score and #f for the move
       ;if neither has a legal move then return a value of (final-value player board and #f for the board)
       ;(println "this happened!!!")
       [else  (cond
                [(any-legal-move? (opponent player) board) (values (* -1 (eval-fn (opponent player) board)) #f)]
                    [else (values (final-value player board) #f)]) ])
     ])
  )

(define (alpha-beta-searcher depth eval-fn)
  "A strategy that searches to DEPTH and then uses EVAL-FN."
  (let ([output (lambda (player board)
                  (let-values ([(value move) (alpha-beta player board losing-value winning-value depth eval-fn)])
                    move))])
    (set! output (procedure-rename output
                                   (string->symbol
                                    (string-append
                                     "alpha-beta-searcher-"
                                     (number->string depth)
                                     "-"
                                     (symbol->string
                                      (object-name eval-fn))))))
    output))
