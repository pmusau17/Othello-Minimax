#lang racket

(require "typed-othello-game-logic.rkt")

(provide (all-defined-out))

;; some functions to help out

(define (maximizer eval-fn)
  "Return a strategy that will consider every legal move,
  apply EVAL-FN to each resulting board, and choose 
  the move for which EVAL-FN returns the best score.
  FN takes two arguments: the player-to-move and board"
  (let ([output
         (lambda (player board)
           (let ([moves (legal-moves player board)]
                 [highest-score #f]
                 [best-move #f])
             (let ([scores (map
                            (lambda (move)
                              (eval-fn
                               player
                               (make-move move player board)))
                            moves)])
               (for ([score-move-pair (map list scores moves)])
                 (cond
                   ((or (equal? highest-score #f) (< highest-score (first score-move-pair)))
                    (set! highest-score (first score-move-pair))
                    (set! best-move (second score-move-pair)))))
               best-move)))])
    (set! output (procedure-rename output
                                   (string->symbol
                                    (string-append
                                     "maximizer-"
                                     (symbol->string
                                      (object-name eval-fn))))))
    output))

(define (weighted-squares player board)
  "Sum of the weights of player's squares minus opponent's."
  (let ([opp (opponent player)])
    (apply +
     (map
      (lambda (i)
        (cond
          ((equal? (board-ref board i) player) (list-ref *weights* i))
          ((equal? (board-ref board i) opp) (- 0 (list-ref *weights* i)))
          (else 0)))
      all-squares))))

(define *weights*
  '(0   0   0  0  0  0  0   0   0 0
     0 120 -20 20  5  5 20 -20 120 0
     0 -20 -40 -5 -5 -5 -5 -40 -20 0
     0  20  -5 15  3  3 15  -5  20 0
     0   5  -5  3  3  3  3  -5   5 0
     0   5  -5  3  3  3  3  -5   5 0
     0  20  -5 15  3  3 15  -5  20 0
     0 -20 -40 -5 -5 -5 -5 -40 -20 0
     0 120 -20 20  5  5 20 -20 120 0
     0   0   0  0  0  0  0   0   0 0))