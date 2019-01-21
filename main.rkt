#lang racket
(require "strategies.rkt")
(require "visualizer.rkt")
(require "typed-othello-game-logic.rkt")
(require "utils.rkt")

;; layout file
(define layout-path-parameter (make-parameter "layouts/basic.lay"))

;; for visualizing a game
(define tock (make-parameter 0.5))

;; for playing a single game
(define white-strategy-parameter (make-parameter "random-strategy"))
(define black-strategy-parameter (make-parameter "random-strategy"))

;; for playing round robin
(define strategies-parameter (make-parameter null))

;; a list of the available eval functions
(define eval-fns (list weighted-squares count-difference))

;; alpha-beta and minimax for different depths and eval functions
(define (get-depths-and-evals strategy-maker)
  (foldr append null
         (map (lambda (depth)
                (map (lambda (e-fn)
                       (strategy-maker depth e-fn))
                     eval-fns))
              (range 1 11))))

;; the strategies to choose from
(define possible-strategies (append
                             (list random-strategy maximize-difference maximize-weight)
                             (get-depths-and-evals minimax-searcher)
                             (get-depths-and-evals alpha-beta-searcher)))

(define parser
  (command-line
   #:once-each
   [("-l" "--layout") LAYOUT-PATH-PARAMETER
                      "maze layout filepath"
                      (layout-path-parameter LAYOUT-PATH-PARAMETER)]
   [("-w" "--white-strategy") WHITE-STRATEGY-PARAMETER
                      "strategy for white player"
                      (white-strategy-parameter WHITE-STRATEGY-PARAMETER)]
   [("-b" "--black-strategy") BLACK-STRATEGY-PARAMETER
                      "strategy for black player"
                      (black-strategy-parameter BLACK-STRATEGY-PARAMETER)]
   [("-t" "--tock") TOCK
                      "tock length in seconds during visualization (default .5)"
                      (tock (string->number TOCK))]
   #:multi
   [("-s" "--strategies") SEARCH-STRATEGY-PARAMETER
                      "strategies to run in a round robin"
                      (strategies-parameter (cons SEARCH-STRATEGY-PARAMETER (strategies-parameter)))]))

(define (name-to-strategy strategy-name)
  (let ([output #f])
    (set! strategy-name (string->symbol strategy-name))
    (for ([strategy possible-strategies]
      #:unless (not (equal? (object-name strategy) strategy-name)))
      (set! output strategy))
    (if output output (error "not a valid strategy name:" strategy-name))))

(define white-strategy (name-to-strategy (white-strategy-parameter)))
(define black-strategy (name-to-strategy (black-strategy-parameter)))

(define strategies (map name-to-strategy (strategies-parameter)))

(if (> (length strategies) 0)
    (for ([result
           (sort (round-robin (layout-path-parameter) strategies)
                 (lambda (x y) (> (second x) (second y))))])
      (display result)
      (display "\n"))
    (visualize-play (layout-path-parameter) white-strategy black-strategy (tock)))