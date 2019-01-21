#lang racket
;; to take averages
(require math/statistics)

(require "typed-othello-game-logic.rkt")
(require "strategies.rkt")
(require "visualizer.rkt")
(require "utils.rkt")

;; all the layout files
(define all-layout-paths
         (map path->string (directory-list "layouts" #:build? #t)))

;; layout files that test that the output is correct
(define test-paths (filter (lambda (s) (string-contains? s "test-easy")) all-layout-paths))
(define hard-test-paths (filter (lambda (s) (string-contains? s "test-hard")) all-layout-paths))

;; a list of the available eval functions
(define eval-fns (list weighted-squares count-difference))

;; test which strategy is faster
(define (time-strategy strategy)
  (let ([all-boards (map layout-path-parser all-layout-paths)])
    (mean
     (map (lambda (board)
            (mean (map (lambda (player)
                         (let ([t0 (current-gc-milliseconds)])
                           (strategy player board)
                           (- (current-gc-milliseconds) t0)))
                       (list WHITE BLACK))))
          all-boards))))

(define (alpha-beta-speed-up depth)
  (mean
   (map (lambda (e-fn)
          (-
           (time-strategy (minimax-searcher depth e-fn))
           (time-strategy (alpha-beta-searcher depth e-fn))))
        eval-fns)))

(define (alpha-beta-faster?)
  (let ([prev-val (alpha-beta-speed-up 3)]
        [sorted #t]
        [val 0]
        [depth 0])
    (for ([i (range 4 6)]
          #:break (not sorted))
      (set! val (alpha-beta-speed-up i))
      (set! depth i)
      (if (< prev-val val)
          (set! prev-val val)
          (set! sorted #f)))
    (cond
      (sorted #t)
      (else
       (display "\n speed up for ")
       (display (- depth 1))
       (display " : ")
       (display prev-val)
       (display " >= speed up for ")
       (display depth)
       (display " : ")
       (display val)
       (display "\n")
       #f))))

;; test which strategy wins
(define (correct-winners? board-layout-path eval-fn)
  (let ([strategies (list
                     (maximizer eval-fn)
                     (minimax-searcher 2 eval-fn)
                     (alpha-beta-searcher 5 eval-fn))])
    (define correct #t)
    (for ([white-index (range (length strategies))]
          #:break (not correct))
      (define white-strategy (list-ref strategies white-index))
      (for ([black-index (range (length strategies))]
            #:unless (equal? white-index black-index)
            #:break (not correct))
        (define black-strategy (list-ref strategies black-index))
        (define result (play board-layout-path white-strategy black-strategy))
        (cond
          ((and (< white-index black-index) (not (equal? result 'BLACK-WINS)))
           (display "\n")
           (display (object-name black-strategy))
           (display " lost to ")
           (display (object-name white-strategy))
           (display " when playing as black on ")
           (display board-layout-path)
           (display "\n")
           (set! correct #f))
          ((and (> white-index black-index) (not (equal? result 'WHITE-WINS)))
           (display "\n")
           (display (object-name white-strategy))
           (display " lost to ")
           (display (object-name black-strategy))
           (display " when playing as white on ")
           (display board-layout-path)
           (display "\n")
           (set! correct #f)))))
    correct))

;; does the strategy find the right square
(define-struct board-and-answer (board white-answer black-answer layout-string))

(define (get-correct-move label)
  (cond
    ((equal? label "#f") #f)
    (else (string->number label))))

(define (test-single-moves strategy hard)
  (let ([board-and-answer-list (map
                             (lambda (board-layout-path)
                               (let ([board (layout-path-parser board-layout-path)]
                                     [parts (string-split (car (string-split board-layout-path ".lay")) "-")])
                                 (board-and-answer board (get-correct-move (third parts)) (get-correct-move (fourth parts)) board-layout-path)))
                             (if hard hard-test-paths test-paths))])
    (define correct #t)
    (define answer #f)
    (define correct-answer #f)
    (for ([board-and-answer board-and-answer-list]
          #:break (not correct))
      (for ([player (list BLACK WHITE)]
            #:break (not correct))
        (set! answer (strategy player (board-and-answer-board board-and-answer)))
        (if (equal? player WHITE)
            (set! correct-answer (board-and-answer-white-answer board-and-answer))
            (set! correct-answer (board-and-answer-black-answer board-and-answer)))
        (cond
          ((not (equal? answer correct-answer))
           (display "\n")
           (display (object-name strategy))
           (display " returned: ")
           (display answer)
           (display " for ")
           (display (if (equal? player BLACK) 'BLACK 'WHITE))
           (display " in ")
           (display (board-and-answer-layout-string board-and-answer))
           (display " but the correct answer is: ")
           (display correct-answer)
           (display "\n")
           (set! correct #f)))))
    correct))

;; final test
(define (auto-grader)
  (and
   (test-single-moves (alpha-beta-searcher 2 weighted-squares) #f)
   (test-single-moves (minimax-searcher 2 weighted-squares) #f)
   (test-single-moves (alpha-beta-searcher 4 weighted-squares) #t)
   (alpha-beta-faster?)
   (correct-winners? "layouts/basic.lay" weighted-squares)
   (correct-winners? "layouts/basic.lay" count-difference)))

;(auto-grader)