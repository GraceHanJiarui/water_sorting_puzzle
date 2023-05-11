;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname tubes-bonus) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; **********************************************
;; Jiarui Han (20991186)
;; CS 135 Fall 2022
;; Assignment 10, Problem 2
;; **********************************************
;;

(require "lib-tubes.rkt")

;; A Game is (make-game Nat Nat (listof (listof Sym)))
(define-struct game (tubesize maxcolours tubes))

;;; Constants

(define emptygame
  (make-game 0 5
             (list empty empty empty empty empty)))

(define emptygame2
  (make-game 10 3 empty))

(define emptygame3
  (make-game 10 3 (list empty empty)))

(define smallgame1
  (make-game 2 2
             (list (list 'blue 'red)
                   (list 'blue 'red)
                   (list))))

(define smallgame2
  (make-game 2 3
             (list (list 'blue 'red)
                   (list 'blue 'red)
                   (list))))

(define smallinvalidgame1
  (make-game 2 1
             (list (list 'blue 'red)
                   (list 'blue 'red)
                   (list))))


(define smallinvalidgame2
  (make-game 2 2
             (list (list 'blue 'red)
                   (list 'blue 'blue)
                   (list))))

(define smallinvalidgame3
  (make-game 2 2
             (list (list 'blue 'red 'blue)
                   (list 'red)
                   (list))))


(define smallgamefinal
  (make-game 2 2
             (list (list)
                   (list 'blue 'blue)
                   (list 'red 'red))))


(define mediumgame
  (make-game 2 3
             (list (list 'blue 'red)
                   (list 'red 'yellow)
                   (list 'yellow 'blue)
                   (list))))

(define mediumgamestuck
  (make-game 2 3
             (list (list 'blue 'red)
                   (list 'red 'yellow)
                   (list 'yellow 'blue)
                   )))

(define largergame
  (make-game 3 3
             (list (list 'blue 'red 'red)
                   (list 'yellow 'blue 'yellow)
                   (list 'red 'yellow 'blue)
                   (list))))

(define biggame
  (make-game 5 3
             (list (list 'blue 'blue 'red 'red 'yellow)
                   (list 'red 'red 'yellow 'blue 'red)
                   (list 'yellow 'blue 'blue 'yellow 'yellow)
                   (list)
                   (list))))

(define biggame2
  (make-game 5 3
             (list (list 'yellow 'blue 'blue 'yellow 'yellow)
                   (list 'red 'red 'yellow 'blue 'red)
                   (list)
                   (list 'blue 'blue 'red 'red 'yellow)
                   (list))))

(define biggamesolve
  (make-game 5 3
             (list (list 'blue 'blue 'blue 'blue 'blue)
                   (list 'red 'red 'red 'red 'red)
                   (list 'yellow 'yellow 'yellow 'yellow 'yellow)
                   (list)
                   (list))))

(define hugegame
  (make-game 4 9
             (list (list 'purple 'pink 'yellow 'blue)
                   (list 'blue 'green 'purple 'white)
                   (list 'orange 'yellow 'black 'blue)
                   (list 'white 'orange 'orange 'pink)
                   (list 'pink 'red 'red 'black)
                   (list 'yellow 'green 'orange 'blue)
                   (list 'white 'purple 'red 'yellow)
                   (list 'green 'red 'green 'black)
                   (list 'purple 'black 'white 'pink)
                   (list)
                   (list))
             ))
(define my-game
  (make-game 4 4
             (list (list 'blue 'blue 'red 'red )
                   (list 'red 'blue 'red)
                   (list 'yellow 'blue 'yellow 'yellow)
                   (list 'yellow)
                   (list)
                   (list))))

(define (check-colour? size num los)
  (cond
    [(empty? los) (>= num 0)]
    [(= size (foldr
              (lambda (x occurance)
                (cond[(symbol=? x (first los)) (add1 occurance)]
                     [else occurance])) 0 los))
     (check-colour? size (sub1 num) (filter (lambda (x) (not (symbol=? x (first los)))) los))]
    [else false]))

(define (valid-game? gm)
  (and (check-colour? (game-tubesize gm)
                    (game-maxcolours gm)
                    (foldr
                     (lambda (x acc)
                       (foldr (lambda (y acc1) (cons y acc1)) acc x))
                     empty (game-tubes gm)))
       (foldr (lambda (x acc)
                (and (<= (length x) (game-tubesize gm)) acc)) true (game-tubes gm))))

(define (remove-completed gm)
  (local[(define game-tubes-after
           (foldr (lambda (x acc)
                    (cond
                      [(and (= (length x) (game-tubesize gm))
                            (empty? (filter (lambda (y) (not (symbol=? y (first x)))) x)))
                       acc]
                      [else (cons x acc)])) empty (game-tubes gm)))]
  (make-game (game-tubesize gm)
             (local [;; (count llos) produces the amount of colours in the llos
                     ;; count: (listof (listof Sym)) -> Nat
                     (define (count llos)
                       (cond [(empty? llos) 0]
                             [else
                              (add1
                               (count
                                (filter (lambda (x) (not (symbol=? (first llos) x)))
                                        llos)))]))]
               (count (foldr (lambda (x rror) (append x rror))
                             empty
                             game-tubes-after)))
             game-tubes-after)))

(define (finished-game? gm)
  (empty? (filter (lambda (x) (not (empty? x))) (game-tubes (remove-completed gm)))))

(define (num-blocks llos)
  (cond
    [(empty? llos) 0]
    [(empty? (first llos)) (num-blocks (rest llos))]
    [else (+ (length (foldr
                      (lambda (x acc)
                        (cond
                          [(or (empty? acc)
                               (not (symbol=? x (first acc))))
                           (cons x acc)]
                          [(symbol=? x (first acc)) acc])) empty (first llos)))
             (num-blocks (rest llos)))]))


(define (equiv-game? gm1 gm2)
  (local[(define gml1 (game-tubes gm1))
         (define gml2 (game-tubes gm2))
         ;; (tube-equiv? los1 los2) produces true if all the symbols in los1 are the same
         ;; as that of los2 in the same order, and false otherwise.
         ;; tube-equiv?: (listof Sym) (listof Sym) -> Bool
         (define (tube-equiv? los1 los2)
           (cond
             [(empty? los1) (empty? los2)]
             [(empty? los2) false]
             [else (and (symbol=? (first los1) (first los2))
                        (tube-equiv? (rest los1) (rest los2)))]))
         ;; (filter-tube los1 gml2) filters out one of the same element as los1
         ;; in gml1, and produces the rest of the game
         ;; filter-tube: (listof Sym) Game -> Game
         (define (filter-tube los1 gml2)
           (cond
             [(empty? gml2) empty]
             [(tube-equiv? los1 (first gml2)) (rest gml2)]
             [else (cons (first gml2) (filter-tube los1 (rest gml2)))]))]
    (cond
      [(empty? gml1) (and (empty? gml2)
                          (= (game-maxcolours gm1) (game-maxcolours gm2))
                          (= (game-tubesize gm1) (game-tubesize gm2)))]
      [else
       (and (= (length gml1) (length gml2))
            (equiv-game?
             (make-game (game-maxcolours gm1)
                        (game-tubesize gm1)
                        (rest gml1))
             (make-game (game-maxcolours gm2)
                        (game-tubesize gm2)
                        (filter-tube (first gml1) gml2))))])))


(define (all-equiv? log1 log2)
  (local[;; (filter-game los1 log2)  filters out one of the same element as los1
         ;; in log2, and returns the rest of log2
         ;; filter-game: Game (listof Game) -> (listof Game)
         (define (filter-game los1 log2)
           (cond
             [(empty? log2) empty]
             [(equiv-game? los1 (first log2)) (rest log2)]
             [else (cons (first log2) (filter-game los1 (rest log2)))]))]
  (cond
      [(empty? log1) (empty? log2)]
      [else
       (and (= (length log1) (length log2))
            (all-equiv? (rest log1) (filter-game (first log1) log2)))])))

(define (next-games gm) (next-games/acc gm 0 empty))

(define (next-games/acc gm index my-llos)
  (local[(define gm-los (game-tubes gm))]
    (cond
      [(>=  index (length gm-los)) my-llos]
      [(empty? (list-ref gm-los index))
       (next-games/acc gm (add1 index) my-llos)]
      [else (next-games/acc gm (add1 index) (append 
      (filter game?
              (build-list
               (length gm-los)
               (lambda (x)
                 (cond
                   [(or (= index x)
                        (>= (length (list-ref gm-los x)) (game-tubesize gm)))
                    false]
                   [else
                     (make-game
                      (game-tubesize gm)
                      (game-maxcolours gm)
                      (foldr cons empty (build-list (length gm-los) (lambda (y) (cond
                         [(= y index)
                          (rest (list-ref gm-los y))]
                         [(= x y)
                          (cons (first (list-ref gm-los index)) (list-ref gm-los y))]
                         [else
                          (list-ref gm-los y)])))))])))) my-llos))])))

(define (my-member? los1 log2)
           (cond
             [(empty? log2) false]
             [(equiv-game? los1 (first log2)) true]
             [else (my-member? los1 (rest log2))]))

;; (solve gm draw-option) determines if the game gm is solveable,
;; and will also draw each possible move depending on the draw-option

;; Examples:
(check-expect (time (solve biggame 'off)) true)

;; solve: Game (anyof 'off 'norm 'slow 'fast) -> Bool
(define (solve gm draw-option)
  (cond
    [(boolean? (solve/acc gm draw-option)) true]
    [else false]))

(define (solve/acc gm draw-option)
  (local
    [(define setup (puzzle-setup gm draw-option))
     (define (solve-helper to-visit visited)
       (cond
         [(empty? to-visit) visited]
         [else
          (local
            [(define draw (draw-board (first to-visit) draw-option))
             (define new-visit (sort-gm to-visit))]
            (cond
              [(finished-game? (first new-visit)) true]
              [(my-member? (first new-visit) visited)
               (solve-helper (rest new-visit) visited)]
              [else
               (local [(define nbrs (next-games (first new-visit)))
                       (define new (filter (lambda (x) (not (my-member? x visited))) nbrs))
                       (define result (solve-helper new (cons (first new-visit) visited)))]
                 (cond [(boolean? result) true]
                       [else (solve-helper (rest new-visit) result)]))]))]))]
    (solve-helper (list gm) empty)))

(define (sort-gm lst-gm)
  (local [(define (bottom sym los acc)
            (cond [(empty? los) (add1 acc)]
                  [(symbol=? sym (first los))
                   (bottom sym (rest los) (add1 acc))]
                  [else (bottom (first los) (rest los) 0.5)]))
          (define (bottom/lst llos)
            (cond [(empty? llos) 0]
                  [else (+ (cond [(empty? (first llos)) 1]
                                 [else (bottom (first (first llos)) (rest (first llos)) 0.5)])
                           (bottom/lst (rest llos)))]))
          (define (sort-helper lst)
            (quicksort lst (lambda (x y) (cond [(>= (bottom/lst (game-tubes x))
                                                   (bottom/lst (game-tubes y)))
                                                true]
                                               [else false]))))]
    (sort-helper lst-gm)))

;(check-expect (time (solve biggame 'slow)) true)
(check-expect (time (solve hugegame 'off)) true)
(check-expect (solve mediumgamestuck 'off) false)