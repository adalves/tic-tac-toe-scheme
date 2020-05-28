;Ana Carolina Alves - Jogo da Velha

(define win-move '((1 2 3) (4 5 6) (7 8 9) (1 4 7) (2 5 8) (3 6 9) (1 5 9) (3 5 7)))

(define (print-board board)
    (if (not (null? board))
        (begin 
            (display (list (car board) (cadr board) (caddr board))) 
            (newline) 
            (print-board (cdddr board)))
            ()))

(define (free-space? n board)
    (equal? (list-ref board (- n 1)) n))

(define (place-move player n board)
    (if (equal? (car board) n)
        (cons player (cdr board))
        (cons (car board) (place-move player n (cdr board)))))

(define (win? player board)
    (check-win? (pl-brd player board 1) win-move))

    (define (pl-brd player board n)
        (if (not (null? board))
            (if (equal? player (car board))
                (cons n (pl-brd player (cdr board) (+ n 1)))
                (pl-brd player (cdr board) (+ n 1)))
            '()))

    (define (check-win-move? pl-brd sub-wp)
        (cond
            ((null? sub-wp) #t)
            ((null? pl-brd) #f)
            (else 
                (if (equal? (car pl-brd) (car sub-wp))
                    (check-win-move? (cdr pl-brd)(cdr sub-wp))
                    (check-win-move? (cdr pl-brd) sub-wp)))))

    (define (check-win? pl-brd wp)
        (if (null? wp)
            #f
            (if (check-win-move? pl-brd (car wp))
                #t
                (check-win? pl-brd (cdr wp)))))      

(define (draw? board)
    (if (null? board)
        #t
        (if (number? (car board))
            #f
            (draw? (cdr board)))))

(define (opponent player)
    (if (equal? player "X")
        "O"
        "X"))

(define (player-turn player board ai)
    (display "Vez de ")
    (display player)
    (display ": ")
    (let ((n (read)))
        (if (and (number? n) (< n 10) (> n 0) (free-space? n board))
            (let ((board (place-move player n board)))
                (print-board board)
                (cond 
                    ((win? player board) (begin (display player) (display " ganhou!")))
                    ((draw? board) (display "Empate!"))
                    (ai (ai-turn (opponent player) board))
                    (else (player-turn (opponent player) board ai))))
            (begin 
                (display "Valor inválido.") 
                (newline) 
                (player-turn player board ai)))))

(define (score player board)
    (cond
        ((win? player board) 10)
        ((win? (opponent player) board) -10)
        ((draw? board) 0)
        (else #f)))

(define (get-best-move player board)
    (define (try-move player board c-move b-score b-move)
        (if (> c-move 9)
            b-move
            (if (free-space? c-move board)
                (let ((s (minimax player (place-move player c-move board))))
                    ;(display c-move) (display " ") (display s) (newline)
                    (if (> s b-score) 
                        (try-move player board (+ c-move 1) s c-move)
                        (try-move player board (+ c-move 1) b-score b-move)))
                (try-move player board (+ c-move 1) b-score b-move))))
    (try-move player board 1 -100 1))

(define (minimax player board)
    (define (try-score player board c-move b-score max depth)
        (if (> c-move 9)
            b-score
            (let ((s (score player board)))
                (if (not s)
                    (if (free-space? c-move board)
                        (if max
                            (let ((t (try-score player (place-move player c-move board) 1 100 (not max) (+ depth 1))))
                                (if (> (- t depth) b-score)
                                    (try-score player board (+ c-move 1) (- t depth) max depth)
                                    (try-score player board (+ c-move 1) b-score max depth)))
                            (let ((t (try-score player (place-move (opponent player) c-move board) 1 -100 (not max) (+ depth 1))))
                                (if (< (+ t depth) b-score)
                                    (try-score player board (+ c-move 1) (+ t depth) max depth)
                                    (try-score player board (+ c-move 1) b-score max depth))))
                        (try-score player board (+ c-move 1) b-score max depth))
                    s))))
    (try-score player board 1 100 #f 0))

(define (ai-turn player board)
    (display "Vez de ")
    (display player)
    (display "(AI): ") (newline)
    (let ((board (place-move player (get-best-move player board) board)))
        (print-board board)
        (cond 
            ((win? player board) (begin (display player) (display " ganhou!")))
            ((draw? board) (display "Empate!"))
            (else (player-turn (opponent player) board #t)))))

(define (game-start player ai)
    (print-board '(1 2 3 4 5 6 7 8 9))
    (if (equal? player "O")
        (ai-turn "X" '(1 2 3 4 5 6 7 8 9))
        (player-turn player '(1 2 3 4 5 6 7 8 9) ai)))

(define (start-ai-mode)
    (display "1. Primeira jogada (X)") (newline)
    (display "2. Segunda jogada (O) - lento!") (newline)
    (let ((n (read)))
        (cond 
            ((equal? n 1) (game-start "X" #t))
            ((equal? n 2) (game-start "O" #t))
            (else (start)))))

(define (start-2player-mode)
    (game-start "X" #f))

(define (start)
    (display "Jogo da Velha!") (newline)
    (display "1. Jogador vs AI") (newline)
    (display "2. Jogador vs Jogador") (newline)
    (let ((n (read)))
        (cond 
            ((equal? n 1) (start-ai-mode))
            ((equal? n 2) (start-2player-mode))
            (else (start)))))

(start)
