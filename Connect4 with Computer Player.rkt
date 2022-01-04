#lang typed/racket
(require typed/test-engine/racket-tests)
(require "../include/cs151-core.rkt")
(require "../include/cs151-image.rkt")
(require "../include/cs151-universe.rkt")

;; We'll use the same version of Some and Optional that we used on Homework 5.
(define-struct (Some X)
  ([value : X]))

(define-type (Optional X)
  (U 'none (Some X)))

;; The game has two players, who we'll call 'black and 'white. You can choose
;; any color or pattern you would like for the pieces, but we'll still call
;; the players by these names.
(define-type Player (U 'black 'white 'hover))

;; (Pos row col) represents a position on the game board. The lower-left corner
;; is at row 0, column 0. The lower-right corner is at row 0, column 6. The
;; upper-left corner is at row 5, column 0. The upper-right corner is at row 5,
;; column 6.
(define-struct Pos
  ([row : Integer]   ;; integer between 0 and 6 inclusive
   [col : Integer])) ;; integer between 0 and 5 inclusive

;; (Stack height pieces) represents one full column of the game board. The
;; integer height is the number of pieces currently in that column. The
;; list pieces is a list of the pieces in the column (or more precisely, a list
;; of the players who placed the pieces). The last element of the list is the
;; bottom piece in the stack, while the first element of the list is the top
;; piece in the stack (so far). The value of height should alway match the
;; length of the list pieces.
(define-struct Stack
  ([height : Integer]
   [pieces : (Listof Player)]))

;; (Board stacks) represents a game board. The list stacks will always have
;; seven elements, representing the seven columns of the game board. The first
;; element of stacks represents the leftmost column of the board.
(define-struct Board
  ([stacks : (Listof Stack)]))

;; (Game board next) represents the state of the game at a given moment of time.
;; The current state of the game board is saved in board. The player whose turn
;; it is currently is stored in next.
(define-struct Game
  ([board : Board]
   [next : Player]))

;; If a player has won the game by creating a line of four, then
;; (Winning-Line player start end) can be used to keep track of which player
;; created the line of four, and where that line is (it goes from start to end).
;; Generally, if a player's winning move creates more than one line of four, we
;; won't care which one gets represented.
(define-struct Winning-Line
  ([player : Player]
   [start : Pos]
   [end : Pos]))

;; visualization


(overlay
 (circle (* 80 7/16) "solid" "gray")
 (square 80  "solid" "black"))

(: print-stack : Stack Integer -> Image)
;; prints a stack of colors based on a lsit 
(define (print-stack stack padding)
  (local
    {(: print-stack-aux : Player Image -> Image)
     (define (print-stack-aux player image)
       (above (overlay
               (circle (* padding 13/32) "solid"
                       (if (symbol=? player 'black)
                           "cadetblue"
                           "aquamarine"))   
               (square padding "solid" "black"))
              image))}
    (match stack
      [(Stack 0 '()) (square padding 0 "black")]
      [(Stack height players) (foldr print-stack-aux empty-image players)])))
(check-expect (image-height (print-stack (Stack 2 (list 'black 'black)) 80))
              160) 
(check-expect (image-width (print-stack (Stack 2 (list 'black 'black)) 80))
              80)                      
      

(: board-image : Board Integer -> Image)
;; construct an image of the board to be used for
;; Parameter "board": the board
;; Parameer "padding": the padding between the centers of the circles
(define (board-image board padding)
  (local
    {(: board-image-width : Integer Integer -> Image)
     ;; build one row of the board
     (define (board-image-width w padding)
       (if (> w 0)
           (beside
            (overlay
             (circle (* padding 13/32) "solid" "gray")
             (square padding  "solid" "black"))
            (board-image-width (- w 1) padding))
           empty-image))
     (: board-image-height : Integer Integer Integer -> Image)
     ;; stack each row on top of each other to reach the desired height of
     ;; of the board
     (define (board-image-height h w padding)
       (if (> h 0)
           (above
            (board-image-width w padding)
            (board-image-height (- h 1) w padding))
           empty-image))
     (: board-image-fold : Stack Image -> Image)
     ;; overlay the current game over the board image
     (define (board-image-fold stack image)
       (beside/align "bottom" 
                     (print-stack stack padding) image))}
    (overlay/align "left" "bottom"
                   (foldr board-image-fold empty-image (Board-stacks board))
                   (board-image-height 6 7 padding))))


(board-image (Board (list (Stack 0 '())
                          (Stack 1 (list 'black))
                          (Stack 2 (list 'black 'white))
                          (Stack 3 (list 'black 'white 'white))
                          (Stack 4 (list 'black 'white 'white 'white))
                          (Stack 5 (list 'black 'white 'white 'white 'white))
                          (Stack 6 (list 'black 'white 'black 'black
                                         'black 'white)))) 80)
(board-image (Board (list (Stack 0 (list 'white 'black 'black 'black 'black))
                          (Stack 1 (list 'white 'black 'black 'black))
                          (Stack 2 (list 'white 'black 'black))
                          (Stack 3 (list 'white 'black))
                          (Stack 4 '())
                          (Stack 5 '())
                          (Stack 6 '()))) 80)
(board-image (Board (list (Stack 0 '())
                          (Stack 1 '())
                          (Stack 2 (list 'white))
                          (Stack 3 (list 'white 'black))
                          (Stack 4 (list 'white 'black 'black))
                          (Stack 5 (list 'white 'black 'black 'black))
                          (Stack 6 '()))) 80)

(check-expect
 (image-width
  (board-image (Board (list (Stack 0 '())
                            (Stack 1 '())
                            (Stack 2 (list 'white))
                            (Stack 3 (list 'white 'black))
                            (Stack 4 (list 'white 'black 'black))
                            (Stack 5 (list 'white 'black 'black 'black))
                            (Stack 6 '()))) 80)) 560)
(check-expect
 (image-height
  (board-image (Board (list (Stack 0 '())
                            (Stack 1 '())
                            (Stack 2 (list 'white))
                            (Stack 3 (list 'white 'black))
                            (Stack 4 (list 'white 'black 'black))
                            (Stack 5 (list 'white 'black 'black 'black))
                            (Stack 6 '()))) 80)) 480)
                            
(: new-game : Game)
;; starts a new game with no pieces on the board
(define new-game
  (Game (Board (list (Stack 0 '())
                     (Stack 0 '())
                     (Stack 0 '())
                     (Stack 0 '())
                     (Stack 0 '())
                     (Stack 0 '())
                     (Stack 0 '()))) 'white))

(board-image (Game-board new-game) 80)

(: board-ref : Board Pos -> (Optional Player))
;; Determine if there is a piece played in a specific place
(define (board-ref board position)
  (match board
    [(Board lst)
     (match (list-ref lst (Pos-col position))
       [(Stack h stack-list)
        (if (>= (- h 1) (Pos-row position))
            (Some (list-ref (reverse stack-list) (Pos-row position)))
            'none)])]))

(check-expect (board-ref
               (Board (list
                       (Stack 0 '())
                       (Stack 1 (list 'black))
                       (Stack 2 (list 'black 'white))
                       (Stack 3 (list 'black 'white 'white))
                       (Stack 4 (list 'black 'white 'white 'white))
                       (Stack 5 (list 'black 'white 'white 'white 'white))
                       (Stack 6 (list 'black 'white 'black 'black
                                      'black 'white)))) (Pos 0 1))
              (Some 'black))
(check-expect (board-ref
               (Board (list
                       (Stack 0 '())
                       (Stack 1 (list 'black))
                       (Stack 2 (list 'black 'white))
                       (Stack 3 (list 'black 'white 'white))
                       (Stack 4 (list 'black 'white 'white 'white))
                       (Stack 5 (list 'black 'white 'white 'white 'white))
                       (Stack 6 (list 'black 'white 'black 'black
                                      'black 'white)))) (Pos 1 1))
              'none)
(check-expect (board-ref
               (Board (list
                       (Stack 0 '())
                       (Stack 1 (list 'black))
                       (Stack 2 (list 'black 'white))
                       (Stack 3 (list 'black 'white 'white))
                       (Stack 4 (list 'black 'white 'white 'white))
                       (Stack 5 (list 'black 'white 'white 'white 'white))
                       (Stack 6 (list 'black 'white 'black 'black
                                      'black 'white)))) (Pos 0 2))
              (Some 'white))

(: valid-move? : Game Player Integer -> Boolean)
;; determine if the column is valid, is not full, and their turn
(define (valid-move? game player column)
  (match game 
    [(Game board next)
     (if (symbol=? player next)
     ;;(if (or (symbol=? player next) (not (symbol=? 'hover player)))
         ;;(error "must switch player")
         (if (not (and (>= column 0) (<= column 6)))
             #f
             (match (list-ref (Board-stacks board) column)
               [(Stack height stack-list)
                (if (>= height 6)
                    #f
                    #t)]))
         (if (not (and (>= column 0) (<= column 6)))
             #f
             (match (list-ref (Board-stacks board) column)
               [(Stack height stack-list)
                (if (>= height 6)
                    #f
                    #t)])))]))

(check-expect (valid-move? new-game 'white 2) #t)
(check-expect (valid-move? new-game 'black 7) #f)
(check-expect (valid-move?
               (Game (Board (list
                             (Stack 0 '())
                             (Stack 1 (list 'black))
                             (Stack 2 (list 'black 'white))
                             (Stack 3 (list 'black 'white 'white))
                             (Stack 4 (list 'black 'white 'white 'white))
                             (Stack 5 (list 'black 'white 'white 'white 'white))
                             (Stack 6 (list 'black 'white 'black 'black
                                            'black 'white)))) 'white) 'black 6)
              #f)
(check-expect (valid-move? new-game 'black 4) #t)

(: apply-move : Game Player Integer -> Game)
;; check if the move is valid, if it is, apply move to board.
;; Else raise an error
(define (apply-move game player column)
  (if (valid-move? game player column)
      (match game
        [(Game board next)
         (match board
           [(Board stacks)
            (local
              {(: apply-move-aux : (Listof Stack) Integer Integer Player ->
                  (Listof Stack))
               ;; Take in stack and outputs new stack given the addition
               ;; by a player
               (define (apply-move-aux stack column acc player)
                 (match stack
                   [(cons (Stack h lst) rest)
                    (if (= column acc)
                        (cons (Stack (+ h 1) (append (list player) lst)) rest)
                        (cons (Stack h lst) (apply-move-aux rest column
                                                            (+ acc 1) player)))]
                   ['() '()]))}
              (Game (Board (apply-move-aux stacks column 0 player))
                    (if (symbol=? next 'white)
                        'black
                        'white)))])])
      (error "not a valid move")))

(check-error (apply-move (Game (Board
                                (list
                                 (Stack 0 '())
                                 (Stack 1 (list 'black))
                                 (Stack 2 (list 'black 'white))
                                 (Stack 3 (list 'black 'white 'white))
                                 (Stack 4 (list 'black 'white 'white 'white))
                                 (Stack 5 (list 'black 'white 'white 'white
                                                'white))
                                 (Stack 6 (list 'black 'white 'black 'black
                                                'black 'white)))) 'white)
                         'black 7) "not a valid move")  
(check-expect (apply-move new-game 'black 0) (Game
                                              (Board
                                               (list
                                                (Stack 1 '(black))
                                                (Stack 0 '())
                                                (Stack 0 '())
                                                (Stack 0 '())
                                                (Stack 0 '())
                                                (Stack 0 '())
                                                (Stack 0 '())))
                                              'black))
(check-expect (apply-move (Game
                           (Board
                            (list
                             (Stack 1 (list 'black))
                             (Stack 0 '())
                             (Stack 0 '())
                             (Stack 0 '())
                             (Stack 0 '())
                             (Stack 0 '())
                             (Stack 0 '())))
                           'black)
                          'white 0)
              (Game
               (Board
                (list
                 (Stack 2 (list 'white  'black))
                 (Stack 0 '())
                 (Stack 0 '())
                 (Stack 0 '())
                 (Stack 0 '())
                 (Stack 0 '())
                 (Stack 0 '())))
               'white))

(: column-win-check : Game Player -> (Optional Winning-Line))
;; determine if a player has 4 pieces next to each other in a column
(define (column-win-check game player)
  (match game 
    [(Game board p)
     (match board
       [(Board stacks)
        (local
          {(: column-win-check-aux : (Listof Stack) Integer ->
              (Optional Winning-Line))
           ;; check if a stack has more than 4 elements and if there are 4 in a
           ;; row
           (define (column-win-check-aux stack column)
             (match stack
               [(cons (Stack h lst) rest)
                (if (>= h 4)
                    (local
                      {(: check-same-player : (Listof Player) Integer
                          -> Integer)
                       ;; aux funciton to check if there are 4 of the same
                       ;; player in a column and output a integer to determine
                       ;; which row the line of 4 starts in
                       (define (check-same-player list row)
                         (if (>= (length list) 4)
                             (match list
                               [(cons x1 (cons x2 (cons x3 (cons x4 xr))))
                                (if (and (symbol=? x1 player)
                                         (symbol=? x2 player)
                                         (symbol=? x3 player)
                                         (symbol=? x4 player))
                                    row
                                    (check-same-player
                                     (cons x2 (cons x3 (cons x4 xr)))
                                     (+ row 1)))])
                             -1))}
                      (if (= (check-same-player lst 0) -1)
                          (column-win-check-aux rest (+ 1 column))
                          (Some (Winning-Line player
                                              (Pos (check-same-player
                                                    (reverse lst) 0)
                                                   column)
                                              (Pos (+ 3
                                                      (check-same-player
                                                       (reverse lst) 0))
                                                   column)))))
                    (column-win-check-aux rest (+ 1 column)))]
               ['() 'none]))}
          (column-win-check-aux stacks 0))])]))
(check-expect (column-win-check
               (Game (Board (list
                             (Stack 0 '())
                             (Stack 1 (list 'black))
                             (Stack 2 (list 'black 'white))
                             (Stack 3 (list 'black 'white 'white))
                             (Stack 4 (list 'black 'white 'white 'white))
                             (Stack 5 (list 'black 'white 'white 'white 'white))
                             (Stack 6 (list 'black 'white 'black 'black
                                            'black 'white)))) 'white) 'black)
              'none)
(check-expect (column-win-check
               (Game (Board (list
                             (Stack 0 '())
                             (Stack 1 (list 'black))
                             (Stack 2 (list 'black 'white))
                             (Stack 3 (list 'black 'white 'white))
                             (Stack 4 (list 'black 'white 'white 'white))
                             (Stack 5 (list 'black 'white 'white 'white 'white))
                             (Stack 6 (list 'black 'white 'black 'black
                                            'black 'white)))) 'white) 'white)
              (Some
               (Winning-Line
                'white
                (Pos 0 5)
                (Pos 3 5)))) 
               
(: chip-id : Stack Integer -> (U 'none 'white 'black 'hover))
;; determine the identity of the chip in a specific location in a stack
(define (chip-id stack index)
  (match stack
    [(Stack h lst)
     (if (>= index h)
         'none
         (list-ref lst (- h index 1)))]))

(check-expect (chip-id (Stack 2 (list 'black 'white)) 1) 'black) 
(check-expect (chip-id (Stack 2 (list 'black 'white)) 3) 'none) 
(check-expect (chip-id (Stack 2 (list 'black 'white)) 2) 'none) 
(check-expect (chip-id (Stack 0 (list)) 2) 'none)

(: row-win-check : Game Player -> (Optional Winning-Line))
;; determine if a player has 4 pieces next to each other in a row
(define (row-win-check game player)
  (match game 
    [(Game board p)
     (match board
       [(Board stacks)
        (local
          {(: row-win-check-aux : (Listof Stack) Integer ->
              (Optional Winning-Line))
           ;; check if comparative row index is within height of the board
           (define (row-win-check-aux stacklist1 row)
             (if (>= row 6)
                 'none
                 (local
                   {(: check-same-player-row : (Listof Stack) Integer Integer
                       -> Integer)
                    ;; check if there are 4 pieces in a row that have
                    ;; the same type
                    (define
                      (check-same-player-row stacklist2 index column-index)
                      (if (>= (length stacklist2) 4)
                          (match stacklist2
                            [(cons x1 (cons x2 (cons x3 (cons x4 rest))))
                             (if (and (symbol=? (chip-id x1 index) player)
                                      (symbol=? (chip-id x2 index) player)
                                      (symbol=? (chip-id x3 index) player)
                                      (symbol=? (chip-id x4 index) player))
                                 column-index
                                 (check-same-player-row
                                  (cons x2 (cons x3 (cons x4 rest))) index
                                  (+ column-index 1)))])
                          -1))}
                   (if (= (check-same-player-row stacklist1 row 0) -1)
                       (row-win-check-aux stacklist1 (+ 1 row))
                       (Some
                        (Winning-Line player
                                      (Pos row
                                           (check-same-player-row stacklist1
                                                                  row 0))
                                      (Pos row
                                           (+ 3 (check-same-player-row
                                                 stacklist1 row 0)))))))))}
          (row-win-check-aux stacks 0))]
       ['() 'none])]))
               
(check-expect (row-win-check
               (Game (Board (list
                             (Stack 0 '())
                             (Stack 1 (list 'black))
                             (Stack 2 (list 'black 'white))
                             (Stack 3 (list 'black 'white 'white))
                             (Stack 4 (list 'black 'white 'white 'white))
                             (Stack 5 (list 'black 'white 'white 'white 'white))
                             (Stack 6 (list 'black 'white 'black 'black
                                            'black 'white)))) 'white) 'white)
              (Some
               (Winning-Line
                'white
                (Pos 0 2)
                (Pos 0 5))))
(check-expect (row-win-check
               (Game (Board (list
                             (Stack 0 '())
                             (Stack 1 (list 'white))
                             (Stack 2 (list 'black 'white))
                             (Stack 2 (list 'black 'white))
                             (Stack 2 (list 'black 'black))
                             (Stack 2 (list 'black 'white))
                             (Stack 0 '()))) 'white) 'black)
              (Some
               (Winning-Line
                'black
                (Pos 1 2)
                (Pos 1 5)))) 
(check-expect (row-win-check
               (Game (Board (list
                             (Stack 0 '())
                             (Stack 1 (list 'black))
                             (Stack 2 (list 'black 'white))
                             (Stack 3 (list 'black 'white 'white))
                             (Stack 4 (list 'black 'white 'white 'black))
                             (Stack 5 (list 'black 'white 'white 'white 'white))
                             (Stack 6 (list 'black 'white 'black 'black
                                            'black 'white)))) 'white) 'white)
              'none)                                   
                                 
(: +-diagonal-win-check : Game Player -> (Optional Winning-Line))
;; determine if a player has 4 pieces in a consecutive positive diagonal 
(define (+-diagonal-win-check game player)
  (match game 
    [(Game board p)
     (match board
       [(Board stacks)
        (local
          {(: +-diagonal-win-check-aux : (Listof Stack) Integer ->
              (Optional Winning-Line))
           ;; check if comparative diagonal index is within height of the board
           (define (+-diagonal-win-check-aux stacklist1 row)
             (if (>= (+ row 3) 6)
                 'none
                 (local
                   {(: check-same-player-diagonal :
                       (Listof Stack) Integer Integer -> Integer)
                    ;; check if there are 4 pieces in a diagonal that have
                    ;; the same type
                    (define
                      (check-same-player-diagonal stacklist2 index column-index)
                      (if (>= (length stacklist2) 4)
                          (match stacklist2
                            [(cons x1 (cons x2 (cons x3 (cons x4 rest))))
                             (if (and (symbol=? (chip-id x1 index) player)
                                      (symbol=? (chip-id x2 (+ index 1)) player)
                                      (symbol=? (chip-id x3 (+ index 2)) player)
                                      (symbol=? (chip-id x4 (+ index 3))
                                                player))
                                 column-index
                                 (check-same-player-diagonal
                                  (cons x2 (cons x3 (cons x4 rest))) index
                                  (+ column-index 1)))])
                          -1))}
                   (if (= (check-same-player-diagonal stacklist1 row 0) -1)
                       (+-diagonal-win-check-aux stacklist1 (+ 1 row))
                       (Some
                        (Winning-Line player
                                      (Pos row
                                           (check-same-player-diagonal
                                            stacklist1
                                            row 0))
                                      (Pos (+ row 3)
                                           (+ 3 (check-same-player-diagonal
                                                 stacklist1 row 0)))))))))}
          (+-diagonal-win-check-aux stacks 0))]
       ['() 'none])]))        
(check-expect (+-diagonal-win-check
               (Game (Board (list
                             (Stack 0 '())
                             (Stack 1 (list 'black))
                             (Stack 2 (list 'black 'white))
                             (Stack 3 (list 'black 'white 'white))
                             (Stack 4 (list 'black 'white 'white 'black))
                             (Stack 5 (list 'black 'white 'white 'white 'white))
                             (Stack 6 (list 'black 'black 'black 'black
                                            'black 'white)))) 'white) 'white)
              (Some
               (Winning-Line
                'white
                (Pos 0 2)
                (Pos 3 5))))                    
(check-expect (+-diagonal-win-check
               (Game (Board (list
                             (Stack 0 '())
                             (Stack 1 (list 'black))
                             (Stack 2 (list 'black 'white))
                             (Stack 3 (list 'black 'black 'white))
                             (Stack 4 (list 'black 'white 'white 'black))
                             (Stack 5 (list 'black 'white 'white 'white 'white))
                             (Stack 6 (list 'black 'black 'black 'black
                                            'black 'white)))) 'white) 'white)
              'none)  

(: --diagonal-win-check : Game Player -> (Optional Winning-Line))
;; determine if there are 4 of the same pieces in a negative diagonal
(define (--diagonal-win-check game player)
  (match game
    [(Game board p)
     (local
       {(: --diagonal-win-check-aux : Pos -> (Optional Winning-Line))
        ;; takes in a position and determines if there is a negative diagonal
        ;; winning line
        (define ( --diagonal-win-check-aux position)
          (match position
            [(Pos row col)
             (if (< row 3)
                 'none
                 (if (> col 3)
                     (--diagonal-win-check-aux (Pos (- row 1) 0))
                     (match (board-ref board position)
                       ['none (if (> col 4)
                                  (--diagonal-win-check-aux (Pos (- row 1) 0))
                                  (--diagonal-win-check-aux (Pos row
                                                                 (+ col 1))))]
                       [(Some x1)
                        (match (board-ref board (Pos (- row 1) (+ col 1)))
                          ['none (if (> col 4)
                                     (--diagonal-win-check-aux (Pos (- row 1)
                                                                    0))
                                     (--diagonal-win-check-aux (Pos row
                                                                    (+ col 1)
                                                                    )))]
                          [(Some x2)
                           (match (board-ref board (Pos (- row 2) (+ col 2)))
                             ['none (if (> col 4)
                                        (--diagonal-win-check-aux (Pos (- row 1)
                                                                       0))
                                        (--diagonal-win-check-aux (Pos row
                                                                       (+ col 1)
                                                                       )))]
                             [(Some x3)
                              (match (board-ref board (Pos (- row 3) (+ col 3)))
                                ['none (if (> col 4)
                                           (--diagonal-win-check-aux (Pos
                                                                      (- row 1)
                                                                      0))
                                           (--diagonal-win-check-aux
                                            (Pos row
                                                 (+ col 1))))]
                                [(Some x4)
                                 (if (and (symbol=? x1 player)
                                          (symbol=? x2 player)
                                          (symbol=? x3 player)
                                          (symbol=? x4 player))
                                     (Some (Winning-Line
                                            player
                                            (Pos row col)
                                            (Pos (- row 3) (+ col 3))))
                                     (if (> col 4)
                                         (--diagonal-win-check-aux
                                          (Pos (- row 1) 0))
                                         (--diagonal-win-check-aux
                                          (Pos row (+ col 1)))))])
                              ])])])))]))}
       (--diagonal-win-check-aux (Pos 5 0)))]))
                                       
                                    

(check-expect (--diagonal-win-check
               (Game (Board (list
                             (Stack 0 '())
                             (Stack 0 '())
                             (Stack 1 (list 'white))
                             (Stack 2 (list 'white 'black))
                             (Stack 3 (list 'white 'black 'black))
                             (Stack 0 '())
                             (Stack 0 '())))  'white) 'white)
              'none)  
(check-expect (--diagonal-win-check
               (Game (Board (list
                             (Stack 5 (list 'white 'black 'black 'black 'black))
                             (Stack 4 (list 'white 'black 'black 'black))
                             (Stack 3 (list 'white 'black 'black))
                             (Stack 2 (list 'white 'black))
                             (Stack 0 '())
                             (Stack 0 '())
                             (Stack 0 '())))  'white) 'white)
              (Some
               (Winning-Line
                'white
                (Pos 4 0)
                (Pos 1 3))))
      

(: outcome : Game -> (U Winning-Line 'tie 'ongoing))
;; determine the current status of a game
(define (outcome game)
  (match (column-win-check game 'white)
    [(Some c-winning-line-white) c-winning-line-white]
    ['none
     (match (column-win-check game 'black)
       [(Some c-winning-line-black) c-winning-line-black]
       ['none
        (match (row-win-check game 'white)
          [(Some r-winning-line-white) r-winning-line-white]
          ['none
           (match (row-win-check game 'black)
             [(Some r-winning-line-black) r-winning-line-black]
             ['none
              (match (+-diagonal-win-check game 'white)
                [(Some +-winning-line-white) +-winning-line-white]
                ['none
                 (match (+-diagonal-win-check game 'black)
                   [(Some +-winning-line-black) +-winning-line-black]
                   ['none
                    (match (--diagonal-win-check game 'white)
                      [(Some --winning-line-check-white)
                       --winning-line-check-white]
                      ['none
                       (match (--diagonal-win-check game 'black)
                         [(Some --winning-line-check-black)
                          --winning-line-check-black]
                         ['none
                          (match game
                            [(Game (Board (list
                                           (Stack 6 lst1)
                                           (Stack 6 lst2)
                                           (Stack 6 lst3)
                                           (Stack 6 lst4)
                                           (Stack 6 lst5)
                                           (Stack 6 lst6)
                                           (Stack 6 lst7))) player) 'tie]
                            [_ 'ongoing])])])])])])])])]))

(check-expect (outcome (Game (Board (list
                                     (Stack 0 '())
                                     (Stack 1 (list 'black))
                                     (Stack 2 (list 'black 'white))
                                     (Stack 3 (list 'black 'white 'white))
                                     (Stack 4 (list 'black 'white 'white
                                                    'black))
                                     (Stack 5 (list 'black 'white 'white 'white
                                                    'white))
                                     (Stack 6 (list 'black 'black 'black 'black
                                                    'black 'white)))) 'white))
              (Winning-Line 'white (Pos 0 5) (Pos 3 5)))                     
(check-expect (outcome (Game (Board (list
                                     (Stack 6 (list 'black 'black 'black 'white
                                                    'white 'white))
                                     (Stack 6 (list 'white 'white 'white 'black
                                                    'black 'black))
                                     (Stack 6 (list 'black 'black 'black 'white
                                                    'white 'white))
                                     (Stack 6 (list 'white 'white 'white 'black
                                                    'black 'black))
                                     (Stack 6 (list 'black 'black 'black 'white
                                                    'white 'white))
                                     (Stack 6 (list 'white 'white 'white 'black
                                                    'black 'black))
                                     (Stack 6 (list 'black 'black 'black 'white
                                                    'white 'white)))) 'white))
              'tie)  
(check-expect (outcome new-game) 'ongoing)   

(define-struct XY-Pos
  ;; struct to help turn a position into a pixle position 
  ([x : Real]
   [y : Real]))

(: pos->xy-pos : Pos Integer -> XY-Pos)
;; turn a position into a xy position to be overlayed over the board
;; position is based on padding 
(define (pos->xy-pos position padding)
  (match position
    [(Pos row col)
     (XY-Pos (* col padding)
             (* (- 6 row) padding))]))
(check-expect (pos->xy-pos (Pos 1 1) 80) (XY-Pos 80 400))
  

(: game-image : Game Integer
   (U String Symbol) (U String Symbol) String -> Image)
;; graphic display the status of the game
(define (game-image game padding p1 p2 error)
  (match (outcome game)
    ['ongoing (above (board-image (Game-board game) padding)
                     (overlay
                      (match game
                        [(Game board player)
                         (if (not (string=? error "None"))
                             (text error 16 "black")
                             (if (symbol=? player 'white)
                                 (if (symbol? p1)
                                     (text (string-append (symbol->string p1)
                                                          "'s turn") 16 "black")
                                     (text (string-append p1 "'s turn") 16
                                           "black"))
                                 (if (symbol? p2)
                                     (text (string-append (symbol->string p2)
                                                          "'s turn") 16 "black")
                                     (text (string-append p2 "'s turn")
                                           16 "black"))))])
                      (rectangle (* padding 7) padding "outline" "black")))]
    ['tie (above (board-image (Game-board game) padding)
                 (overlay (text "tie game" 16 "black")
                          (rectangle (* padding 7) padding "outline" "black")))]
    [(Winning-Line player b e)
     (match (pos->xy-pos b padding)
       [(XY-Pos x1 y1)
        (match (pos->xy-pos e padding)
          [(XY-Pos x2 y2)
           (above
            (if (= x1 x2)
                (add-line (board-image (Game-board game) padding)
                          (+ x1 (/ padding 2)) y1
                          (+ x2 (/ padding 2)) (- y2 padding) "red")
                (if (= y1 y2)
                    (add-line (board-image (Game-board game) padding)
                              x1 (- y1 (/ padding 2))
                              (+ x2 padding) (- y2 (/ padding 2)) "red")
                    (if (and (< x1 x2) (> y1 y2))
                        (add-line (board-image (Game-board game) padding)
                                  x1 y1
                                  (+ x2 padding)  (- y2 padding) "red")
                        (add-line (board-image (Game-board game) padding)
                                  x1 (- y1 padding) (+ x2 padding) y2 "red")))) 
            (overlay (above
                      (text "The winner is" 16 "black")
                      (text (if (symbol=? player 'white)
                                (if (symbol? p2)
                                    (symbol->string p2)
                                    p2)
                                (if (symbol? p1)
                                    (symbol->string p1)
                                    p1)) 16 "black"))
                     (rectangle (* padding 7) padding "outline" "black")))
           ])])]))

(game-image (Game (Board (list
                          (Stack 0 '())
                          (Stack 1 (list 'black))
                          (Stack 2 (list 'black 'white))
                          (Stack 3 (list 'black 'white 'white))
                          (Stack 4 (list 'black 'white 'white 'black))
                          (Stack 5 (list 'black 'white 'white 'white 'white))
                          (Stack 6 (list 'black 'black 'black 'black
                                         'black 'white)))) 'white) 80 "tim"
                                                                   'black
                                                                   "None")
(game-image (Game (Board (list
                          (Stack 0 '())
                          (Stack 1 (list 'black))
                          (Stack 0 '())
                          (Stack 1 (list 'white))
                          (Stack 0 '())
                          (Stack 0 '())
                          (Stack 0 '()))) 'black) 80 "tim" 'black "None")  
(game-image (Game (Board (list
                          (Stack 0 '())
                          (Stack 1 (list 'black))
                          (Stack 1 (list 'black ))
                          (Stack 1 (list 'black))
                          (Stack 1 (list 'black))
                          (Stack 0 '())
                          (Stack 0 '()))) 'white) 80 "tim" 'black "None")  
(game-image (Game (Board (list (Stack 0 '())
                               (Stack 0 '())
                               (Stack 1 (list 'white))
                               (Stack 2 (list 'white 'black))
                               (Stack 3 (list 'white 'black 'black))
                               (Stack 4 (list 'white 'black 'black 'black))
                               (Stack 0 '()))) 'white) 80 "tim" 'black "None") 
(game-image (Game (Board (list
                          (Stack 4 (list 'black 'black 'black 'white))
                          (Stack 4 (list 'white 'black 'black 'black))
                          (Stack 3 (list 'white 'black 'black))
                          (Stack 2 (list 'white 'black))
                          (Stack 0 '())
                          (Stack 0 '())
                          (Stack 0 '())))  'white) 80 "tim" 'black "None")
(game-image (Game (Board (list
                          (Stack 6 (list 'black 'black 'black 'white
                                         'white 'white))
                          (Stack 6 (list 'white 'white 'white 'black
                                         'black 'black))
                          (Stack 6 (list 'black 'black 'black 'white
                                         'white 'white))
                          (Stack 6 (list 'white 'white 'white 'black
                                         'black 'black))
                          (Stack 6 (list 'black 'black 'black 'white
                                         'white 'white))
                          (Stack 6 (list 'white 'white 'white 'black
                                         'black 'black))
                          (Stack 6 (list 'black 'black 'black 'white
                                         'white 'white)))) 'white) 80 "Andrew"
                                                                   'black
                                                                   "None")


#|~~~~~~~~~~~~~~~~~~~~~~~~ Phase 2 ~~~~~~~~~~~~~~~~~~~~~~~~|#

(define-type Strategy (Game -> Integer))

(: always-choose-center : Strategy)
;; Strategy for an AI where it always picks the same column to play in for
;; every turn  
(define (always-choose-center game)
  3)

(: always-choose : Integer -> Strategy)
;; Strategy for an AI where it will always play in the same column
(define (always-choose col)
  (lambda ([game : Game]) col))

(: first-available : Strategy)
;; Strategy for an AI where it will pick the the first available column to make
;; a move
(define (first-available game)
  (local
    {(: first-available-aux : Game Integer -> Integer)
     ;; aux funciton for first-available to keep track of the accumulator
     (define (first-available-aux game acc)
       (match game
         [(Game board player)
          (cond
            [(> acc 6) (error "tie game")]
            [(valid-move? game (if (symbol=? player 'black)
                                   'white
                                   'black) acc) acc]
            [else (first-available-aux game (+ acc 1))])]))}
    (first-available-aux game 0)))

(check-expect (first-available new-game) 0) 
(check-error (first-available
              (Game (Board (list
                            (Stack 6 (list 'black 'black 'black 'white
                                           'white 'white))
                            (Stack 6 (list 'white 'white 'white 'black
                                           'black 'black))
                            (Stack 6 (list 'black 'black 'black 'white
                                           'white 'white))
                            (Stack 6 (list 'white 'white 'white 'black
                                           'black 'black))
                            (Stack 6 (list 'black 'black 'black 'white
                                           'white 'white))
                            (Stack 6 (list 'white 'white 'white 'black
                                           'black 'black))
                            (Stack 6 (list 'black 'black 'black 'white
                                           'white 'white)))) 'white))
             "tie game")
(check-expect (first-available
               (Game (Board (list
                             (Stack 6 (list 'black 'black 'black 'white
                                            'white 'white))
                             (Stack 6 (list 'white 'white 'white 'black
                                            'black 'black))
                             (Stack 6 (list 'black 'black 'black 'white
                                            'white 'white))
                             (Stack 6 (list 'white 'white 'white 'black
                                            'black 'black))
                             (Stack 6 (list 'black 'black 'black 'white
                                            'white 'white))
                             (Stack 0 '())
                             (Stack 0 '()))) 'white)) 5)
(define-struct Human
  ([name : (U String Symbol)]))

(define-struct Bot
  ([name : (U String Symbol)]
   [strategy : Strategy]))

(define-type Controller (U Human Bot))

(define-struct World
  ([p1 : Controller]
   [p2 : Controller]
   [game : Game]
   [padding : Integer]
   [error : String])) 
  
(: react-to-mouse : World Integer Integer Mouse-Event -> World)
;; updates the game based on a move made by a human controller
(define (react-to-mouse world x y event)
  (match world
    [(World p1 p2 game padding error)
     (match event
       ["button-down"
        (match* (p1 p2)
          [((Bot n1 _) (Bot n2 _)) world]
          [(_ _)
           (cond
             [(and (< (exact-floor (/ x padding)) 7)
                   (<= y (* padding 6)))
              (match game
                [(Game board next)
                 (match
                     (list-ref (Board-stacks board) (exact-floor (/ x padding)))
                   [(Stack h lst)
                    (if (= h 6)
                        (World p1 p2 game padding "Column is full")
                        (World p1 p2 (apply-move game
                                                 (if (symbol=? next 'black)
                                                     'white
                                                     'black)
                                                 (exact-floor
                                                  (/ x padding)))
                               padding "None"))])])]
             [else world])])]
       [_
        (match (outcome game)
          [(Winning-Line p b e) world]
          ['tie world]
          ['ongoing
           (match game
             [(Game board next)
              (if (symbol=? next 'black)
                  (match p2
                    [(Human _) world]
                    [(Bot _ strat) (World p1 p2
                                          (apply-move game 'white (strat game))
                                          padding "None")])
                  (match p1
                    [(Human _) world]
                    [(Bot _ strat) (World p1 p2
                                          (apply-move game 'black (strat game))
                                          padding "Error")]))])])])]))
(check-expect (react-to-mouse (World (Human "Tim") (Human "Arthur") new-game 80
                                     "None")
                              40 40 "button-down") (World
                                                    (Human "Tim")
                                                    (Human "Arthur")
                                                    (Game
                                                     (Board
                                                      (list
                                                       (Stack 1 '(black))
                                                       (Stack 0 '())
                                                       (Stack 0 '())
                                                       (Stack 0 '())
                                                       (Stack 0 '())
                                                       (Stack 0 '())
                                                       (Stack 0 '())))
                                                     'black)
                                                    80
                                                    "None"))
(check-expect (react-to-mouse (World (Human "Tim") (Human "Arthur") new-game 80
                                     "None")
                              120 40 "button-down") (World
                                                     (Human "Tim")
                                                     (Human "Arthur")
                                                     (Game
                                                      (Board
                                                       (list
                                                        (Stack 0 '())
                                                        (Stack 1 '(black))
                                                        (Stack 0 '())
                                                        (Stack 0 '())
                                                        (Stack 0 '())
                                                        (Stack 0 '())
                                                        (Stack 0 '())))
                                                      'black)
                                                     80 "None"))


(: draw-world : World -> Image)
;; takes in a world and produces an image of that world
(define (draw-world world)
  (match world
    [(World p1 p2 game padding error)
     (match* (p1 p2)
       [((Human n1) (Human n2)) (game-image game padding n1 n2 error)]
       [((Human n1) (Bot n2 _)) (game-image game padding n1 n2 error)]
       [((Bot n1 _) (Human n2)) (game-image game padding n1 n2 error)]
       [((Bot n1 _) (Bot n2 _)) (game-image game padding n1 n2 error)])]))

(draw-world (World (Human "Tim") (Human "Arthur") new-game 80 "None"))
(draw-world (World (Bot "Bot" first-available) (Human "Arthur") new-game
                   80 "None"))
(check-expect (image-height (draw-world (World (Human "Tim") (Human "Arthur")
                                               new-game 80 "None"))) 560)
(check-expect (image-width (draw-world (World (Human "Tim") (Human "Arthur")
                                              new-game 80 "None"))) 560)

(: play : Controller Controller Integer -> World)
;; big-bang interactive line of 4
(define (play p1 p2 padding)
  (big-bang (World p1 p2 new-game padding "None") : World
    [to-draw draw-world]
    [on-mouse react-to-mouse])) 

#|~~~~~~~~~~~~~~~~~~~~~~~~ Phase 3 ~~~~~~~~~~~~~~~~~~~~~~~~|#

(define-type Heuristic (Game -> Integer))

(: did-I-win? : Heuristic)
;; if black has a winning line, this function will output the integer 1 or
;; if white has a winning line, it will output the integer -1
(define (did-I-win? game)
  (match (outcome game)
    [(Winning-Line 'black _ _) 1]
    [(Winning-Line 'white _ _) -1]
    [_ 0]))

(: apply-move-hover : Game Player Integer -> Game)
;; check if the move is valid, if it is, apply move to board. No player switch.
;; Else raise an error
(define (apply-move-hover game player column)
  (if (valid-move? game player column)
      (match game
        [(Game board next)
         (match board
           [(Board stacks)
            (local
              {(: apply-move-hover-aux : (Listof Stack) Integer Integer Player
                  -> (Listof Stack))
               ;; Take in stack and outputs new stack given the addition
               ;; by a player
               (define (apply-move-hover-aux stack column acc player)
                 (match stack
                   [(cons (Stack h lst) rest)
                    (if (= column acc)
                        (cons (Stack (+ h 1) (append (list player) lst)) rest)
                        (cons (Stack h lst) (apply-move-hover-aux rest column
                                                            (+ acc 1) player)))]
                   ['() '()]))}
              (Game (Board (apply-move-hover-aux stacks column 0 player))
                    (if (symbol=? next 'white)
                        'white
                        'black)))])])
      (error "not a valid move")))
(check-expect (apply-move-hover new-game 'black 0) (Game
                                              (Board
                                               (list
                                                (Stack 1 '(black))
                                                (Stack 0 '())
                                                (Stack 0 '())
                                                (Stack 0 '())
                                                (Stack 0 '())
                                                (Stack 0 '())
                                                (Stack 0 '())))
                                              'white))
(check-expect (apply-move-hover (Game
                           (Board
                            (list
                             (Stack 1 (list 'black))
                             (Stack 0 '())
                             (Stack 0 '())
                             (Stack 0 '())
                             (Stack 0 '())
                             (Stack 0 '())
                             (Stack 0 '())))
                           'black)
                          'white 0)
              (Game
               (Board
                (list
                 (Stack 2 (list 'white  'black))
                 (Stack 0 '())
                 (Stack 0 '())
                 (Stack 0 '())
                 (Stack 0 '())
                 (Stack 0 '())
                 (Stack 0 '())))
               'black))

(: count-winning-positions : Heuristic)
;; calculate the number of empty spaces that would give a victory for black,
;; minus the number of empty spaces that would give a victory to white.
;; If player black has already won the game, return the integer 1000. If player
;; white has won the game, return the integer -1000
(define (count-winning-positions game1)
  (match (outcome game1)
    [(Winning-Line 'black start end) 1000]
    [(Winning-Line 'white start end) -1000]
    [_
(local
    {(: count-winning-aux : Game Integer -> Integer)
     ;; aux function to keep track of added pieces throughout the board and
     ;; returns the outcomes
     (define (count-winning-aux g column)
       (cond
         [(> column 6) 0]
         [(= (Stack-height
              (list-ref (Board-stacks (Game-board g)) column)) 6)
          (count-winning-aux g (+ 1 column))]
         [else (match (outcome (apply-move-hover g (Game-next g) column))
                 [(Winning-Line player start end)
                  (+ 1 (count-winning-aux
                        (apply-move-hover g 'hover column) column))]
                 [_ (count-winning-aux
                     (apply-move-hover g 'hover column) column)])]))}
    (- (count-winning-aux (Game (Game-board game1) 'black) 0)
       (count-winning-aux (Game (Game-board game1) 'white) 0)))]))

(check-expect (count-winning-positions new-game) 0)
(check-expect (count-winning-positions(Game (Board (list
              (Stack 1 (list 'white))
              (Stack 0 '())
              (Stack 1 (list 'white))
              (Stack 6 (list 'black 'white 'black 'black 'black 'white))
              (Stack 5 (list 'black 'white 'white 'black 'white))
              (Stack 4 (list 'black 'white 'black 'black))
              (Stack 0 '()))) 'white)) 1)
(check-expect (count-winning-positions (Game (Board (list
              (Stack 1 (list 'black))
              (Stack 1 (list 'black))
              (Stack 1 (list 'black))
              (Stack 0 '())
              (Stack 0 '())
              (Stack 0 '())
              (Stack 0 '()))) 'white)) 1)
(check-expect (count-winning-positions (Game (Board (list
              (Stack 1 (list 'white))
              (Stack 1 (list 'white))
              (Stack 1 (list 'white))
              (Stack 0 '())
              (Stack 0 '())
              (Stack 0 '())
              (Stack 0 '()))) 'white)) -1)
(board-image (Board (list
              (Stack 1 (list 'white))
              (Stack 0 '())
              (Stack 1 (list 'white))
              (Stack 6 (list 'black 'white 'black 'black 'black 'white))
              (Stack 5 (list 'black 'white 'white 'black 'white))
              (Stack 4 (list 'black 'white 'black 'black))
              (Stack 0 '()))) 80)
(check-expect (count-winning-positions (Game (Board (list
                          (Stack 4 (list 'black 'black 'black 'white))
                          (Stack 4 (list 'white 'black 'black 'black))
                          (Stack 3 (list 'white 'black 'black))
                          (Stack 2 (list 'white 'black))
                          (Stack 0 '())
                          (Stack 0 '())
                          (Stack 0 '()))) 'white)) 1000)


(define-struct Tree
  ([game : Game]
   [column : Integer]
   [children : (Listof Tree)]))


(: make-children : Tree Integer -> (Listof Tree))
(define (make-children tree column)
  (cond
    [(> column 6) '()]
    [else (match tree
            [(Tree game col children)
             (match game
               [(Game board next) 
                (if (valid-move? game next column)
                    (cons (Tree (apply-move game next column) column '())
                          (make-children tree (+ 1 column)))
                    (make-children tree (+ 1 column)))])])]))

(check-expect (make-children (Tree new-game -1 '()) 0)
              (list (Tree (Game (Board (list (Stack 1 '(white))
                                             (Stack 0 '())
                                             (Stack 0 '())
                                             (Stack 0 '())
                                             (Stack 0 '())
                                             (Stack 0 '())
                                             (Stack 0 '()))) 'black) 0 '())
                    (Tree (Game (Board (list (Stack 0 '())
                                             (Stack 1 '(white))
                                             (Stack 0 '())
                                             (Stack 0 '())
                                             (Stack 0 '())
                                             (Stack 0 '())
                                             (Stack 0 '()))) 'black) 1 '())
                    (Tree (Game (Board (list (Stack 0 '())
                                             (Stack 0 '())
                                             (Stack 1 '(white))
                                             (Stack 0 '())
                                             (Stack 0 '())
                                             (Stack 0 '())
                                             (Stack 0 '()))) 'black) 2 '())
                    (Tree (Game (Board (list (Stack 0 '())
                                             (Stack 0 '())
                                             (Stack 0 '())
                                             (Stack 1 '(white))
                                             (Stack 0 '())
                                             (Stack 0 '())
                                             (Stack 0 '()))) 'black) 3 '())
                    (Tree (Game (Board (list (Stack 0 '())
                                             (Stack 0 '())
                                             (Stack 0 '())
                                             (Stack 0 '())
                                             (Stack 1 '(white))
                                             (Stack 0 '())
                                             (Stack 0 '()))) 'black) 4 '())
                    (Tree (Game (Board (list (Stack 0 '())
                                             (Stack 0 '())
                                             (Stack 0 '())
                                             (Stack 0 '())
                                             (Stack 0 '())
                                             (Stack 1 '(white))
                                             (Stack 0 '()))) 'black) 5 '())
                    (Tree (Game (Board (list (Stack 0 '())
                                             (Stack 0 '())
                                             (Stack 0 '())
                                             (Stack 0 '())
                                             (Stack 0 '())
                                             (Stack 0 '())
                                             (Stack 1 '(white))))
                                'black) 6 '())))


(: build-tree : Tree Integer Integer -> Tree)
;; builds a tree with children
(define (build-tree tree ply acc)
    (if (= acc ply)
        tree
        (match tree
          [(Tree game column '())
           (Tree game column
                 (map
            (lambda ([t : Tree]) (build-tree t ply (+ acc 1)))
            (make-children tree 0)))])))

(check-expect (build-tree (Tree new-game 0 '()) 0 0) (Tree
                                                      (Game
                                                       (Board
                                                        (list
                                                         (Stack 0 '())
                                                         (Stack 0 '())
                                                         (Stack 0 '())
                                                         (Stack 0 '())
                                                         (Stack 0 '())
                                                         (Stack 0 '())
                                                         (Stack 0 '())))
                                                       'white)
                                                      0
                                                      '()))
(check-expect (build-tree (Tree new-game 0 '()) 1 0) (Tree
 (Game
  (Board
   (list
    (Stack 0 '())
    (Stack 0 '())
    (Stack 0 '())
    (Stack 0 '())
    (Stack 0 '())
    (Stack 0 '())
    (Stack 0 '())))
  'white)
 0
 (list
  (Tree
   (Game
    (Board
     (list
      (Stack 1 '(white))
      (Stack 0 '())
      (Stack 0 '())
      (Stack 0 '())
      (Stack 0 '())
      (Stack 0 '())
      (Stack 0 '())))
    'black)
   0
   '())
  (Tree
   (Game
    (Board
     (list
      (Stack 0 '())
      (Stack 1 '(white))
      (Stack 0 '())
      (Stack 0 '())
      (Stack 0 '())
      (Stack 0 '())
      (Stack 0 '())))
    'black)
   1
   '())
  (Tree
   (Game
    (Board
     (list
      (Stack 0 '())
      (Stack 0 '())
      (Stack 1 '(white))
      (Stack 0 '())
      (Stack 0 '())
      (Stack 0 '())
      (Stack 0 '())))
    'black)
   2
   '())
  (Tree
   (Game
    (Board
     (list
      (Stack 0 '())
      (Stack 0 '())
      (Stack 0 '())
      (Stack 1 '(white))
      (Stack 0 '())
      (Stack 0 '())
      (Stack 0 '())))
    'black)
   3
   '())
  (Tree
   (Game
    (Board
     (list
      (Stack 0 '())
      (Stack 0 '())
      (Stack 0 '())
      (Stack 0 '())
      (Stack 1 '(white))
      (Stack 0 '())
      (Stack 0 '())))
    'black)
   4
   '())
  (Tree
   (Game
    (Board
     (list
      (Stack 0 '())
      (Stack 0 '())
      (Stack 0 '())
      (Stack 0 '())
      (Stack 0 '())
      (Stack 1 '(white))
      (Stack 0 '())))
    'black)
   5
   '())
  (Tree
   (Game
    (Board
     (list
      (Stack 0 '())
      (Stack 0 '())
      (Stack 0 '())
      (Stack 0 '())
      (Stack 0 '())
      (Stack 0 '())
      (Stack 1 '(white))))
    'black)
   6
   '()))))

(: max-list : (Listof Integer) -> Integer)
;; determine the max integer from a list of integers
(define (max-list lst)
  (match lst
    [(list val) val]
    [(cons x xr)
     (max x (max-list xr))]))

(check-expect (max-list (list 1 4)) 4)


(: min-list :(Listof Integer) -> Integer)
;; determine the max integer from a list of integers
(define (min-list lst)
  (match lst
    [(list val) val]
    [(cons x xr)
     (min x (min-list xr))]))

(check-expect (min-list (list 1 4)) 1)


(: minimax-eval : Heuristic Integer Game -> Integer)
;;This function takes in a heuristic function, the ply
;; (that is, the number of moves to look ahead),
;; and a game state, and assigns a score using the given heuristic function.
(define (minimax-eval heuristic ply game)
  (local
    {(: minimax-max : Tree -> Integer)
     ;; determine the max heuristic score for black player
     (define (minimax-max t)
        (match t
          [(Tree game column '()) (heuristic game)]
          [(Tree game column children) (max-list (map minimax-max children))]))
     (: minimax-min : Tree -> Integer)
     ;; determine the min heuristic score for white player
     (define (minimax-min t)
        (match t
          [(Tree game column '()) (heuristic game)]
          [(Tree game column children) (min-list (map minimax-min children))]))}
    (match game
      [(Game board 'black) (minimax-max (build-tree (Tree game 0 '()) ply 0))]
      [(Game board 'white) (minimax-min (build-tree (Tree game 0 '())
                                                    ply 0))])))

(check-expect (minimax-eval count-winning-positions 1 new-game) 0)
(check-expect (minimax-eval count-winning-positions 1 (Game (Board (list
              (Stack 1 (list 'white))
              (Stack 0 '())
              (Stack 1 (list 'white))
              (Stack 6 (list 'black 'white 'black 'black 'black 'white))
              (Stack 5 (list 'black 'white 'white 'black 'white))
              (Stack 4 (list 'black 'white 'black 'black))
              (Stack 0 '()))) 'black)) 1000)
(check-expect (minimax-eval count-winning-positions 1 (Game (Board (list
              (Stack 1 (list 'white))
              (Stack 0 '())
              (Stack 1 (list 'white))
              (Stack 6 (list 'black 'white 'black 'black 'black 'white))
              (Stack 5 (list 'black 'white 'white 'black 'white))
              (Stack 4 (list 'black 'white 'black 'black))
              (Stack 0 '()))) 'white)) -1000)

(: max-index : (Listof Integer) -> Integer)
;; given a list of integers and an index, return a integer for the index
;; of the max
(define (max-index list1)
  (local
    {(: max-index-aux : (Listof Integer) Integer -> Integer)
     ;; aux function that keeps track of the index
     (define (max-index-aux lst index)
       (match lst
         [(list val) index]
         [(cons x xr)
          (if (> x (max-list xr))
              index
              (max-index-aux xr (+ index 1)))]))}
    (max-index-aux list1 0)))

(check-expect (max-index (list 1 3 4 3)) 2) 

(: min-index : (Listof Integer) -> Integer)
;; given a list of integers and an index, return a integer for the index
;; of the min
(define (min-index list1)
  (local
    {(: min-index-aux : (Listof Integer) Integer -> Integer)
     ;; aux function that keeps track of the index
     (define (min-index-aux lst index)
       (match lst
         [(list val) index]
         [(cons x xr)
          (if (< x (min-list xr))
              index
              (min-index-aux xr (+ index 1)))]))}
    (min-index-aux list1 0)))

(check-expect (min-index (list 1 3 4 3)) 0) 

(: make-minimax-strategy : Heuristic Integer -> Strategy)
;; builds a strategy out of a Heuristic and a ply
(define (make-minimax-strategy heuristic ply)
  (local
    {(: make-minimax-strategy-aux : Game -> Integer)
     ;; aux function to take in a game
     (define (make-minimax-strategy-aux g)
       (match (outcome g)
         [(Winning-Line player start end) (error "game already won")]
         ['tie (error "game is tied")]
         ['ongoing
          (local
            {(define children (make-children (Tree g 0 '()) 0))}
            (match g
              [(Game board 'white)
               (Tree-column (list-ref children (min-index (map
                           (lambda ([t : Tree])
                                   (minimax-eval heuristic (- ply 1)
                                                 (Tree-game t))) children))))]
              [(Game board 'black)
               (Tree-column (list-ref children (max-index (map
                           (lambda ([t : Tree])
                                   (minimax-eval heuristic (- ply 1)
                                                 (Tree-game t)))
                           children))))]))]))}
         make-minimax-strategy-aux))


  
(test)
