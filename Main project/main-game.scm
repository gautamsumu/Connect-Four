#lang racket/gui

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;     CS-154 PROJECT      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;     "CONNECT FOUR"      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;    VARUN - 100050068    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;  CHANDAN - 100050059    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;   GAUTAM - 100050058    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(require 2htdp/image)
(require rnrs/io/ports-6)
(require "ai.scm")

;;;;;;;;;;;;;;;;;  FILE INPUT - OUTPUT  ;;;;;;;;;;;;;;;;;;;;;;;

(define v (make-vector 11 #\#))
(define u (make-vector 10 #\#))

(define (getvec)
  (begin (define in (open-input-file "text.txt" #:mode 'text))
         (set! v (vector-append (read in) (make-vector 1 #\#)))
         (close-input-port in)))

(define (init)
  (define (helper i)
    (cond((= i 9)(vector-set! u i (vector-ref v i)))
         (else (begin (vector-set! u i (vector-ref v i))
                      (helper (+ i 1))))))
  (helper 0))

(define (chek a)
  (begin (getvec)
         (> a (cdr (vector-ref v 9)))))

(define (findg)
  (define (helper i)
    (cond((= i 11)j)
         (else (let ((val (vector-ref v i)))
                 (if (> (cdr val) (cdar j))(begin (set! j (cons val i))
                                                  
                                                  (helper (+ i 1)))
                     (helper (+ i 1)))))))
  (begin (define j (cons (cons 0 -1) -1))
         (helper 0)))

(define (sort k)
  (define (helper i)
    (let((x (findg)))
      (cond((= i 9)(begin (vector-set! u i (car x))
                          (vector-set! v (cdr x) (cons -1 -1))))
           (else (begin (vector-set! u i (car x))
                        (vector-set! v (cdr x) (cons -1 -1))
                        (helper (+ i 1)))))))
  (begin (vector-set! v 10 k)
         (helper 0)))

(define (ryt a)
  (begin (getvec)
         (sort a)
         (define out (open-output-file "text.txt" #:mode 'text #:exists 'truncate))
         (write u out)
         (close-output-port out)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;   GAME MAP  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-2d-vector r c val)
  (build-vector r 
                (lambda(x) (make-vector c val))))

(define gamegrid (make-2d-vector 6 7 0))

(define number-of-moves-1 0)
(define number-of-moves-2 0)

(define (immediate-checker i column)
  (if(> i 7) column
     (if(null? (game-end-checker (player2-temp i gamegrid)))
        (if(null? (game-end-checker (player1-temp i gamegrid)))
           (immediate-checker (+ i 1) column)
           (immediate-checker (+ i 1) i))
        i)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;    GRAPHICS  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define icon (read-bitmap "blueball.bmp"))
(define blueball (read-bitmap "blue.png"))
(define redball (read-bitmap "red.png"))
(define graph (read-bitmap "graph.png"))
(define human-ai (read-bitmap "single.png"))
(define human-human (read-bitmap "double.png"))
(define newgame (read-bitmap "newgame.png"))
(define screen (read-bitmap "bg1.jpg"))
(define exit (read-bitmap "exit.png"))
(define high (read-bitmap "high.png"))
(define inst (read-bitmap "instructions.png"))
(define simple (read-bitmap "simple.png"))
(define return (read-bitmap "back.png"))
(define gamescreen (read-bitmap "gameback.png"))
(define yes (read-bitmap "yes.png"))
(define no (read-bitmap "no.png"))
(define exitscreen (read-bitmap "exitscreen.png"))
(define instscreen (read-bitmap "instructscreen.png"))
(define sandq (read-bitmap "sandq.png"))
(define cc (read-bitmap  "Cc.png"))
(define o (read-bitmap "Oo.png"))
(define n (read-bitmap "Nn.png"))
(define savedgame (read-bitmap "loadgame.png"))
(define e (read-bitmap "Ee.png"))
(define tt (read-bitmap "Tt.png"))
(define bluewinball (read-bitmap "blueb.png"))
(define redwinball (read-bitmap "redb.png"))
(define f (read-bitmap "Ff.png"))
(define oo (read-bitmap "Ooo.png"))
(define u1 (read-bitmap "Uu.png"))
(define r (read-bitmap "Rr.png"))
(define bg (read-bitmap "bg.png"))
(define p1 (read-bitmap "p1.png"))
(define p2 (read-bitmap "p2.png"))
(define cwi (read-bitmap "cw.png"))
(define cli (read-bitmap "cl.png")) 
(define exit2 (read-bitmap "exit2.png"))
(define submit (read-bitmap "submit.png"))                                                                                        
 
(define x 0)
(define y 0)
(define gamestatus 'welcome)
(define p 0)
(define i 0)
(define a 0)
(define m 0)
(define trash 0)
(define score1 0)
(define score2 0)

(define p1_ball blueball)
(define p2_ball redball)
(define cur_ball 'p1_ball)
(define ball p1_ball)
(define scene (list (list graph 0 0)))

(define game (new frame% [label "Connect four"]
                  [min-width 574]
                  [min-height 700]
                  [stretchable-width #f]
                  [stretchable-height #f]))

(send game set-icon icon)

(define (drop final)
  (define (iter)
    (if (>= p final) (bounce)
        (begin
          (set! p (+ p 5))
          (send back on-paint)
          (sleep/yield .001)
          (iter)))) 
  (iter))

(define (bounce)
  (set! p (- p 10))
  (send back on-paint)
  (sleep/yield .015)
  (set! p (- p 10))
  (send back on-paint)
  (sleep/yield .017)
  (set! p (- p 10))
  (send back on-paint)
  (sleep/yield .02)
  (set! p (- p 5))
  (send back on-paint)
  (sleep/yield .02)
  (set! p (- p 5))
  (send back on-paint)
  (sleep/yield .02)
  (set! p (+ p 5))
  (send back on-paint)
  (sleep/yield .02)
  (set! p (+ p 5))
  (send back on-paint)
  (sleep/yield .02)
  (set! p (+ p 10))
  (send back on-paint)
  (sleep/yield .02)
  (set! p (+ p 10))
  (send back on-paint)
  (sleep/yield .017)
  (set! p (+ p 10))
  (send back on-paint)
  (sleep/yield .015)
     (if(eq? cur_ball 'p1_ball)
        (begin (player1 (+ (/ a 82) 1) gamegrid) (set! number-of-moves-1 (+ number-of-moves-1 1)))
     (begin (player2 (+ (/ a 82) 1) gamegrid) (set! number-of-moves-2 (+ number-of-moves-2 1))))
     
  (if(null? (game-end-checker gamegrid)) #f
     (begin
       (set! ball p2_ball)
           (set! cur_ball 'p2_ball)
           (exit1)))
  (if (eq? cur_ball 'p1_ball)
      (if(eq? gamestatus 'singlegame)
         (begin    
           (set! ball p2_ball)
           (set! cur_ball 'p2_ball)
           (set! a 0)
           (set! p 0)
           (set! m 0)
           (set! m 1)
           (let((best-column (let((temp-col (immediate-checker 1 '())))
                               (if(null? temp-col) (best-drop-column (list-of-prob-values (minimax-tree gamegrid)))
                                  temp-col)))) (set! a (* 82 (- best-column 1))))
           (drop (end-y-finder (/ a 82))))
         (begin       
           (set! ball p2_ball)
           (set! cur_ball 'p2_ball)
           (set! a 0)
           (set! p 0)
           (set! m 0)))
      (begin
        (set! ball p1_ball)
        (set! cur_ball 'p1_ball)
        (set! a 0)
        (set! p 0)
        (set! m 0))))
  
(define (give-y init final)
  (define bounce-vec  (vector final (- final 20) (- final 40) (- final 60) (- final 80) (- final 90) (- final 100) (- final 105) (- final 100) (- final 90) (- final 80) (- final 60) (- final 40) (- final 20)))
  (if (>= (+ init (* i 5)) final) (if (> (floor (/ (+ init (- (* i 5) final)) 5)) 13) final
                                      (vector-ref bounce-vec (floor (/ (+ init (- (* i 5) final)) 5))))
      (+ init (* i 5))))

(define my-canvas% (class canvas%
                     (define/public (welcome)
                      (send back on-paint))
                     
                     (define/override (on-event event)
                       (set! x (send event get-x))
                       (set! y (send event get-y))
                      
                       (cond
                         ((eq? gamestatus 'home)
                          (if (send event button-down?)
                              (cond
                                ((and (> x 192) (< x 366))
                                 (cond
                                   ((and (> y 325) (< y 347)) (begin
                                                              (set! gamestatus 'newgame)
                                                              (send game enable #f)
                                                              (send pop-up on-paint)
                                                              (send over show #t)))
                                   ((and (> y 360) (< y 382)) (begin (loadgame)
                                                                     (send back on-paint)))
                                   ((and (> y 395) (< y 417)) (begin
                                                                (set! gamestatus 'instructions)
                                                                (send back on-paint)))
                                   ((and (> y 430) (< y 452)) (begin
                                                                (set! gamestatus 'highscores)
                                                                (send back on-paint)))
                                   ((and (> y 465) (< y 487)) (begin
                                                                (set! gamestatus 'exit)
                                                                (send game enable #f)
                                                                (send pop-up on-paint)
                                                                (send over show #t))))))
                              #f))

                         ((eq? gamestatus 'newgame)
                          (if (send event button-down?)
                              (cond
                                ((and (> x 112) (< x 287))
                                 (cond
                                   ((and (> y 30) (< y 50)) (begin
                                                              (set! gamestatus 'singlegame)
                                                              (send game enable #t)
                                                              (send over show #f)
                                                              (send back on-paint)))
                                   ((and (> y 75) (< y 95)) (begin
                                                               (set! gamestatus 'doublegame)
                                                               (send game enable #t)
                                                               (send over show #f)
                                                               (send back on-paint)))))
                                 ((and (> x 50) (< x 150) (> y 110) (< y 160)) (begin
                                                                                 (send game enable #t)
                                                                                 (set! gamestatus 'home)
                                                                                 (send over show #f))))
                              #f))
                         
                         ((or (eq? gamestatus 'singlegame) (eq? gamestatus 'doublegame))
                          (if (= m 0)
                              (if (send event button-down?)
                                  (cond
                                    ((< y 640)
                                      (begin
                                        (if(not (= (2d-vector-ref gamegrid 0 (floor (/ x 82))) 0))
                                     #f                                     
                                     (begin
                                       (set! m 1)
                                       (set! a (* 82 (floor (/ x 82))))
                                       (drop (end-y-finder (floor (/ x 82)))))))                                     )
                                      ((and (> y 650) (< y 672) (> x 200) (< x 350))
                                             (begin
                                               (savegame)
                                               (send game show #f))))
                                  (begin
                                    (set! a (- x 41))  (send back on-paint)))
                              #f))
                         
                         ((eq? gamestatus 'highscores)
                          (if (send event button-down?)
                              (if (and (> x 80) (< x 170) (> y 500) (< y 522))
                                  (begin
                                    (set! gamestatus 'welcome)
                                    (welcome-iter)
                                    (send back on-paint))
                                #f)
                              #f))
                         
                         ((eq? gamestatus 'exit)
                          (if (send event button-down?)
                              (cond
                                ((and (> y 200) (< y 222))
                                 (cond
                                   ((and (> x 80) (< x 200)) (begin
                                                               (send over show #f)
                                                               (send game show #f)))
                                   ((and (> x 250) (< x 370)) (begin
                                                                (set! gamestatus 'home)
                                                                (send game enable #t)
                                                                (send over show #f))))))
                              #f))
                         
                         ((eq? gamestatus 'instructions)
                          (cond
                            ((send event button-down?)
                             (cond
                               ((and (> x 80) (< x 170) (> y 500) (< y 522))
                                (begin
                                  (set! gamestatus 'welcome)
                                  (welcome-iter)
                                  (send back on-paint)))))))))
                     
                       (super-new)))

  (define (end-y-finder column)
    (define (y-checker j)
      (if(and (< (+ j 1) 6) (= (2d-vector-ref gamegrid (+ j 1) column) 0))
         (y-checker (+ j 1))
         (* 82 (+ 1 j))))
    (y-checker 0))
  
  (define back (new my-canvas% [parent game]
                    [paint-callback
                     (λ(canvas dc)
                       (cond
                         ((eq? gamestatus 'welcome)
                            (send dc erase)
                            (send dc draw-bitmap-section screen 0 0 250 0 574 700)
                            (send dc draw-bitmap f (give-y -85 120) 200)
                            (send dc draw-bitmap oo (give-y 5 200) 200)
                            (send dc draw-bitmap u1 (- 574 (give-y 85 294)) 200)
                            (send dc draw-bitmap r  (- 574 (give-y -5 214)) 200)
                                  
                          (send dc draw-bitmap cc 1 (give-y -65 100))
                          (send dc draw-bitmap o 78 (give-y -45 100))
                          (send dc draw-bitmap n 154 (give-y -78 100))
                          (send dc draw-bitmap n 230 (give-y -5 100))
                          (send dc draw-bitmap e 306 (give-y -28 100))
                          (send dc draw-bitmap cc 385 (give-y -13 100))
                          (send dc draw-bitmap tt 460 (give-y -65 100))
                          (sleep/yield .03))
                                   
                         ((eq? gamestatus 'home)
                          (send dc draw-bitmap newgame 192 325)
                          (send dc draw-bitmap savedgame 192 360)
                          (send dc draw-bitmap inst 192 395)
                          (send dc draw-bitmap high 192 430)
                          (send dc draw-bitmap exit 192 465))
                          
                         ((eq? gamestatus 'highscores)
                          (begin
                            (send dc erase)
                            (getvec)
                            (init)
                               (send dc draw-bitmap gamescreen 0 0)	 	
                               (send dc set-text-foreground (make-object color% 255 0 0))
                               (send dc set-font (make-object font% 15 'swiss 'normal 'bold #t))
                               (define (helper i)
                                 (cond((= i 9)(begin (send dc draw-text (string-append (number->string (+ i 1))
                                                                                       ".")
                                                           100 (+ (* i 20) 130))
                                                     (send dc draw-text (car (vector-ref u i)) 170 (+ (* i 20) 130))
                                                     (send dc draw-text (number->string (cdr (vector-ref u i)))
                                                           350 (+ (* i 20) 130))))
                                      (else (begin (send dc draw-text (string-append (number->string (+ i 1))
                                                                                     ".")
                                                         100 (+ (* i 20) 130))
                                                   (send dc draw-text (car (vector-ref u i)) 170 (+ (* i 20) 130))
                                                   (send dc draw-text (number->string (cdr (vector-ref u i)))
                                                         350 (+ (* i 20) 130))
                                                   (helper (+ i 1))))))
                               (begin (send dc draw-text "RANK" 100 90)
                                      (send dc draw-text "PLAYER-NAME" 170  90)
                                      (send dc draw-text " SCORE  " 350 90)
                                      (send dc set-text-foreground (make-object color% 0 0 255))
                                      (send dc set-font (make-object font% 10 'swiss 'italic 'bold #f))
                                      (helper 0)
                                      (send dc draw-bitmap return 80 500))))
                         
                         ((eq? gamestatus 'instructions)
                          (send dc erase)
                          (send dc draw-bitmap instscreen 0 0)
                          (send dc draw-bitmap return 80 500))
                         
                         ((or (eq? gamestatus 'singlegame) (eq? gamestatus 'doublegame))
                          (send dc erase)
                          (send dc draw-bitmap gamescreen 0 0)
                          (coins-painter)
                          (send dc draw-bitmap ball a p)
                          (send dc draw-bitmap graph 0 62)
                          (send dc draw-bitmap sandq 200 650))))                       
                     ]))
  
  (define (blink lst)
  (let ((val (car lst)))
    (define (helper i)
      (cond((= i 0)(begin
                     (for-each (lambda(x)(2d-vector-set! gamegrid (car x) (cdr x) (+ val 2)))
                               (cdr lst))
                     (coins-painter)))
           (else (begin
                   (for-each (lambda(x)(2d-vector-set! gamegrid (car x) (cdr x) (+ val 2)))
                             (cdr lst))
                   (coins-painter)
                   (sleep/yield 0.2)
                   (for-each (lambda(x)(2d-vector-set! gamegrid (car x) (cdr x) val))
                             (cdr lst))
                   
                   (coins-painter)
                   (sleep/yield 0.5)
                   (helper (- i 1))))))
    (helper 5)))
  
  (define dc (send back get-dc))
  
  (define (coins-painter)
                          (define (each-coin row column)
                            (begin
                              (cond
                                ((= (2d-vector-ref gamegrid row column) 1)
                                 (send dc draw-bitmap blueball  (* 82 column) (* 82 (+ 1 row))))
                                ((= (2d-vector-ref gamegrid row column) 2)
                                 (send dc draw-bitmap redball  (* 82 column) (* 82 (+ 1 row))))
                                ((= (2d-vector-ref gamegrid row column) 3)
                                 (send dc draw-bitmap bluewinball  (* 82 column) (* 82 (+ 1 row))))
                                ((= (2d-vector-ref gamegrid row column) 4)
                                 (send dc draw-bitmap redwinball  (* 82 column) (* 82 (+ 1 row))))
                                (else (set! trash 1)))
                              (cond
                                ((and (= (+ row 1) 6) (= (+ column 1) 7)) (set! trash 1))
                                ((= (+ column 1) 7) (each-coin (+ row 1) 0))
                                (else (each-coin row (+ column 1))))))
                          (each-coin 0 0))

 (define over (new frame% [label "extra"] 
                  [parent game]
                [min-width 400]	 
 	 	[min-height 250]	 
 	 	[x 300]	 
 	 	[y 300]
                [border 2]
                [style (list 'no-resize-border 'float 'no-caption )]	 
 	 	[stretchable-width #f]
                [stretchable-height #f]))
  
  (define pop-up (new my-canvas%
                      [parent over]
                      [paint-callback
                       (λ(canvas dc)
                         (cond
                            ((eq? gamestatus 'newgame)
                          (send dc erase)
                          (send dc draw-bitmap simple 0 0)
                          (send dc draw-bitmap human-ai 112 30)
                          (send dc draw-bitmap human-human 112 75)
                          (send dc draw-bitmap return 50 110))
                         ((eq? gamestatus 'exit)
                          (send dc erase)
                          (send dc draw-bitmap exitscreen 0 0)
                          (send dc draw-bitmap yes 80 200)
                          (send dc draw-bitmap no 250 200))))]))
  
  (define entername (new frame% [label "Game Over !!!"]
                       [style (list 'no-caption)]
                       [min-width 500]	 
                       [min-height 350]	 
                       [stretchable-width #f]	 
                       [stretchable-height #f]))

  (define enternamecl (new frame% [label "You Won"]
                       [style (list 'no-caption)]
                       [min-width 500]	 
                       [min-height 350]	 
                       [stretchable-width #f]	 
                       [stretchable-height #f]))

(define enternamecw (new frame% [label "Game Over !!!"]
                       [style (list 'no-caption)]
                       [min-width 500]	 
                       [min-height 350]	 
                       [stretchable-width #f]	 
                       [stretchable-height #f]))

(define cw (new canvas% [parent enternamecw]
               [paint-callback (λ(canvas dc)
                           (send dc draw-bitmap cwi 0 0))]))

(define cl (new canvas% [parent enternamecl]
               [paint-callback (λ(canvas dc)
                           (send dc draw-bitmap cli 0 0))]))
                                                                                                
                            

(define c (new canvas% [parent entername]
               [paint-callback (λ(canvas dc)
                           (if(= (car (game-end-checker gamegrid)) 3)
                              (send dc draw-bitmap p1 0 0)
                              (send dc draw-bitmap p2 0 0)))]))
                                           
(define t (new text-field% [label #f]
               [parent entername]))
               
(define tw (new text-field% [label #f]
               [parent enternamecl]))

(define b (new button% [parent entername]
               [label submit]
               [callback (lambda(button event) (begin 
                                                 (ryt (cons (send t get-value) 
                                                            (if(= (car (game-end-checker gamegrid)) 
                                                                  1)score1 score2))) 
                                                 (send t set-value "")
                                                 (send t enable #f) 
                                                 (send b enable #f)))]))

(define bw (new button% [parent enternamecl]
               [label submit]
               [callback (lambda(button event) (begin 
                                                 (ryt (cons (send tw get-value) 
                                                            (if(= (car (game-end-checker gamegrid)) 
                                                                  1)score1 score2)))
                                                 (send t set-value "")
                                                 (send tw enable #f) 
                                                 (send bw enable #f)
                                                 (begin
                                                  (send t enable #t) 
                                                  (send b enable #t)
                                                  (set! gamestatus 'welcome)
                                                  (send game enable #t)
                                                  (send entername show #f)
                                                  (initgame) (savegame)
                                                  (welcome-iter)
                                                  (send back on-paint))))]))


(define (initgame)
  (define (helper r c)
    (cond
      ((= r 6) #f)
      ((= c 7) (helper (+ r 1) 0))
      (else
       (begin
        (2d-vector-set! gamegrid r c 0)
        (helper r (+ c 1))))))
  (helper 0 0))

(define eb (new button% [parent entername]
                [label exit2]
                [callback (lambda(button event) (begin
                                                  (send t enable #t) 
                                                  (send b enable #t)
                                                  (initgame) (savegame)
                                                  (set! gamestatus 'welcome)
                                                  (send game enable #t)
                                                  (send entername show #f)
                                                  
                                                  (welcome-iter)
                                                  (send back on-paint)))]))

(define eb1 (new button% [parent enternamecw]
                [label exit2]
                [callback (lambda(button event) (begin
                                                  (initgame) (savegame)
                                                  (set! gamestatus 'welcome)
                                                  (send game enable #t)
                                                  (send enternamecw show #f)
                                                  (welcome-iter)
                                                  (send back on-paint)))]))

(define eb2 (new button% [parent enternamecl]
                [label exit2]
                [callback (lambda(button event) (begin
                                                  (send tw enable #f) 
                                                  (send bw enable #f)
                                                  (initgame) (savegame)
                                                  (set! gamestatus 'welcome)
                                                  (send game enable #t)
                                                  (send enternamecl show #f)
                                                  (welcome-iter)
                                                  (send back on-paint)))]))

(define (loadgame)
  (begin (define in (open-input-file "savegame.txt" #:mode 'text))
         (let ((temp (read in)))
           (set! gamestatus (vector-ref temp 0))
           (set! cur_ball (vector-ref temp 1))
           (set! gamegrid (vector-ref temp 2)))
         (close-input-port in)))

(define (savegame)
  (define temp (vector gamestatus cur_ball gamegrid))
  (define out (open-output-file "savegame.txt" #:mode 'text #:exists 'truncate))
         (write temp out)
         (close-output-port out))

(define (exit1)
  (begin (blink (game-end-checker gamegrid))
         (set! score1 (floor (/ 420 number-of-moves-1)))
         (set! score2 (floor (/ 420 number-of-moves-2)))
         (if(eq? gamestatus 'singlegame)
            (if(= (car (game-end-checker gamegrid)) 3)(send enternamecl show #t)
               (send enternamecw show #t))
            (send entername show #t)) (send game enable #f)))

(define (welcome-iter)
    (if (> i 80)
      (begin
        (set! i 0)
        (set! gamestatus 'home)
        (send back on-paint))
      (begin
        (set! i (+ i 1))
        (send back on-paint)
        (welcome-iter))))
  
(send game show #t)

(welcome-iter)
  
  