#lang racket

(define (make-2d-vector r c val)
  (build-vector r 
                (lambda(x) (make-vector c val))))

(define (2d-vector-ref vec r c)
  (vector-ref (vector-ref vec r) c))

(define (2d-vector-set! vec r c val)
  (let ((v (vector-ref vec r)))
    (begin
      (vector-set! v c val)
      (vector-set! vec r v))))

(define (2d-vector-copy grid)
  (define temp (make-2d-vector 6 7 #\#))
  (define (helper n)
    (if(< n 6)
       (let ((v (vector-ref grid n)))
         (begin
           (vector-set! temp n (vector-copy v))
           (helper (+ n 1))))
       temp))
  (helper 0))

(define (dropper columnnumber whose-turn grid)
  (define (checker j)
    (if(and (< (+ j 1) 6) (= (2d-vector-ref grid (+ j 1) (- columnnumber 1)) 0))
       (checker (+ j 1))
       (2d-vector-set! grid j (- columnnumber 1) whose-turn)))
  (if(not (= (2d-vector-ref grid 0 (- columnnumber 1)) 0))
     "column is full"
     (begin
       (checker 0)
       ;(display grid)
       ;(display #\newline))))
)))
(define (player1 column-number grid)
  (dropper column-number 1 grid))

(define (player2 column-number grid)
  (dropper column-number 2 grid))

(define v1 (vector (cons 5 0) (cons 4 0) (cons 3 0) (cons 2 0) (cons 1 0) (cons 0 0)
                   (cons 0 1) (cons 0 2) (cons 0 3) (cons 0 4) (cons 0 5) (cons 0 6) (cons 0 7)))
(define v2 (vector (cons 0 0) (cons 0 1) (cons 0 2) (cons 0 3) (cons 0 4) (cons 0 5)
                   (cons 0 6) (cons 1 6) (cons 2 6) (cons 3 6) (cons 4 6) (cons 5 6) (cons 6 6)))

(define (evaluator board)
  (define staticvalue 0)
  (define (threat-type row column)
    (define (threat-helper r c count)
      (if(= r 6) (if(= (modulo count 2) 0) 'even 'odd)
         (if(= (2d-vector-ref board r c) 0)
            (threat-helper (+ r 1) c (+ count 1))
            (if(= (modulo count 2) 0) 'even 'odd))))
    (threat-helper (+ row 1) column 1))
  (define (value-for-three ri ci rf cf start end coin)
    (define value 0)
    (define start-threat (if(equal? start #f) #f (threat-type ri ci)))
    (define end-threat (if(equal? end #f) #f (threat-type rf cf)))
    (begin
      (if(equal? start-threat #f)(set! value value)
         (if(equal? start-threat 'even) (if(= coin 1) (set! value 16)
                                           (set! value 32))
            (if(= coin 1) (set! value 32) (set! value 16))))
      (if(equal? end-threat #f)(set! value value)
         (if(equal? end-threat 'even) (if(= coin 1) (set! value 16)
                                         (set! value 32))
            (if(= coin 1) (set! value 32) (set! value 16))))
      value))
  (define (row-wanderer row column start)
    (define (row-helper r c count start presentcoin)
      (define (row-hof f g truth)
        (cond
          ((= count 1) (if start
                           (list (if truth 2 1) (f r 1) (g c 1) truth)
                           (list (if truth 1 0) (f r 1) (g c 1) truth)))
          ((= count 2) (if start
                           (list (if truth 8 4) (f r 1) (g c 1) truth)
                           (list (if truth 4 0) (f r 1) (g c 1) truth)))
          ((= count 3) (list (value-for-three r (- c 4) r c start truth presentcoin) (f r 1) (g c 1) truth))
          (else (list 1000 (f r 1) (g c 1) truth))))
      (cond
        ((= c 7) (row-hof + zero #f)) 
        ((= (2d-vector-ref board r c) 0) (row-hof same + #t))
        ((= (2d-vector-ref board r c) presentcoin) (row-helper r (+ c 1) (+ count 1) start presentcoin))
        (else (row-hof same same #f))))    
    (cond
      ((or (= row 6) (and (= row 5) (= column 7))) staticvalue)
      ((= column 7) (row-wanderer (+ row 1) 0 #f))
      ((= (2d-vector-ref board row column) 0) (row-wanderer row (+ column 1) #t))
      ((= (2d-vector-ref board row column) 1) (begin
                                                (define l (row-helper row (+ column 1) 1 start 1))
                                                (set! staticvalue (- staticvalue 
                                                                     (car l)))
                                                (row-wanderer (list-ref l 1) (list-ref l 2) (list-ref l 3))))
      ((= (2d-vector-ref board row column) 2) (begin
                                                (define l (row-helper row (+ column 1) 1 start 2))
                                                (set! staticvalue (+ staticvalue 
                                                                     (car l)))
                                                (row-wanderer (list-ref l 1) (list-ref l 2) (list-ref l 3))))))
  (define (column-wanderer row column)
    (define (column-helper r c count presentcoin)
      (define column-value (cond
                             ((= count 1) 1)
                             ((= count 2) 4)
                             ((= count 3) 32)
                             (else 1000)))
      (cond
        ((= r 6) column-value)
        ((= (2d-vector-ref board r c) presentcoin)
         (column-helper (+ r 1) c (+ count 1) presentcoin))
        (else column-value)
        ))
    (cond
      ((or (= column 7) (and (= row 6) (= column 6))) staticvalue)
      ((= row 6) (column-wanderer 0 (+ column 1)))
      ((and (= row 0) (not (= (2d-vector-ref board row column) 0))) (column-wanderer 0 (+ column 1)))
      ((= (2d-vector-ref board row column) 0) (column-wanderer (+ row 1) column))
      ((= (2d-vector-ref board row column) 1) (begin
                                                (set! staticvalue (- staticvalue
                                                                     (column-helper (+ row 1) column 1 1)))
                                                (column-wanderer 0 (+ column 1))))
      ((= (2d-vector-ref board row column) 2) (begin
                                                (set! staticvalue (+ staticvalue
                                                                     (column-helper (+ row 1) column 1 2)))
                                                (column-wanderer 0 (+ column 1))))))
  (define (diagonal-wanderer row column start var vec f g fr fc)
    (define (car-vec x a) (car (vector-ref vec (+ var 1))))
    (define (cdr-vec y b) (cdr (vector-ref vec (+ var 1))))
    (define (diagonal-helper r c count start presentcoin)
      (define (list-hof h j truth k)
        (cond
          ((= count 1) (if start (list (if truth 2 1) (h r 1) (j c 1) truth (k var 1))
                           (list (if truth 1 0) (h r 1) (j c 1) truth (k var 1))))
          ((= count 2) (if start (list (if truth 8 4) (h r 1) (j c 1) truth (k var 1))
                           (list (if truth 4 0) (h r 1) (j c 1) truth (k var 1))))
          ((= count 3) (list (value-for-three ((if(equal? f +) - +) r 4) ((if(equal? g +) - +) c 4) r c start truth presentcoin)
                             (h r 1) (j c 1) truth (k var 1)))
          (else (list 1000 (h r 1) (j c 1) truth (k var 1)))))       
      (cond
        ((or (= r fr) (= c fc)) (list-hof car-vec cdr-vec #f +))
        ((= (2d-vector-ref board r c) 0) (list-hof f g #t same))
        ((= (2d-vector-ref board r c) presentcoin) (diagonal-helper (f r 1) (g c 1) (+ count 1) start presentcoin))
        (else (list-hof same same #f same))))  
    (cond
      ((= var 12) staticvalue)
      ((or (= row fr) (= column fc)) (diagonal-wanderer (car (vector-ref vec (+ var 1))) (cdr (vector-ref vec (+ var 1)))
                                                          #f (+ var 1) vec f g fr fc))
      ((= (2d-vector-ref board row column) 0) (diagonal-wanderer (f row 1) (g column 1) #t var vec f g fr fc))
      ((= (2d-vector-ref board row column) 1) (begin
                                                (define l (diagonal-helper (f row 1) (g column 1) 1 start 1))
                                                (set! staticvalue (- staticvalue 
                                                                     (car l)))
                                                (diagonal-wanderer (list-ref l 1) (list-ref l 2) 
                                                                     (list-ref l 3) (list-ref l 4) vec f g fr fc)))
      ((= (2d-vector-ref board row column) 2) (begin
                                                (define l (diagonal-helper (f row 1) (g column 1) 1 start 2))
                                                (set! staticvalue (+ staticvalue 
                                                                     (car l)))
                                                (diagonal-wanderer (list-ref l 1) (list-ref l 2) 
                                                                     (list-ref l 3) (list-ref l 4) vec f g fr fc)))))
  
  
  (begin
    (row-wanderer 0 0 #f)
    (diagonal-wanderer 5 0 #f 0 v1 + + 6 7)
    (column-wanderer 0 0)
    (diagonal-wanderer 0 0 #f 0 v2 + - 6 -1)
    staticvalue))
(define (zero x y) 0)
;))



(struct node (val list)#:transparent)

(define (game-end-checker grid)
  (define (end-checker-hof fr fc f g ir ic)
  (define (helper r c)
    (cond
      ((and (= r fr) (= c fc)) '())
      ((if(= fr 3) (= r fr) (= c fc)) (if(= fr 3) (helper 0 (+ c 1)) (helper (+ r 1) 0)))
      (else
       (if(and (not (= (2d-vector-ref grid r c) 0))
               (= (2d-vector-ref grid r c)
                  (2d-vector-ref grid (f r 1) (g c 1))
                  (2d-vector-ref grid (f r 2) (g c 2))
                  (2d-vector-ref grid (f r 3) (g c 3)))
               )
          (list (2d-vector-ref grid r c)
                (cons r c) (cons (f r 1) (g c 1)) (cons (f r 2) (g c 2)) (cons (f r 3) (g c 3)))
          (if(= fr 3) (helper (+ r 1) c) (helper r (+ c 1)))))))
  (helper ir ic))
  (if(null? grid) '()
     (append (end-checker-hof 5 4 same + 0 0)
          (end-checker-hof 3 6 + same 0 0)
          (end-checker-hof 3 6 + - 0 3)
          (end-checker-hof 2 4 + + 0 0))))

(define (same x y) x)


(define (dropper-temp columnnumber whose-turn grid)
  (define temp-grid (2d-vector-copy grid))
  (define (checker j)
    (if(and (< (+ j 1) 6) (= (2d-vector-ref temp-grid (+ j 1) (- columnnumber 1)) 0))
       (checker (+ j 1))
       (2d-vector-set! temp-grid j (- columnnumber 1) whose-turn)))
  (if(not (= (2d-vector-ref temp-grid 0 (- columnnumber 1)) 0))
     '()
     (begin
       (checker 0)
       temp-grid)))

(define (player1-temp column-number grid)
  (dropper-temp column-number 1 grid))

(define (player2-temp column-number grid)
  (dropper-temp column-number 2 grid))


;;This function finds the minimax tree
(define (minimax-tree grid)
  
  (define (next-possible grid given-player count)
    (define next-board 0)
    (define (loop-dropper i)
      (if(< i 8)
         (begin
           (set! next-board (given-player i grid))
           (if(vector? next-board)
              (append (list (node next-board (node-list (all-possible next-board (+ count 1)))))
                      (loop-dropper (+ i 1)))
              (append '(()) (loop-dropper (+ i 1)))))
         '()))
    (loop-dropper 1))
  
  (define (all-possible grid count)
    (cond
      ((= count 5) (node grid (list (node (evaluator grid) '()))))
      ((= (modulo count 2) 1) 
       (node grid (next-possible grid player2-temp count)))
      (else
       (node grid (next-possible grid player1-temp count)))))
  (define t (all-possible grid 1))
  
  t)

(define (max-min-hof l max-min f)
  (if(null? l) (list max-min)
     (if(null? (car l))
        (max-min-hof (cdr l) max-min f)
        (if(f (car l) max-min)
           (max-min-hof (cdr l) (car l) f)
           (max-min-hof (cdr l) max-min f)))))

(define (form-list-hof bl val f)
  (define (value-appender l)
    (if(null? l) '()
       (if(null? (car l))
          (append '(()) (value-appender (cdr l)))
          (if(number? (node-val (car l)))
             (list (node-val (car l)))
             (append (form-list-hof (node-list (car l)) (- 0 val) (if(equal? f >) < >))
                     (value-appender (cdr l)))))))
  (max-min-hof (value-appender bl) val f))

(define (list-of-prob-values tree)
  (define (prob-value l)
    (if(null? l) '()                                       
       (if(null? (car l))
          (append '(()) (prob-value (cdr l)))
          (append (form-list-hof (node-list (car l)) 50000 <)
                  (prob-value (cdr l))))))
  (prob-value (node-list tree)))

;;finds best drop column for the computer

(define (best-drop-column prob-list)
  (define (bdc-helper i lst present-max present-best)
    (if(null? lst) present-best
       (if(null? (car lst))
          (bdc-helper (+ i 1) (cdr lst) present-max present-best)
          (if(> (car lst) present-max)
             (bdc-helper (+ i 1) (cdr lst) (car lst) i)
             (bdc-helper (+ i 1) (cdr lst) present-max present-best)))))
  (bdc-helper 1 prob-list -50000 4))


(provide best-drop-column)
(provide minimax-tree)
(provide list-of-prob-values)
(provide game-end-checker)
(provide 2d-vector-ref)
(provide 2d-vector-set!)
(provide 2d-vector-copy)
(provide player1)
(provide player2)
(provide player1-temp)
(provide player2-temp)