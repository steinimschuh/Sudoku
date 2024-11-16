;
; Sodoku Solver using Scheme's Backtracking Feature
;
(define sodoku-h '())
(define sodoku-v '())
(define sodoku-c '())

(define a-sodoku '())

(define v-h '())
(define v-v '())
(define v-c '())

(define retry '())

(define transpose
    (lambda (x)
        (cond ((null? (car x)) '())
              (else (cons (map car x) (transpose (map cdr x)))))))

(define make-cubes
    (lambda (a b c . z)
        (letrec ((group (lambda (a b c)
                            (cond ((null? a) '())
                                  (else (cons 
                                            (list (car a) (cadr a) (caddr a) (car b) (cadr b) (caddr b) (car c) (cadr c) (caddr c))
                                            (group (list-tail a 3) (list-tail b 3) (list-tail c 3))))))))
            (cond ((null? z) (group a b c))
                  (else (append (group a b c) (apply make-cubes z)))))))

(define strip   
    (lambda (x)
        (cond ((null? x) '())
              ((eq? (car x) '_) (strip (cdr x)))
              (else (cons (car x) (strip (cdr x)))))))

(define missing
    (lambda (x)
        (letrec ((compare (lambda (x y) (cond ((null? x) '())
                                              ((null? y) (cons #t (compare (cdr x) y)))
                                              ((eq? (car x) (car y)) (cons #f (compare (cdr x) (cdr y))))
                                              (else (cons #t ( compare (cdr x) y)))))))
            (compare '(1 2 3 4 5 6 7 8 9) (sort < (strip (apply (lambda x (map car x)) x)))))))

(define candidates
    (lambda (a b c)
        (letrec ((numbers (lambda (n l) (cond ((null? l) '())
                                              (else (if (car l) 
                                                    (cons n (numbers (+ n 1) (cdr l))) 
                                                    (numbers (+ n 1) (cdr l))))))))
            (numbers 1 (map (lambda (a b c) (and a b c)) a b c)))))

(define print-sodoku
    (lambda ()
        (map (lambda (x) (map car x)) sodoku-h)))

(define choose
    (lambda (x)
        (cond ((null? x) '())
        (else (if (call/cc (lambda (k) (set! retry (cons k retry)) (k #t)))
                  (car x)
                  (begin (set! retry (cdr retry))
                         (choose (cdr x))))))))

(define scan-cube
    (lambda (open i j replaced mode level)
        (cond ((eq? i 9)
            (cond ((eq? open 0))
                ((not replaced) (if (< level 2) (scan-cube 0 0 0 #f mode (+ level 1) ) ) )
                (else (scan-cube 0 0 0 #f mode level))))
            ((eq? j 9) (scan-cube open (+ i 1) 0 replaced mode level))
            (else
                (let ((element (vector-ref (vector-ref a-sodoku i) j)))
                    (if (eq? (car element) '_)
                        (let* ((k (+ (* 3 (quotient i 3)) (quotient j 3)))
                                (mi (missing (vector-ref v-h i)))
                                (mj (missing (vector-ref v-v j)))
                                (mc (missing (vector-ref v-c k)))
                                (cl (candidates mi mj mc)))
                (if (null? cl) ((car retry) #f))
                (if (<= (length cl) level)
                    (let ((c (choose cl)))
                        (cond ((null? c) (set-car! element '_) ((car retry) #f))
                                (else (set-car! element c) (set! replaced #t))))
                        (set! open (+ open 1))))))
        (scan-cube open i (+ j 1) replaced mode level)))))

(define solve
    (lambda (sodoku)
        (set! sodoku-h (map (lambda (x) (map list x)) sodoku))
        (set! sodoku-v (transpose sodoku-h))
        (set! sodoku-c (apply make-cubes sodoku-h))
        (set! a-sodoku (list->vector (map list->vector sodoku-h)))
        (set! v-h (list->vector sodoku-h))  
        (set! v-v (list->vector sodoku-v))
        (set! v-c (list->vector sodoku-c))
        (set! retry '())
        (scan-cube 0 0 0 #f 0 1)
        (print-sodoku)))

(define sodoku-1  '((_ 6 _ 9 2 8 _ _ _)
                    (7 8 1 _ _ _ _ _ _)
                    (9 _ _ _ _ _ 8 3 6)
                    (_ 4 2 8 1 9 _ _ _)
                    (_ 7 5 _ _ _ 1 8 _)
                    (_ _ _ 5 7 6 2 4 _)
                    (4 5 8 _ _ _ _ _ 1)
                    (_ _ _ _ _ _ 4 9 2)
                    (_ _ _ 1 4 3 _ 7 _)))

(define sodoku-2  '((_ _ _ 5 _ 1 9 _ 3)
                    (_ 5 4 _ _ 9 _ _ 2)
                    (_ 9 _ _ 2 3 _ _ 6)
                    (_ _ 7 2 4 _ 3 _ _)
                    (4 _ 5 _ _ _ 7 _ 9)
                    (_ _ 8 _ 9 5 1 _ _)
                    (5 _ _ 8 6 _ _ 3 _)
                    (1 _ _ 9 _ _ 4 5 _)
                    (7 _ 3 1 _ 4 _ _ _)))

(define sodoku-3  '((_ _ _ 9 _ _ _ _ _)
                    (_ 6 _ _ 5 1 _ _ _)
                    (1 _ 4 _ _ _ 3 _ 2)
                    (4 _ _ _ _ _ _ _ _)
                    (_ _ 2 _ _ _ _ _ 9)
                    (_ _ _ 3 _ _ 7 2 6)
                    (_ _ 6 _ 1 8 _ _ _)
                    (_ _ _ _ _ 3 2 _ _)
                    (8 7 1 5 _ 9 4 _ _)))

(define sodoku-4  '((_ _ _ _ _ _ _ _ _)
                    (_ _ _ _ _ 5 1 9 3)
                    (_ _ 6 4 _ 3 _ _ _)
                    (_ _ _ _ _ _ _ _ 1)
                    (_ _ _ _ _ _ _ 8 _)
                    (_ 1 _ 8 _ 9 _ 7 6)
                    (4 9 _ 1 _ _ _ 3 _)
                    (_ _ 2 3 _ _ _ _ 8)
                    (8 _ _ 6 _ 4 _ 5 9)))

(define sodoku-5  '((_ _ _ _ 2 _ _ 7 _)
                    (4 _ _ _ 1 8 _ _ 5)
                    (_ 7 3 _ _ _ _ _ 9)
                    (_ _ _ _ _ _ 7 _ _)
                    (_ _ _ _ 8 _ _ 1 _)
                    (_ _ _ _ _ 9 _ _ 3)
                    (_ 9 _ _ _ 5 _ _ _)
                    (8 _ _ 6 _ 1 _ 3 _)
                    (_ _ 2 _ _ _ 4 _ _)))
