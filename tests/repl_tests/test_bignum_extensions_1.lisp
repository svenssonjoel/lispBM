


(hide-trapped-error)


;; ------------------- t1

(define bytes [0x00 0x00 0x00 0x01])                                          
(define bn (bn-from-bytes bytes))                                             
(define bytes1 (bn-to-bytes bn))

(define t1 (eq bytes1 bytes))


;; -------------------- t2
(define a (bn-from-bytes [0x00 0x00 0x00 0x01])) 
(define b (bn-from-bytes [0xFF 0xFF 0xFF 0xFF]))                              
(define e (bn-to-bytes (bn-add b a)))                                         
(define t2 (eq e [0x00 0x00 0x00 0x01 0x00 0x00 0x00 0x00]))

;; ---------------------- t3

(define a (bn-from-u32 10))
(define b (bn-from-u32 102))
(define c (bn-add a b))
(define d (bn-to-u32 c))
(define t3 (= 112 d))


;; ---------------------- t4

(define d (bn-cmp a b))
(define e (bn-cmp b a))
(define f (bn-cmp a a))
(define g (bn-cmp b b))

(define t4 (and (= d -1)
                (= e 1)
                (= f 0)
                (= g 0)))


(if (and t1 t2 t3 t4)
    (print "SUCCESS")
  (print "FAILURE"))
    
