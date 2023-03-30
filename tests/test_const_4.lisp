
; "heap"-boxed values
(define a @const-start 1u32 @const-end)
(define b @const-start 2u32 @const-end)


(= (+ a b) 3)
