(define f @const (closure (x y) (+ x y) nil))

(define a @const '(1u32 2u32 3u32 4u32))

@const-start
(eq '(2u32 3u32 4u32 5u32) (map (f 1) a))
@const-end
