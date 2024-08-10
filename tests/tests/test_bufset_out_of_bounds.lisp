
(define b (bufcreate 10))

(define r1 (not (bufset-i8 b 10 1)))
(define r2 (not (bufset-i16 b 10 1)))
(define r3 (not (bufset-i32 b 10 1)))

(define r4 (not (bufset-u8 b 10 1)))
(define r5 (not (bufset-u16 b 10 1)))
(define r6 (not (bufset-u24 b 10 1)))
(define r7 (not (bufset-u32 b 10 1)))

(define r8 (not (bufset-f32 b 10 1.0)))


(check (and r1 r2 r3 r4 r5 r6 r7 r8))

