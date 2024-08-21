
(define eerr '(exit-error eval_error))
(define terr '(exit-error type_error))

(define r1 (eq eerr (trap (str-from-n))))
(define r2 (eq terr (trap (str-from-n "hej"))))

(define r3 (eq eerr (trap (str-join))))
(define r4 (eq terr (trap (str-join "apa" 1))))

(define r5 (eq eerr (trap (str-to-i))))
(define r6 (eq terr (trap (str-to-i 56))))

(define r7 (eq eerr (trap (str-to-f))))
(define r8 (eq terr (trap (str-to-f 3))))

(define r9 (eq terr (trap (str-part))))

(define r10 (eq eerr (trap (str-split))))

(define r11 (eq eerr (trap (str-replace))))

(define r12 (eq eerr (trap (str-to-upper))))
(define r13 (eq eerr (trap (str-to-lower))))

(define r14 (eq eerr (trap (str-cmp))))

(define r15 (eq eerr (trap (str-len))))
(define r16 (eq terr (trap (str-len 2))))

(check (and r1 r2 r3 r4 r5
	    r6 r7 r8 r9 r10
	    r11 r12 r13
	    r14 r15 r16))
