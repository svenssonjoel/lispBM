
;; Promotion of u32 type

(if (is-64bit) {
  (define a1 (eq (type-of (* 1u32 1b)) type-u32))
  (define a2 (eq (type-of (* 1u32 1)) type-i))
  (define a3 (eq (type-of (* 1u32 1u)) type-u))
  (define a4 (eq (type-of (* 1u32 1i32)) type-u32))
  (define a5 (eq (type-of (* 1u32 1u32)) type-u32))
  (define a6 (eq (type-of (* 1u32 1i64)) type-i64))
  (define a7 (eq (type-of (* 1u32 1u64)) type-u64))
  (define a8 (eq (type-of (* 1u32 1.0f32)) type-float))
  (define a9 (eq (type-of (* 1u32 1.0f64)) type-Double))
  }
  {
  (define a1 (eq (type-of (* 1u32 1b)) type-u32))
  (define a2 (eq (type-of (* 1u32 1)) type-u32))
  (define a3 (eq (type-of (* 1u32 1u)) type-u32))
  (define a4 (eq (type-of (* 1u32 1i32)) type-u32))
  (define a5 (eq (type-of (* 1u32 1u32)) type-u32))
  (define a6 (eq (type-of (* 1u32 1i64)) type-i64))
  (define a7 (eq (type-of (* 1u32 1u64)) type-u64))
  (define a8 (eq (type-of (* 1u32 1.0f32)) type-float))
  (define a9 (eq (type-of (* 1u32 1.0f64)) type-Double))
  })

(check (apply and (list a1 a2 a3 a4 a5 a6 a7 a8 a9)))
