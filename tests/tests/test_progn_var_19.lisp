(def lst (list 1 2 3 4))

{
    (var (a b . rst) lst)
    (check (= a 1))
    (check (= b 2))
    (check (eq rst (list 3 4)))
}