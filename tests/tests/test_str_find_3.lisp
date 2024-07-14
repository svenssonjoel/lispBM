(check (=
    (str-find "ab ab" "ab")
    (str-find "ab ab" "ab" 'right)
    (str-find "ab ab" "ab" 0 -100)
    (str-find "ab ab" "ab" 0 -5)
    (str-find "" "")
    (str-find "ab" "")
    (str-find "ab ab" '("ba" "ab"))
    0
))