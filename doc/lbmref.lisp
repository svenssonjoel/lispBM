
(defun render-code-res-pairs (rend cs)
  (match cs
         (nil t)
         ( ((? x) . (? xs))
           (let ((cstr (to-str x))
                 (res (eval x))
                 (rstr (to-str res)))
             {
             (rend "<tr>\n")
             (rend "<td>\n\n")
             (rend "```clj\n")
             (rend cstr)
             (rend "```\n")
             (rend "\n\n</td>\n")
             (rend "<td>\n\n")
             (rend "```clj\n")
             (rend rstr)
             (rend "```\n")
             (rend "\n\n</td>\n")
             (rend "</tr>\n")
             (render-code-res-pairs rend xs)
             }))))

(defun render-code-table (rend c)
    {
    (rend "<table>\n")
    (rend "<tr>\n")
    (rend "<td> Example </td> <td> Result </td>\n")
    (rend "</tr>\n")
    (render-code-res-pairs rend c)
    (rend "</table>\n")
    })

(defun render-program-table (rend c)
  (let ((cstrs (map to-str c))
        (res (eval-program c))
        (rstr (to-str res)))
    {
    (rend "<table>\n")
    (rend "<tr>\n")
    (rend "<td> Example </td> <td> Result </td>\n")
    (rend "</tr>\n")
    (rend "<td>\n\n")
    (rend "```clj\n")
    (map rend cstrs)
    (rend "```\n")
    (rend "\n\n</td>\n")
    (rend "<td>\n\n")
    (rend "```clj\n")
    (rend rstr)
    (rend "```\n")
    (rend "\n\n</td>\n")
    (rend "</tr>\n")
    (rend "</table>\n")
    }))


(defun render-it (rend ss)
  (match ss
         ( nil (rend "") )
         ( (section (? i) (? x) (? xs))
           {
           (match i
                  (1 (rend (str-merge "# " x "\n")))
                  (2 (rend (str-merge "## " x "\n")))
                  (3 (rend (str-merge "### " x "\n")))
                  (4 (rend (str-merge "#### " x "\n"))))
           (render rend xs)
           }
           )
         ( (para (? x)) { (map (lambda (s) (rend s)) x) (rend "\n") } )
         ( hline (rend "---"))
         ( newline (rend "\n"))
         ( (bold (? s))
           (rend (str-merge "**" s "**")))
         ( (program (? c)) (render-program-table rend c))
         ( (code (? c)) (render-code-table rend c))
         ( _ (render rend ss))
         ))


(defun render (rend ss)
  (match ss
         (nil t)
         ( ((? x) . (? xs))
           {(render-it rend x)
           (render rend xs) }
           )))

(define end nil)

(defun s+ (s ss)
  (cons s ss))

(defun section (i str strs)
  (list 'section i str strs))

(defun ref-entry (str strs)
  (list
   'hline
   'newline
   (section 3 str strs)
   'newline
   'hline
   ))

(defun hline ()
  'hline)

(defun para (str)
  (list 'para str))

(defun code (c)
  (list 'code c))

(defun code-examples (c)
  (list 'code-examples c))

(defun program (c)
  (list 'program c))

(defun newline ()
  'newline)

(defun bold (str)
  (list 'bold str))

(def ch-symbols
     (section 2 "About Symbols"
              ( list
                (para (list "Symbols are very important and central to LispBM and also perhaps"
                            "a bit different from identifiers/names used in languages such as C."
                            "A short introduction to symbols could be a good place to start."
                            ))
                (para (list "One way to think about a symbol is as a name. Used as a name, a"
                            "symbol can identify a value or function in the environment. A"
                            "symbol can also be used as data in and of itself, more on "
                            "this later."
                            ))
                'newline
                'hline
                (bold "NOTE") 'newline

                (para (list "Symbols are expressed as strings in your program such as `a`, `let`,"
                            "`define`, `+` or `orange`. The \"reader\", the part of LBM that parses"
                            "code, translates each symbol into a 28bit value. The string `orange`"
                            "for example is only of interest if you print a symbol and then the"
                            "runtime system will look up what string corresponds to the 28bit"
                            "identifier you want to print. So the runtime system is never wasting"
                            "time comparing strings to see if a symbol is this or that symbol, it's"
                            "all integer comparisons."
                            ))
                'hline

                (para (list "You associate values with symbols using, <a href=\"#define\">define</a>,"
                            "<a href=\"#let\">let</a> and you can change the value bound to a \"variable\""
                            "using <a href=\"#set\">set</a>, <a href=\"#setvar\">setq</a> or <a href=\"#setvar\">setvar</a>."
                            ))

                (para (list "Not all symbols are treated the same in LBM. Some symbols are treated as"
                            "special because of their very fundamental nature. Among these special symbols"
                            "you find `define`, `let` and `lambda` for example. These are things that you"
                            "should not be able to redefine and trying to redefine them leads to an error."
                            "Symbols that start with `ext-` are special and reserved for use together"
                            "with extensions that are loaded and bound at runtime."
                            ))
                (para (list "Examples of symbols used as data are `nil` and `t`. `nil` represents"
                            "\"nothing\", the empty list or other similar things and `t`"
                            "represents true.  But any symbol can be used as data by quoting it"
                            "`'`, see <a href=\"#quotes-and-quasiquotation\"> Quotes and Quasiquotation </a>."
                            ))

                (section 3 "Valid Symbol Names"
                         (list
                          (para (list "A symbol is a string of characters following the rules:"
                                      "1. The first character is a one of 'a' - 'z' or 'A' - 'Z' or '+-*/=<>#!'."
                                      "2. The rest of the characters are in 'a' - 'z' or 'A' - 'Z' or '0' - '9' or '+-*/=<>!?_'."
                                      "3. At most 256 characters long."
                                      ))
                          (para (list "Note that lower-case and upper-case alphabetical letters are considered identical"
                                      "so the symbol `apa` is the same symbol as `APA`."
                                      ))

                          (para (list "examples of valid symbols:"
                                      "```"
                                      "apa"
                                      "apa?"
                                      "!apa"
                                      "kurt_russel_is_great"
                                      "```"
                                      ))
                          end))

                end)))


;; Arithmetic section

(define arith-add
  (ref-entry "+"
           (list
            (para (list "Adds up an aribtrary number of values. The form of a `+` expression is `(+ expr1 ... exprN)`."
                        ))

            (code '((+ 1 2)
                    (+ 1 2 3 4)
                    (+ 1 1u)
                    (+ 2i 3.14)))
            end)))

(define arith-sub
  (ref-entry "-"
           (list
            (para (list "Subtract an arbitrary number of values from a value. The form of a `-` expression is `(- expr1 ... exprN)`."
                        ))
            (code '((- 5 3)
                    (- 10 5 5)
                    (- 10 2u)
                    (- 10 3.14)))
            end)))

(define arith-mul
  (ref-entry "*"
           (list
            (para (list "Multiplying an arbitrary number of values. The form of a `*` expression is `(* expr1 ... exprN)`."
                        ))

            (code '((* 2 2)
                    (* 2 3 4 5)
                    (* 10 2u)
                    (* 4 3.14)))
            end)))

(define arith-div
  (ref-entry "/"
             (list
              (para (list "Division. The form of a `/` expression is `(/ expr1 ... exprN)`."
                          ))
              (code '((/ 128 2)
                      (/ 6.28 2)
                      (/ 256 2 2 2 2 2 2 2)))
              end)))

(define arith-mod
  (ref-entry "mod"
             (list
              (para (list "Modulo operation. The form of a `mod` expression is `(mod expr1 exp2)`."
                          "The modulo operation is not generalised to n arguments."
                          ))
              (code '((mod 5 3)
                      (mod 1024 100)
                      (mod -7 5)))
              end)))

(define arithmetic
  (section 2 "Arithmetic"
           (list arith-add
                 arith-sub
                 arith-mul
                 arith-div
                 arith-mod
                 )
           ))

;; Comaprisons section

(define  comp-eq
  (ref-entry "eq"
            (list
             (para (list "Compare values for equality. The `eq` operation implements structural"
                         "equiality. The form of an 'eq` expression is `(eq expr1 ... exprN)`."
                         "\n"
                         "Structural equality means that the values must have the identical in"
                         "memory representations to be considered equal."
                         ))
             (code '((eq (+ 1 2) 3)
                     (eq 1 1 1 1)
                     (eq 1 1 2 1)
                     (eq (+ 3 4) (+ 2 5) (+ 1 6))
                     (eq (list 1 2 3 4) (list 1 2 3 4))
                     (eq (list 1 2 4 5) (list 1 2 3 4))
                     ))
             end)))

(define comp-not-eq
  (ref-entry "not-eq"
             (list
              (para (list "`not-eq` implements the negation of eq. In other words, `(not-eq a b c)` evaluates to the same result as `(not (eq a b c))`."
                          ))
              (code '((not-eq (+ 1 2) 3)
                      (not-eq 1 1 1 1)
                      (not-eq 1 1 2 1)
                      (not-eq (+ 3 4) (+ 2 5) (+ 1 6))
                      (not-eq (list 1 2 3 4) (list 1 2 3 4))
                      (not-eq (list 1 2 4 5) (list 1 2 3 4))
                      ))
              end)))

(define comp-=
  (ref-entry "="
             (list
              (para (list "The `=` operation can only be used on numerical arguments. If you know you are comparing numbers, it will be more efficient to use `=`."
                          "\n"
                          "An important difference between `eq` and `=` is"
                          "that `=` compare the numerical values of the arguments. A 3 is a 3"
                          "independent of them being different types. `eq` on the other"
                          "hand compares the representations of the arguments exactly and they must"
                          "match in structure, type and value to be considered equal."
                          ))
              (code '((= 1 1)
                      (= 1 2)
                      (= (+ 2 3) (+ 1 4))
                      (= (+ 1 2) (+ 2 3))
                      ))
              end)))


(define comparisons
  (section 2 "Comparisons"
           (list comp-eq
                 comp-not-eq
                 comp-=
                 )
           ))


(def manual (list ch-symbols
                  arithmetic
                  comparisons))
