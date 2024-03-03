
(defun has-alt-txt (x)
  (match x
         ( (alt-txt . _) true)
         (_ false)))


(defun pretty (c)
  (pretty-ind 0 c))

(defun ind-spaces (n)
  (str-replicate n 32b))

(defun pretty-ind (n c)
  (match c
         ( (progn (? e ) . (? es))
           (str-merge "(progn " (pretty e) (pretty-aligned-ontop (+ n 7) es) ")" ))
         ( (quote (? e)) (str-merge (ind-spaces n) "'" (pretty e)))
         ( (let ((? b0) . (? brest)) (? body)) ;; pattern
           (str-merge (ind-spaces n)
                      "(let ("

                      (pretty b0)
                      (pretty-aligned-ontop (+ n 6) brest)
                      ")\n"

                      (pretty-ind (+ n 5) body)
                      ")"
                      ))
         ( (cond (? x) . (? xs) )
           (let ( (conds (pretty-aligned-ontop (+ n 6) xs))
                  (cond0 (pretty x)))
             (str-merge (ind-spaces n) "(cond " cond0 conds ")")
             )
           )
         ( ((? x) . (? xs)) (str-merge (ind-spaces n) "(" (pretty x) (pretty-list xs) ")" ))
         (_ (str-merge (ind-spaces n) (to-str c))))
  )

(defun pretty-list (c)
  (match c
         ( nil "" )
         ( ((? x) . nil) (str-merge " " (pretty x) ))
         ( ((? x) . (? y))
           (if (eq (type-of y) type-list)
               (str-merge " " (pretty x) (pretty-list y))
             (str-merge " " (pretty x) "." (pretty y)))
           )
         ( (? x) (str-merge " . " (pretty x)))))

(defun pretty-aligned-ontop (n cs)
  (match cs
         (nil "")
         ( ( (? x ) . (? xs))
           (str-merge "\n" (pretty-ind n x) (pretty-aligned-ontop n xs))))
  )

(defun render-code-res-pairs (rend cs)
  (match cs
         (nil t)
         ( ((? x) . (? xs))
           (let ((x-str (if (has-alt-txt x)
                            (ix x 2)
                          (pretty x)))
                 (x-code (if (has-alt-txt x)
                             (ix x 1)
                           x))
                 (res (eval nil x-code))
                 (rstr (to-str res)))
             {
             (rend "<tr>\n")
             (rend "<td>\n\n")
             (rend "```clj\n")
             (rend x-str)
             (rend "\n```\n")
             (rend "\n\n</td>\n")
             (rend "<td>\n\n")
             (rend "```clj\n")
             (rend rstr)
             (rend "\n```\n")
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

(defun render-program-res-pairs (rend cs)
  (match cs
         (nil t)
         ( ((? x) . (? xs))
           (let ((cstrs (map (lambda (c) (str-merge (pretty c) "\n"))  x))
                 (res (eval-program nil x))
                 (rstr (to-str res)))
             {
             (rend "<tr>\n")
             (rend "<td>\n\n")
             (rend "\n```clj\n")
             (map rend cstrs)
             (rend "\n```\n")
             (rend "\n\n</td>\n")
             (rend "<td>\n\n")
             (rend "\n```clj\n")
             (rend rstr)
             (rend "\n```\n")
             (rend "\n\n</td>\n")
             (rend "</tr>\n")
             (render-program-res-pairs rend xs)
             }))))

(defun render-program-table (rend c)
  {
    (rend "<table>\n")
    (rend "<tr>\n")
    (rend "<td> Example </td> <td> Result </td>\n")
    (rend "</tr>\n")
    (render-program-res-pairs rend c)
    (rend "</table>\n")
    })


(defun render-it (rend ss)
  (match ss
         ( nil (rend "\n") )
         ( (section (? i) (? x) (? xs))
           {
           (match i
                  (1 (rend (str-merge "# " x "\n\n")))
                  (2 (rend (str-merge "## " x "\n\n")))
                  (3 (rend (str-merge "### " x "\n\n")))
                  (4 (rend (str-merge "#### " x "\n\n"))))
           (render rend xs)
           }
           )
         ( (para (? x)) { (map (lambda (s) (rend (str-merge s " "))) x) (rend "\n\n") } )
         ( hline (rend "\n---\n\n"))
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

(defun image (alt url)
  (para (list (str-merge "![" alt "](" url " \"" alt "\")"))))

(def ch-symbols
     (section 2 "About Symbols"
              ( list
                (para (list "Symbols are very important and central to LispBM and also perhaps"
                            "a bit different from identifiers/names used in languages such as C."
                            "A short introduction to symbols could be a good place to start."
                            ))
                (para (list "One way to think about a symbol is as a name. Used as a name, a"
                            "symbol can identify a value or function in the environment. A"
                            "symbol can also be used as data in and of itself, more on"
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

(define comp-!=
  (ref-entry "!="
             (list
              (para (list "The `!=` operation implements the negation of `=`. So, `(!= a b)` evaluates to the same result as `(not (= a b))`."
                          ))
              (code '((!= 1 1)
                      (!= 1 2)
                      (!= (+ 2 3) (+ 1 4))
                      (!= (+ 1 2) (+ 2 3))
                      ))
              end)))

(define comp->
  (ref-entry ">"
             (list
              (para (list "Greater than comparison. A greater than comparison has the form `(> expr1 ... exprN)` and evaluates to `t` if expr1 is greater than all of expr2 ... exprN."
                          ))

              (code '((> 5 2)
                      (> 2 5)
                      (> 3.14 1)
                      (> 1 3.14)
                      ))
              end)))

(define comp-<
  (ref-entry "<"
             (list
              (para (list "Less than comparison. A less than comparison has the form `(> expr1 ... exprN)` and evaluates to `t` if expr1 is less than all of expr2 ... exprN."
                          ))

              (code '((< 5 2)
                      (< 5 2)
                      (< 3.14 1)
                      (< 1 3.14)
                      ))
              end)))

(define comp->=
  (ref-entry ">="
             (list
              (para (list "Greater than or equal comparison. A greater than comparison has the form `(>= expr1 ... exprN)` and evaluates to `t` if expr1 is greater than or equal to all of expr2 ... exprN."
                          ))

              (code '((>= 1 1)
                      (>= 5 2)
                      (>= 2 5)
                      (>= 3.14 1)
                      (>= 1 3.14)
                      ))
              end)))

(define comp-<=
  (ref-entry "<="
             (list
              (para (list "Less than or equal comparison. A less than or equal comparison has the form `(<= expr1 ... exprN)` and evaluates to `t` if expr1 is less than or equal to all of expr2 ... exprN."
                          ))

              (code '((<= 1 1)
                      (<= 5 2)
                      (<= 2 5)
                      (<= 3.14 1)
                      (<= 1 3.14)
                      ))
              end)))

(define comparisons
  (section 2 "Comparisons"
           (list comp-eq
                 comp-not-eq
                 comp-=
                 comp->
                 )
           ))

;; Boolean operators

(define bool-and
  (ref-entry "and"
             (list
              (para (list "Boolean `and` operation between n arguments. The form of an `and`"
                          "expression is `(and expr1 ... exprN)`.  This operation treats all"
                          "non-nil values as true. Boolean `and` is \"shirt-circuiting\" and only"
                          "evaluates until a false is encountered."
                          ))
              (code '((and t t)
                      (and t t (+ 1 2))
                      (and t (< 5 3))
                      ))
              end)))

(define bool-or
  (ref-entry "or"
             (list
              (para (list "Boolean `or` operation between n arguments. The form of an `or`"
                          "expression is `(or expr1 ... exprN)`.  This operation treats all"
                          "non-nil values as true. Boolean `or` is \"short-circuiting\" and only"
                          "evaluates until a true is encountered."
                          ))

              (code '((or nil nil)
                      (or nil t)
                      (or t nil)
                      (or t t)
                      (or nil (+ 1 2))
                      ))
              end)))

(define bool-not
  (ref-entry "not"
             (list
              (para (list "Boolean `not` takes one argument. The form of a `not` expression is"
                          "`(not expr)`. All non-nil values are considered true."
                          ))

              (code '((not t)
                      (not nil)
                      (not 42)
                      ))
              end)))



(define boolean
  (section 2 "Boolean operators"
           (list bool-and
                 bool-or
                 bool-not
                 )
           ))

;; Bitwise operations
(define bit-shl
  (ref-entry "shl"
             (list
              (para (list "The shift left operation takes two arguments. The first argument is a value to shift and the"
                          "second argument is the number of bit positions to shift the value."
                          ))

              (code '((shl 1 2)
                      (shl 1u32 2)
                      (shl 1u64 2)
                      ))

              end)))

(define bit-shr
  (ref-entry "shr"
             (list
              (para (list "The shift right operation takes two arguments. The first argument is a"
                          "value to shift and the second argument in the number of bit positions"
                          "to shift the value."
                          ))
              (code '((shr 4 2)
                      (shr 4u32 2)
                      (shr 4u64 2)))

              end)))

(define bit-and
  (ref-entry "bitwise-and"
             (list
              (para (list "Performs the bitwise and operation between two values. The type of the result"
                          "is the same type as the first of the arguments."
                          ))
              (code '((bitwise-and 1048831u32 0xFFFF)
                      ))

              end)))

(define bit-or
  (ref-entry "bitwise-or"
             (list
              (para (list "Performs the bitwise or operation between two values. The type of the result"
                          "is the same type as the first of the arguments."
                          ))
              (code '((bitwise-or 1048816 0xF)
                      ))
              end)))

(define bit-xor
  (ref-entry "bitwise-xor"
             (list
              (para (list "Performs the bitwise exclusive or operation between two values. The type of the result"
                          "is the same type as the first of the arguments."
                          ))
              (code '((bitwise-xor 1048816 0xFF)
                      ))
              end)))

(define bit-not
  (ref-entry "bitwise-not"
             (list
              (para (list "Performs the bitwise negation operations on a value. The result is of same type as"
                          "the argument."
                          ))
              (code '((bitwise-not 4096u32)
                      ))
              end)))

(define bitwise
  (section 2 "Bit level operations"
           (list bit-shl
                 bit-shr
                 bit-and
                 bit-or
                 bit-xor
                 bit-not
            )
           ))

;; Nil and t, true and false

(define value-nil
  (ref-entry "nil"
             (list
              (para (list "Represents the empty list. The nil value is also considered to be false by conditionals."
                          "`nil` is a symbol but it cannot be redefined and will always evaluate to itself."
                          ))
              (code '((cons 1 nil)
                      (if nil 3 100)
                      nil
                      ))

              end)))

(define value-t
  (ref-entry "t"
             (list
              (para (list "All non nil values are considered true in conditionals. `t` should be used in cases where an"
                          "explicit true makes sense. `t` is a symbol but it cannot be redefined and will always evaluate to itself."
                          ))
              (code '((cons 1 t)
                      (if t 3 100)
                      t
                      ))
              end)))

(define value-false
  (ref-entry "false"
             (list
              (para (list "`false` is an alias for `nil`."
                          ))
              (code '((alt-txt (cons 1 false) "(cons 1 false)")
                      (alt-txt (if false 3 100) "(if false 3 100)")
                      (alt-txt false "false")
                      ))
              end)))

(define value-true
  (ref-entry "true"
             (list
              (para (list "`true` is an alias for `t`."
                          ))
              (code '((alt-txt (cons 1 true) "(cons 1 true)")
                      (alt-txt (if true 3 100) "(if true 3 100)")
                      (alt-txt true "true")
                      ))
              end)))


(define nil-and-t
  (section 2 "nil and t, true and false"
           (list value-nil
                 value-t
                 value-false
                 value-true
                 )
           ))

;; Quotes and quasiqoutation

(define op-quote
  (ref-entry "quote"
             (list
              (para (list "Usages of the `'` quote symbol in input code is replaced with the"
                          "symbol quote by the reader.  Evaluating a quoted expression, (quote"
                          "a), results in a unevaluated."
                          ))
              (code '((alt-txt '(+ 1 2) "'(+ 1 2)")
                      (alt-txt (eval '(+ 1 2)) "(eval '(+ 1 2))")
                      (alt-txt 'kurt "'kurt")
                      (quote (+ 1 2))
                      (eval (quote (+ 1 2)))
                      (quote kurt)
                      ))
              end)))

(define op-quasi
  (ref-entry "`"
             (list
              (para (list "The backwards tick `` ` `` is called the quasiquote. It is similar to"
                          "the `'` but allows splicing in results of computations using the <a"
                          "href=\"#,\">,</a> and the <a href=\"#commaat\">,@</a> operators."
                          ))
              (para (list "The result of `'(+ 1 2)` and `` `(+ 1 2)`` are similar in effect. Both"
                          "result in the result value of `(+ 1 2)`, that is a list containing +,"
                          "1 and 2.  When `` `(+ 1 2)`` is read into the heap it is expanded into"
                          "the expression `(append (quote (+)) (append (quote (1)) (append (quote"
                          "(2)) (quote nil))))` which evaluates to the list `(+ 1 2)`."
                          ))
              (code '((alt-txt `(+ 1 2) "`(+ 1 2)")
                      (alt-txt `(+ 1 ,(+ 1 1)) "`(+ 1 ,(+ 1 1))")
                      (append (quote (+ 1)) (list (+ 1 1)))
                      ))
              end)))

(define op-comma
  (ref-entry ","
             (list
              (para (list "The comma is used to splice the result of a computation into a quasiquotation."
                          ))
              (para (list "The expression `` `(+ 1 ,(+ 1 1))`` is expanded by the reader into"
                          "`(append (quote (+)) (append (quote (1)) (append (list (+ 1 1)) (quote nil))))`."
                          "Evaluating the expression above results in the list `(+ 1 2)`."
                          ))
              (code '((alt-txt `(+ 1 ,(+ 1 1)) "`(+ 1 ,(+ 1 1))")
                      ))
              end)))

(define op-commaat
  (ref-entry ",@"
             (list
              (para (list "The comma-at operation is used to splice in the result of a computation (that"
                          "returns a list) into a list when quasiquoting."
                          ))
              (code '((alt-txt `(1 2 3 ,@(range 4 10)) "`(1 2 3 ,@(range 4 10))")
                      ))
              end)))

(define quotes
  (section 2 "Quotes and Quasiquotation"
           (list (para
                  (list "Code and data share the same representation, it is only a matter of"
                        "how you look at it. The tools for changing view, or interpretation,"
                        "are the quotation and quasiquotation operations."
                        ))
                 op-quote
                 op-quasi
                 op-comma
                 op-commaat
                 )))

;; Built-in operations

(define built-in-eval
  (ref-entry "eval"
             (list
              (para (list "Evaluate data as an expression. The data must represent a valid expression."
                          ))

              (code '((eval (list + 1 2))
                      (alt-txt (eval '(+ 1 2)) "(eval '(+ 1 2))")
                      (alt-txt (eval `(+ 1 ,@(range 2 5))) "(eval `(+ 1 ,@(range 2 5)))")
                      ))
              end)))

(define built-in-eval-program
  (ref-entry "eval-program"
             (list
              (para (list "Evaluate a list of data where each element represents an expression."
                          ))
              (code '((eval-program (list (list + 1 2) (list + 3 4)))
                      (alt-txt (eval-program '( (+ 1 2) (+ 3 4))) "(eval-program '( (+ 1 2) (+ 3 4)))")
                      (eval-program (list (list define 'a 10) (list + 'a 1)))
                      (alt-txt (eval-program '( (define a 10) (+ a 1))) "(eval-program '( (define a 10) (+ a 1)))")
                      ))
              end)))

(define built-in-type-of
  (ref-entry "type-of"
             (list
              (para (list "The `type-of` function returns a symbol that indicates what type the"
                          "argument is. The form of a `type-of` expression is `(type-of expr)`."
                          ))
              (code '((type-of 1)
                      (type-of 1u)
                      (type-of 1i32)
                      (type-of 1u32)
                      (type-of 1i64)
                      (type-of 1u64)
                      (type-of 3.14)
                      (type-of 3.14f64)
                      (alt-txt (type-of 'apa) "(type-of 'apa)")
                      (type-of (list 1 2 3))
                      ))
              end)))

(define built-in-sym2str
  (ref-entry "sym2str"
             (list
              (para (list "The `sym2str` function converts a symbol to its string representation."
                          "The resulting string is a copy of the original so you cannot destroy"
                          "built in symbols using this function."
                          ))
              (code '((sym2str (quote lambda))
                      (alt-txt (sym2str 'lambda) "(sym2str 'lambda)")
                      ))
              end)))

(define built-in-str2sym
  (ref-entry "str2sym"
             (list
              (para (list "The `str2sym` function converts a string to a symbol."
                          ))
              (code '((str2sym "hello")
                      ))
              end)))

(define built-in-sym2u
  (ref-entry "sym2u"
             (list
              (para (list "The `sym2u` function returns the numerical value used by the runtime system for a symbol."
                          ))
              (code '((sym2u (quote lambda))
                      (alt-txt (sym2u 'lambda) "(sym2u 'lambda)")
                      ))
              end)))

(define built-in-u2sym
  (ref-entry "u2sym"
             (list
              (para (list "The `u2sym` function returns the symbol associated with the"
                          "numerical value provided. This symbol may be undefined in which case you"
                          "get as result a unnamed symbol."
                          ))
              (code '((u2sym 259u)
                      (u2sym 66334u)
                      ))
              end)))

(define built-ins
  (section 2 "Built-in operations"
           (list built-in-eval
                 built-in-eval-program
                 built-in-type-of
                 built-in-sym2str
                 built-in-str2sym
                 built-in-sym2u
                 built-in-u2sym
                 )))

;; Special forms

(define special-form-if
  (ref-entry "if"
             (list
              (para (list "Conditionals are written as `(if cond-expr then-expr else-expr)`.  If"
                          "the cond-expr evaluates to <a href=\"#nil\"> nil </a> the else-expr will"
                          "be evaluated.  for any other value of cond-expr the then-expr will be"
                          "evaluated."
                          ))
              (code '((if t 1 2)
                      (if nil 1 2)
                      ))
              end)))

(define special-form-cond
  (ref-entry "cond"
             (list
              (para (list "`cond` is a generalization of `if` to discern between n different cases"
                          "based on boolean expressions. The form of a `cond` expression is:"
                          "`(cond ( cond-expr1 expr1) (cond-expr2 expr2) ... (cond-exprN exprN))`."
                          "The conditions are checked from first to last and for the first `cond-exprN`"
                          "that evaluates to true, the corresponding `exprN` is evaluated."
                          ))
              (para (list "If no `cond-exprN` evaluates to true, the result of the entire conditional"
                          "is `nil`."
                          ))
              (program  '(((define a 0)
                           (cond ( (< a 0) 'abrakadabra)
                                 ( (> a 0) 'llama)
                                 ( (= a 0) 'hello-world))
                           )
                          ((define a 5)
                           (cond ( (= a 1) 'doughnut)
                                 ( (= a 7) 'apple-strudel)
                                 ( (= a 10) 'baklava)))
                          ))

              end)))

(define special-form-lambda
  (ref-entry "lambda"
             (list
              (para (list "You create an anonymous function with lambda. The function can be given a name by binding the lambda expression using <a href=\"#define\">define</a>"
                          "or <a href=\"#let\">let</a>. A lambda expression has the form `(lambda param-list body-expr)`."
                          ))
              (code '((lambda (x) (+ x 1))
                      ((lambda (x) (+ x 1)) 1)
                      ))
              end)))

(define special-form-closure
  (ref-entry "closure"
             (list
              (para (list "A <a href=\"#lambda\"> lambda </a> expression evaluates into a closure"
                          "which is very similar to a <a href=\"#lambda\">lambda</a> but extended"
                          "with a captured environment for any names unbound in the param-list"
                          "appearing in the body-expr.  The form of a closure is `(closure"
                          "param-list body-exp environment)`."
                          ))
              (code '((lambda (x) (+ x 1))
                      (let ((a 1)) (lambda (x) (+ a x)))
                      (let ((a 1) (b 2)) (lambda (x) (+ a b x)))
                      ))
              end)))

(define special-form-let
  (ref-entry "let"
             (list
              (para (list "Local environments are created using let. The let binding in lispbm"
                          "allows for mutually recursive bindings. The form of a let is `(let"
                          "list-of-bindings body-expr)` and evaluating this expression means that"
                          "body-expr is evaluted in an environment extended with the"
                          "list-of-bindings."
                          ))

              (code '((let ((a 1) (b 2)) (+ a b))
                      (let ((f (lambda (x) (if (= x 0) 0 (g (- x 1)))))
                            (g (lambda (x) (if (= x 0) 1 (f (- x 1))))))
                        (f 11))
                      ))
              end)))

(define special-form-loop
  (ref-entry "loop"
             (list
              (para (list "loop allows to repeatedly evaluate an expression for as long as a condition"
                          "holds. The form of a loop is `(loop list-of-local-bindings condition-exp body-exp)`."
                          ))
              (para (list "The  `list-of-local-bindings` are very similar to how `let` works, just that here"
                          "the `body-exp` is repeated."
                          ))

              (program '(((define sum 0)
                          (loop ( (a 0) )  (<= a 10) { (setq sum (+ sum a)) (setq a (+ a 1)) })
                          sum)
                         ))
              end)))

(define special-form-define
  (ref-entry "define"
             (list
              (para (list "You can give names to values in a global scope by using define."
                          "The form of define is `(define name expr)`. The expr is evaluated and it is the"
                          "result of the evaluated expr that is stored in the environment."
                          "In lispbm you can redefine already defined values."
                          ))
              (code '((define apa 10)
                      ))
              end)))

(define special-form-undefine
  (ref-entry "undefine"
             (list
              (para (list "A definition in the global can be removed using undefine.  The form of"
                          "an undefine expression is `(undefine name-expr)` where name-expr"
                          "should evaluate to a symbol (for example `'apa`)."
                          ))
              {
              (define apa 10)
              (code '((undefine 'apa)
                      ))
              }
              (para (list "It is also possible to undefine several bindings at the same time by"
                          "providing a list of names."
                          ))
              {
              (define apa 10)
              (define bepa 20)
              (define cepa 30)
              (code '((undefine '(apa bepa cepa))
                      ))
              }
              end)))

(define special-form-set
  (ref-entry "set"
             (list
              (para (list "The `set` form is used to change the value of some variable in an environment."
                          "You can use `set` to change the value of a global definition or a local definition."
                          "An application of the `set` form looks like `(set var-expr val-expr)` where"
                          "`var-expr` should evaluate to a symbol. The `val-expr` is evaluated before"
                          "rebinding the variable. `set` returns the value that `val-expr` evaluates to."
                          ))
              (program '(((define a 10)
                          (set 'a 20)
                          a)
                         ))
              (para (list "`set` works in local environments too such as in the body of a `let`"
                          "or in a `progn`-local variable created using `var`."
                          ))
              (program '(((progn (var a 10) (set 'a 20) a))
                          ))

              end)))

(define special-form-setq
  (ref-entry "setq"
             (list
              (para (list "The `setq` special-form is similar to `set` and to `setvar` but expects the first argument"
                          "to be a symbol. The first argument to `setq` is NOT evaluated."
                          ))
              (program '(((define a 10)
                          (setq a 20)
                          a)
                         ))
              (para (list "Just like `set` and `setvar`, `setq` can be used on variables that"
                          "are bound locally such as in the body of a `let` or a `progn`-local variable"
                          "created using `var`."
                          ))
              (program '(((progn (var a 10) (setq a 20) a))
                         ))
              end)))

(define special-form-setvar
  (ref-entry "setvar"
             (list
              (para (list "`setvar` is the exact same thing as `set`"
                          ))
              end)))

(define special-form-progn
  (ref-entry "progn"
             (list
              (para (list "The progn special form allows you to sequence a number of expressions."
                          "The form of a progn expression is `(progn expr1 ... exprN)`."
                          ))
              (para (list "The evaluation result of a progn sequence is the value that the last `exprN`"
                          "evaluated to. This is useful for sequencing of side-effecting operations."
                          ))
              (code '((progn 1 2 3)
                      (progn (define a 10) (define b 20) (+ a b))
                      ))
              end)))

(define special-form-brack
  (ref-entry "{"
             (list
              (para (list "The curlybrace `{` syntax is a short-form (syntactic sugar) for `(progn`."
                          "The parser replaces occurrences of `{` with `(progn`. The `{` should be"
                          "closed with an `}`."
                          ))
              (para (list "These two programs are thus equivalent:"
                          ))
              (para (list "```clj\n"
                          "(progn\n"
                          "  (define a 10)\n"
                          "  (define b 20)\n"
                          "  (+ a b))\n"
                          "```\n"
                          ))
              (para (list "And"
                          ))
              (para (list "```clj\n"
                          "{\n"
                          "  (define a 10)\n"
                          "  (define b 20)\n"
                          "  (+ a b)\n"
                          "}\n"
                          "```\n"
                          ))
              end)))

(define special-form-close-brack
  (ref-entry "}"
             (list
              (para (list "The closing curlybrace `}` should be used to close an opening `{` but purely"
                          "for esthetical reasons. The `}` is treated identically to a regular closing parenthesis `)`."
                          ))
              (para (list 
                          "The opening `{` and closing `}` curlybraces are used as a short-form for `progn`-blocks"
                          "of sequences expressions."
                          ))
              end)))


(define special-form-var
  (ref-entry "var"
             (list
              (para (list "The var special form allows local bindings in a progn expression. A"
                          "var expression is of the form (var symbol expr) and the symbol `symbol`"
                          "is bound to the value that `expr` evaluates to withing the rest of the progn expression."
                          ))

              (code '((progn (var a 10) (var b 20) (+ a b))
                      (progn (var a 10) (var b (+ a 10)) (+ a b))
                      ))
              end)))
                          

(define special-form-read
  (ref-entry "read"
             (list
              (para (list "Parses a string resulting in either an expression or the <a href=\"#read_error\">read_error</a> in case"
                          "the string can not be parsed into an expression. The form of a read expression is"
                          "`(read string)`."
                          ))
              (code '((alt-txt (read "1") "(read \"1\")")
                      (alt-txt (read "(lambda (x) (+ x 1))") "(read \"(lambda (x) (+ x 1))\"")
                      ))
              end)))


(define special-form-read-program
  (ref-entry "read-program"
             (list
              (para (list "Parses a string containing multiple sequenced expressions. The resulting list of"
                          "expressions can be evaluated as a program using <a href=\"#eval-program\">eval-program</a>."
                          "The form of a read-program expression is `(read-program string)`."
                          ))
              (code '((alt-txt (read-program "(define apa 1) (+ 2 apa)") "(read-program \"(define apa 1) (+ 2 apa)\")")
                      ))
              end)))

(define special-form-read-eval-program
  (ref-entry "read-eval-program"
             (list
              (para (list "Parses and evaluates a program incrementally. `read-eval-program` reads a top-level expression"
                          "then evaluates it before reading the next."
                          ))
              (code '((alt-txt (read-eval-program "(define a 10) (+ a 10)") "(read-eval-program \"(define a 10) (+ a 10)\")")
                      ))
              (para (list "`read-eval-program` supports the `@const-start` and `@const-end` annotations which move all"
                          "global definitions created in the program to constant memory (flash)."
                          ))
              (code '((alt-txt (read-eval-program "@const-start (define a 10) (+ a 10) @const-end") "(read-eval-program \"@const-start (define a 10) (+ a 10) @const-end\")")
                      ))
              end)))

(define special-forms
  (section 2 "Special forms"
           (list
            (para (list "Special forms looks a lot like functions but they are allowed to"
                        "break the norms when it comes to evaluation order of arguments."
                        "a special form may choose to evaluate or not, freely, from its"
                        "list of arguments."
                        ))
            (list special-form-if
                  special-form-cond
                  special-form-lambda
                  special-form-closure
                  special-form-let
                  special-form-loop
                  special-form-define
                  special-form-undefine
                  special-form-set
                  special-form-setq
                  special-form-setvar
                  special-form-progn
                  special-form-brack
                  special-form-close-brack
                  special-form-var
                  special-form-read
                  special-form-read-program
                  special-form-read-eval-program
                  )
            )))

;; Lists and cons cells

(define lists-car
  (ref-entry "car"
             (list
              (para (list "Use `car` to access the `car` field of a cons cell. A `car` expression has the form `(car expr)`."
                          ))
              (para (list "Taking the `car` of a number of symbol type is in general a <a href=\"#type_error\">type_error</a>."
                          ))

              (code '((car (cons 1 2))
                      (car (list 9 8 7))
                      ))
              end)))

(define lists-first
  (ref-entry "first"
             (list
              (para (list "`first` is an alternative  name for the `car` operation."
                          "Use `first` to access the first element of a list or pair. A `first` expression  has the form `(first expr)`."
                          ))
              (code '((first (cons 1 2))
                      (first (list 9 8 7))
                      ))
              end)))


(define lists-cdr
  (ref-entry "cdr"
             (list
              (para (list "Use `cdr` to access the `cdr` field of a cons cell. A `cdr` expression has the form `(cdr expr)`."
                          ))
              (code '((cdr (cons 1 2))
                      (cdr (list 9 8 7))
                      ))
              end)))

(define lists-rest
  (ref-entry "rest"
             (list
              (para (list "`rest` is an alternative name for the `cdr` operation."
                          "Use `rest` to access all elements except the first one"
                          "of a list, or to access the second element in a pair. A `rest` expression has the form `(rest expr)`."
                          ))
              (code '((rest (cons 1 2))
                      (rest (list 9 8 7))
                      ))
              end)))

(define lists-cons
  (ref-entry "cons"
             (list
              (para (list "The `cons` operation allocates a cons cell from the heap and populates the"
                          "`car` and the `cdr` fields of this cell with its two arguments."
                          "The form of a `cons` expression is `(cons expr1 expr2)`."
                          "To build well formed lists the innermost cons cell should have"
                          "nil in the cdr field."
                          ))
              (code '((cons 1 (cons 2 (cons 3 nil)))
                      (cons 1 2)
                      (cons + 1)
                      (cons (cons 1 2) (cons 3 4))
                      ))
              end)))

(define lists-dot
  (ref-entry "."
             (list
              (para (list "The dot, `.`, operation creates a pair. The form of a dot expression"
                          "is `(expr1 . expr2)`. By default the evaluator will attempt to evaluate the"
                          "result of `(expr1 . expr2)` unless it is prefixed with `'`."
                          ))
              (code '('(1 . 2)
                      '((1 . 2) . 3)
                      ))
              end)))

(define lists-list
  (ref-entry "list"
             (list
              (para (list "The `list` function is used to create proper lists. The function"
                          "takes n arguments and is of the form `(list expr1 ... exprN)`."
                          ))
              (code '((list 1 2 3 4)
                      ))
              end)))

(define lists-length
  (ref-entry "length"
             (list
              (para (list "Computes the length of a list. The `length` function takes"
                          "one argument and is of the form `(length expr)`."
                          ))
              (code '((length (list 1 2 3 4))
                      ))
              end)))

(define lists-range
  (ref-entry "range"
             (list
              (para (list "The `range` function computes a list with integer values from a"
                          "range specified by its endpoints. The form of a range expression"
                          "is `(range start-expr end-expr)`. The end point in the range is excluded."
                          ))
              (code '((range 4 8)
                      (range 0 10)
                      (range -4 4)
                      ))
              end)))

(define lists-append
  (ref-entry "append"
             (list
              (para (list "The `append` function combines two lists into a longer list."
                          "An `append` expression is of the form `(append expr1 expr2)`."
                          ))
              (code '((append (list 1 2 3 4) (list 5 6 7 8))
                      ))
              end)))

(define lists-ix
  (ref-entry "ix"
             (list
              (para (list "Index into a list using the `ix` function. The form of an `ix` expression"
                          "is `(ix list-expr index-expr)`. Indexing starts from 0 and if you index out of bounds the result is nil."
                          "A negative index accesses values starting from the end of the list."
                          ))
              (code '((ix (list 1 2 3 4) 1)
                      (ix (list 1 2 3 4) -1)
                      ))
              end)))


(define lists-setix
  (ref-entry "setix"
             (list
              (para (list "Destructively update an element in a list. The form of a `setix` expression"
                          "is `(setix list-expr index-extr value-expr)`. Indexing starts from 0 and"
                          "if you index out of bounds the result is nil."
                          "A negative value -n will update the nth value from the end of the list."
                          ))
              (code '((setix (list 1 2 3 4 5) 2 77)
                      (setix (list 1 2 3 4 5) -2 66)
                      ))
              end)))

(define lists-setcar
  (ref-entry "setcar"
             (list
              (para (list "The `setcar` is a destructive update of the car field of a cons-cell."
                          ))
              (program '(((define apa '(1 . 2))
                          (setcar apa 42)
                          apa
                          )
                         ((define apa (list 1 2 3 4))
                          (setcar apa 42)
                          apa
                          )
                         ))
              end)))

(define lists-setcdr
  (ref-entry "setcdr"
             (list
              (para (list "The `setcdr` is a destructive update of the cdr field of a cons-cell."
                          ))
              (program '(((define apa '(1 . 2))
                          (setcdr apa 42)
                          apa
                          )
                         ((define apa (list 1 2 3 4))
                          (setcdr apa (list 99 100))
                          apa
                          )
                         ))
              end)))

(define lists-take
  (ref-entry "take"
             (list
              (para (list "`take` creates a list containing the `n` first elements of another list."
                          "The form of a `take` expression is `(take list-exp n-exp)`."
                          ))
              (program '(((define apa (list 1 2 3 4 5 6 7 8 9 10))
                          (take apa 5)
                          )
                         ))
              end)))

(define lists-drop
  (ref-entry "drop"
             (list
              (para (list "`drop` creates a list from another list by dropping the `n` first elements of that list."
                          "The form of a `drop` expression is `(drop list-exp n-exp)`."
                          ))
              (program '(((define apa (list 1 2 3 4 5 6 7 8 9 10))
                          (drop apa 5)
                          )
                         ))
              end)))

(define lists-merge
  (ref-entry "merge"
             (list
              (para (list "`merge` merges two lists that are ordered according to a comparator into"
                          "a single ordered list. The form of a `merge` expression is `(merge comparator-exp list-exp1 list-exp2)`."
                          ))
              (program '(((define a (list 2 4 6 8 10 12))
                          (define b (list 1 3 5))
                          (merge < a b)
                          )
                         ))
              end)))

(define lists-sort
  (ref-entry "sort"
             (list
              (para (list "`sort` orders a list of values according to a comparator. The sorting"
                          "algorithm used is an in-place merge-sort. A copy of the input list is created"
                          "at the beginning of the sort to provide a functional interface from the user's"
                          "point of view. The form of a sort expression is `(sort comparator-exp list-exp)`"
                          ))
              (program '(((define a (list 1 9 2 5 1 8 3))
                          (sort < a)
                          )
                         ))
              end)))



(define lists
  (section 2 "Lists and cons cells"
           (list
            (para (list "Lists are built using cons cells. A cons cell is represented by the lbm_cons_t struct in the"
                        "implementation and consists of two fields named the `car` and the `cdr`."
                        "There is no special meaning associated with the `car` and the `cdr` each can hold"
                        "a lbm_value. See <a href=\"#cons\">cons</a> and <a href=\"#list\">list</a> for two ways to create structures of"
                        "cons cells on the heap."
                        ))
            (image "cons cell" "images/cons_cell.png")
            (para (list "A cons cell can be used to store a pair of values. You create a pair by"
                        "sticking a value in both the car and cdr field of a cons cell using either `'(1 . 2)` or"
                        "`(cons 1 2)`."
                        ))
            (image "pair" "images/pair.png")
            (para (list "A list is a number of cons cells linked together where the car fields hold values"
                        "and the cdr fields hold pointers (the last cdr field is nil). The list below"
                        "can be created either as `'(1 2 3)` or as `(list 1 2 3)`."
                        ))
            (image "list" "images/list.png")
            lists-car
            lists-first
            lists-cdr
            lists-rest
            lists-cons
            lists-dot
            lists-list
            lists-length
            lists-range
            lists-append
            lists-ix
            lists-setix
            lists-setcar
            lists-setcdr
            lists-take
            lists-drop
            lists-merge
            lists-sort
            )))

;; Association lists 

(define assoc-acons
  (ref-entry "acons"
             (list
              (para (list "The `acons` form is similar to `cons`, it attaches one more element"
                          "onto an alist. The element that is added consists of a key and a value"
                          "so `acons` takes one more argument than `cons`. The form of an"
                          "`acons` expression is `(acons key-expr val-expr alist-expr)`."
                          "The `alist-expr` should evaluate to an alist but there are no checks"
                          "to ensure this."
                          ))
              (para (list "Example that adds the key `4` and associated value `lemur` to"
                          "an existing alist."
                          ))
              (code '((acons 4 'lemur (list '(1 . horse) '(2 . donkey) '(3 . shark)))
                      ))
              end)))
(define assoc-assoc
  (ref-entry "assoc"
             (list
              (para (list "The `assoc` function looks up the first value in an alist matching a given a key. "
                          "The form of an `assoc` expression is `(assoc alist-expr key-expr)`"
                          ))
              (code '((assoc (list '(1 . horse) '(2 . donkey) '(3 . shark)) 2)
                      ))
              end)))

(define assoc-cossa
  (ref-entry "cossa"
             (list
              (para (list "The `cossa` function looks up the first key in an alist that matches a given value."
                          "The form of an `cossa` expression is `(cossa alist-expr value-expr)`"
                          ))
              (code '((cossa (list '(1 . horse) '(2 . donkey) '(3 . shark)) 'donkey)
                      ))
              end)))

(define assoc-setassoc
  (ref-entry "setassoc"
             (list
              (para (list "The `setassoc` function destructively updates a key-value mapping in an"
                          "alist. The form of a `setassoc` expression is `(setassoc alist-expr key-expr value-expr)`."
                          ))
              (program '(((define apa (list '(1 . horse) '(2 . donkey) '(3 . shark)))
                          (setassoc apa 2 'llama)
                          )
                         ))
              end)))





(define assoc-lists
  (section 2 "association lists (alists)"
           (list
            (para (list "Association lists (alists) are, just like regular lists, built out"
                        "of cons-cells. The difference is that an alist is a list of pairs"
                        "where the first element in each par can be thought of as a key and"
                        "the second element can be thought of as the value. So alists implement"
                        "a key-value lookup structure."
                        ))
            (para (list "`(list '(1 . horse) '(2 . donkey) '(3 . shark))` is an example"
                        "of an alist with integer keys and symbol values."
                        ))
            assoc-acons
            assoc-assoc
            assoc-cossa
            assoc-setassoc
            )))

;; Arrays Byte buffers

(define arrays
  (section 2 "Arrays (byte buffers)"
           (list
            
            )))


;; Manual

(define info
  (let (((major minor patch) (lbm-version))
        (version-str (str-merge (to-str major) "." (to-str minor) "." (to-str patch))))
        (para (list (str-merge "This document was generated by LispBM version " version-str))
        )))

(define manual (list ch-symbols
                  arithmetic
                  comparisons
                  boolean
                  bitwise
                  nil-and-t
                  quotes
                  built-ins
                  special-forms
                  lists
                  assoc-lists
                  arrays
                  info
                  ))




(defun render-manual ()
  (let ((h (fopen "test.md" "w"))
        (r (lambda (s) (fwrite-str h s))))
    (render r manual)))

