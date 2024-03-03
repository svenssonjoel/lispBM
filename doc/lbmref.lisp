
(defun has-alt-txt (x)
  (match x
         ( (alt-txt . _) true)
         (_ false)))



;; TODO: LBM pretty printer is needed for better presentation of programs.
;; TODO: Programs and expressions should evaluate in an empty local env.

(defun render-code-res-pairs (rend cs)
  (match cs
         (nil t)
         ( ((? x) . (? xs))
           (let ((x-str (if (has-alt-txt x)
                            (ix x 2)
                          (to-str x)))
                 (x-code (if (has-alt-txt x)
                             (ix x 1)
                           x))
                 (res (eval x-code))
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
           (let ((cstrs (map (lambda (c) (str-merge (to-str c) "\n"))  x))
                 (res (eval-program x))
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
         ( (para (? x)) { (map (lambda (s) (rend (str-merge s " "))) x) (rend "\n") } )
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
                           )))
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
                  )
            )))


;; Manual

(def manual (list ch-symbols
                  arithmetic
                  comparisons
                  boolean
                  bitwise
                  nil-and-t
                  quotes
                  built-ins
                  special-forms
                  ))




(defun render-manual ()
  (let ((h (fopen "test.md" "w"))
        (r (lambda (s) (fwrite-str h s))))
    (render r manual)))

