## About Symbols

Symbols are very important and central to LispBM and also perhaps a bit different from identifiers/names used in languages such as C. A short introduction to symbols could be a good place to start. 

One way to think about a symbol is as a name. Used as a name, a symbol can identify a value or function in the environment. A symbol can also be used as data in and of itself, more on this later. 



---

**NOTE**
Symbols are expressed as strings in your program such as `a`, `let`, `define`, `+` or `orange`. The "reader", the part of LBM that parses code, translates each symbol into a 28bit value. The string `orange` for example is only of interest if you print a symbol and then the runtime system will look up what string corresponds to the 28bit identifier you want to print. So the runtime system is never wasting time comparing strings to see if a symbol is this or that symbol, it's all integer comparisons. 


---

You associate values with symbols using, <a href="#define">define</a>, <a href="#let">let</a> and you can change the value bound to a "variable" using <a href="#set">set</a>, <a href="#setvar">setq</a> or <a href="#setvar">setvar</a>. 

Not all symbols are treated the same in LBM. Some symbols are treated as special because of their very fundamental nature. Among these special symbols you find `define`, `let` and `lambda` for example. These are things that you should not be able to redefine and trying to redefine them leads to an error. Symbols that start with `ext-` are special and reserved for use together with extensions that are loaded and bound at runtime. 

Examples of symbols used as data are `nil` and `t`. `nil` represents "nothing", the empty list or other similar things and `t` represents true.  But any symbol can be used as data by quoting it `'`, see <a href="#quotes-and-quasiquotation"> Quotes and Quasiquotation </a>. 

### Valid Symbol Names

A symbol is a string of characters following the rules: 1. The first character is a one of 'a' - 'z' or 'A' - 'Z' or '+-*/=<>#!'. 2. The rest of the characters are in 'a' - 'z' or 'A' - 'Z' or '0' - '9' or '+-*/=<>!?_'. 3. At most 256 characters long. 

Note that lower-case and upper-case alphabetical letters are considered identical so the symbol `apa` is the same symbol as `APA`. 

examples of valid symbols: ``` apa apa? !apa kurt_russel_is_great ``` 



## Arithmetic


---


### +

Adds up an aribtrary number of values. The form of a `+` expression is `(+ expr1 ... exprN)`. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(+ 1 2)
```


</td>
<td>

```clj
3
```


</td>
</tr>
<tr>
<td>

```clj
(+ 1 2 3 4)
```


</td>
<td>

```clj
10
```


</td>
</tr>
<tr>
<td>

```clj
(+ 1 1u)
```


</td>
<td>

```clj
2u
```


</td>
</tr>
<tr>
<td>

```clj
(+ 2 3.140000f32)
```


</td>
<td>

```clj
5.140000f32
```


</td>
</tr>
</table>



---


---


### -

Subtract an arbitrary number of values from a value. The form of a `-` expression is `(- expr1 ... exprN)`. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(- 5 3)
```


</td>
<td>

```clj
2
```


</td>
</tr>
<tr>
<td>

```clj
(- 10 5 5)
```


</td>
<td>

```clj
0
```


</td>
</tr>
<tr>
<td>

```clj
(- 10 2u)
```


</td>
<td>

```clj
8u
```


</td>
</tr>
<tr>
<td>

```clj
(- 10 3.140000f32)
```


</td>
<td>

```clj
6.860000f32
```


</td>
</tr>
</table>



---


---


### *

Multiplying an arbitrary number of values. The form of a `*` expression is `(* expr1 ... exprN)`. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(* 2 2)
```


</td>
<td>

```clj
4
```


</td>
</tr>
<tr>
<td>

```clj
(* 2 3 4 5)
```


</td>
<td>

```clj
120
```


</td>
</tr>
<tr>
<td>

```clj
(* 10 2u)
```


</td>
<td>

```clj
20u
```


</td>
</tr>
<tr>
<td>

```clj
(* 4 3.140000f32)
```


</td>
<td>

```clj
12.560000f32
```


</td>
</tr>
</table>



---


---


### /

Division. The form of a `/` expression is `(/ expr1 ... exprN)`. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(/ 128 2)
```


</td>
<td>

```clj
64
```


</td>
</tr>
<tr>
<td>

```clj
(/ 6.280000f32 2)
```


</td>
<td>

```clj
3.140000f32
```


</td>
</tr>
<tr>
<td>

```clj
(/ 256 2 2 2 2 2 2 2)
```


</td>
<td>

```clj
2
```


</td>
</tr>
</table>



---


---


### mod

Modulo operation. The form of a `mod` expression is `(mod expr1 exp2)`. The modulo operation is not generalised to n arguments. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(mod 5 3)
```


</td>
<td>

```clj
2
```


</td>
</tr>
<tr>
<td>

```clj
(mod 1024 100)
```


</td>
<td>

```clj
24
```


</td>
</tr>
<tr>
<td>

```clj
(mod -7 5)
```


</td>
<td>

```clj
-2
```


</td>
</tr>
</table>



---

## Comparisons


---


### eq

Compare values for equality. The `eq` operation implements structural equiality. The form of an 'eq` expression is `(eq expr1 ... exprN)`. 
 Structural equality means that the values must have the identical in memory representations to be considered equal. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(eq (+ 1 2) 3)
```


</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(eq 1 1 1 1)
```


</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(eq 1 1 2 1)
```


</td>
<td>

```clj
nil
```


</td>
</tr>
<tr>
<td>

```clj
(eq (+ 3 4) (+ 2 5) (+ 1 6))
```


</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(eq (list 1 2 3 4) (list 1 2 3 4))
```


</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(eq (list 1 2 4 5) (list 1 2 3 4))
```


</td>
<td>

```clj
nil
```


</td>
</tr>
</table>



---


---


### not-eq

`not-eq` implements the negation of eq. In other words, `(not-eq a b c)` evaluates to the same result as `(not (eq a b c))`. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(not-eq (+ 1 2) 3)
```


</td>
<td>

```clj
nil
```


</td>
</tr>
<tr>
<td>

```clj
(not-eq 1 1 1 1)
```


</td>
<td>

```clj
nil
```


</td>
</tr>
<tr>
<td>

```clj
(not-eq 1 1 2 1)
```


</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(not-eq (+ 3 4) (+ 2 5) (+ 1 6))
```


</td>
<td>

```clj
nil
```


</td>
</tr>
<tr>
<td>

```clj
(not-eq (list 1 2 3 4) (list 1 2 3 4))
```


</td>
<td>

```clj
nil
```


</td>
</tr>
<tr>
<td>

```clj
(not-eq (list 1 2 4 5) (list 1 2 3 4))
```


</td>
<td>

```clj
t
```


</td>
</tr>
</table>



---


---


### =

The `=` operation can only be used on numerical arguments. If you know you are comparing numbers, it will be more efficient to use `=`. 
 An important difference between `eq` and `=` is that `=` compare the numerical values of the arguments. A 3 is a 3 independent of them being different types. `eq` on the other hand compares the representations of the arguments exactly and they must match in structure, type and value to be considered equal. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(= 1 1)
```


</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(= 1 2)
```


</td>
<td>

```clj
nil
```


</td>
</tr>
<tr>
<td>

```clj
(= (+ 2 3) (+ 1 4))
```


</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(= (+ 1 2) (+ 2 3))
```


</td>
<td>

```clj
nil
```


</td>
</tr>
</table>



---


---


### >

Greater than comparison. A greater than comparison has the form `(> expr1 ... exprN)` and evaluates to `t` if expr1 is greater than all of expr2 ... exprN. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(> 5 2)
```


</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(> 2 5)
```


</td>
<td>

```clj
nil
```


</td>
</tr>
<tr>
<td>

```clj
(> 3.140000f32 1)
```


</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(> 1 3.140000f32)
```


</td>
<td>

```clj
nil
```


</td>
</tr>
</table>



---

## Boolean operators


---


### and

Boolean `and` operation between n arguments. The form of an `and` expression is `(and expr1 ... exprN)`.  This operation treats all non-nil values as true. Boolean `and` is "shirt-circuiting" and only evaluates until a false is encountered. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(and t t)
```


</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(and t t (+ 1 2))
```


</td>
<td>

```clj
3
```


</td>
</tr>
<tr>
<td>

```clj
(and t (< 5 3))
```


</td>
<td>

```clj
nil
```


</td>
</tr>
</table>



---


---


### or

Boolean `or` operation between n arguments. The form of an `or` expression is `(or expr1 ... exprN)`.  This operation treats all non-nil values as true. Boolean `or` is "short-circuiting" and only evaluates until a true is encountered. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(or nil nil)
```


</td>
<td>

```clj
nil
```


</td>
</tr>
<tr>
<td>

```clj
(or nil t)
```


</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(or t nil)
```


</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(or t t)
```


</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(or nil (+ 1 2))
```


</td>
<td>

```clj
3
```


</td>
</tr>
</table>



---


---


### not

Boolean `not` takes one argument. The form of a `not` expression is `(not expr)`. All non-nil values are considered true. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(not t)
```


</td>
<td>

```clj
nil
```


</td>
</tr>
<tr>
<td>

```clj
(not nil)
```


</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(not 42)
```


</td>
<td>

```clj
nil
```


</td>
</tr>
</table>



---

## Bit level operations


---


### shl

The shift left operation takes two arguments. The first argument is a value to shift and the second argument is the number of bit positions to shift the value. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(shl 1 2)
```


</td>
<td>

```clj
4
```


</td>
</tr>
<tr>
<td>

```clj
(shl 1u32 2)
```


</td>
<td>

```clj
4u32
```


</td>
</tr>
<tr>
<td>

```clj
(shl 1u64 2)
```


</td>
<td>

```clj
4u64
```


</td>
</tr>
</table>



---


---


### shr

The shift right operation takes two arguments. The first argument is a value to shift and the second argument in the number of bit positions to shift the value. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(shr 4 2)
```


</td>
<td>

```clj
1
```


</td>
</tr>
<tr>
<td>

```clj
(shr 4u32 2)
```


</td>
<td>

```clj
1u32
```


</td>
</tr>
<tr>
<td>

```clj
(shr 4u64 2)
```


</td>
<td>

```clj
1u64
```


</td>
</tr>
</table>



---


---


### bitwise-and

Performs the bitwise and operation between two values. The type of the result is the same type as the first of the arguments. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(bitwise-and 1048831u32 65535)
```


</td>
<td>

```clj
255u32
```


</td>
</tr>
</table>



---


---


### bitwise-or

Performs the bitwise or operation between two values. The type of the result is the same type as the first of the arguments. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(bitwise-or 1048816 15)
```


</td>
<td>

```clj
1048831
```


</td>
</tr>
</table>



---


---


### bitwise-xor

Performs the bitwise exclusive or operation between two values. The type of the result is the same type as the first of the arguments. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(bitwise-xor 1048816 255)
```


</td>
<td>

```clj
1048591
```


</td>
</tr>
</table>



---


---


### bitwise-not

Performs the bitwise negation operations on a value. The result is of same type as the argument. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(bitwise-not 4096u32)
```


</td>
<td>

```clj
4294963199u32
```


</td>
</tr>
</table>



---

## nil and t, true and false


---


### nil

Represents the empty list. The nil value is also considered to be false by conditionals. `nil` is a symbol but it cannot be redefined and will always evaluate to itself. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(cons 1 nil)
```


</td>
<td>

```clj
(1)
```


</td>
</tr>
<tr>
<td>

```clj
(if nil 3 100)
```


</td>
<td>

```clj
100
```


</td>
</tr>
<tr>
<td>

```clj
nil
```


</td>
<td>

```clj
nil
```


</td>
</tr>
</table>



---


---


### t

All non nil values are considered true in conditionals. `t` should be used in cases where an explicit true makes sense. `t` is a symbol but it cannot be redefined and will always evaluate to itself. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(cons 1 t)
```


</td>
<td>

```clj
(1 . t)
```


</td>
</tr>
<tr>
<td>

```clj
(if t 3 100)
```


</td>
<td>

```clj
3
```


</td>
</tr>
<tr>
<td>

```clj
t
```


</td>
<td>

```clj
t
```


</td>
</tr>
</table>



---


---


### false

`false` is an alias for `nil`. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(cons 1 false)
```


</td>
<td>

```clj
(1)
```


</td>
</tr>
<tr>
<td>

```clj
(if false 3 100)
```


</td>
<td>

```clj
100
```


</td>
</tr>
<tr>
<td>

```clj
false
```


</td>
<td>

```clj
nil
```


</td>
</tr>
</table>



---


---


### true

`true` is an alias for `t`. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(cons 1 true)
```


</td>
<td>

```clj
(1 . t)
```


</td>
</tr>
<tr>
<td>

```clj
(if true 3 100)
```


</td>
<td>

```clj
3
```


</td>
</tr>
<tr>
<td>

```clj
true
```


</td>
<td>

```clj
t
```


</td>
</tr>
</table>



---

## Quotes and Quasiquotation

Code and data share the same representation, it is only a matter of how you look at it. The tools for changing view, or interpretation, are the quotation and quasiquotation operations. 


---


### quote

Usages of the `'` quote symbol in input code is replaced with the symbol quote by the reader.  Evaluating a quoted expression, (quote a), results in a unevaluated. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
'(+ 1 2)
```


</td>
<td>

```clj
(+ 1 2)
```


</td>
</tr>
<tr>
<td>

```clj
(eval '(+ 1 2))
```


</td>
<td>

```clj
3
```


</td>
</tr>
<tr>
<td>

```clj
'kurt
```


</td>
<td>

```clj
kurt
```


</td>
</tr>
<tr>
<td>

```clj
'(+ 1 2)
```


</td>
<td>

```clj
(+ 1 2)
```


</td>
</tr>
<tr>
<td>

```clj
(eval '(+ 1 2))
```


</td>
<td>

```clj
3
```


</td>
</tr>
<tr>
<td>

```clj
'kurt
```


</td>
<td>

```clj
kurt
```


</td>
</tr>
</table>



---


---


### `

The backwards tick `` ` `` is called the quasiquote. It is similar to the `'` but allows splicing in results of computations using the <a href="#,">,</a> and the <a href="#commaat">,@</a> operators. 

The result of `'(+ 1 2)` and `` `(+ 1 2)`` are similar in effect. Both result in the result value of `(+ 1 2)`, that is a list containing +, 1 and 2.  When `` `(+ 1 2)`` is read into the heap it is expanded into the expression `(append (quote (+)) (append (quote (1)) (append (quote (2)) (quote nil))))` which evaluates to the list `(+ 1 2)`. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
`(+ 1 2)
```


</td>
<td>

```clj
(+ 1 2)
```


</td>
</tr>
<tr>
<td>

```clj
`(+ 1 ,(+ 1 1))
```


</td>
<td>

```clj
(+ 1 2)
```


</td>
</tr>
<tr>
<td>

```clj
(append '(+ 1) (list (+ 1 1)))
```


</td>
<td>

```clj
(+ 1 2)
```


</td>
</tr>
</table>



---


---


### ,

The comma is used to splice the result of a computation into a quasiquotation. 

The expression `` `(+ 1 ,(+ 1 1))`` is expanded by the reader into `(append (quote (+)) (append (quote (1)) (append (list (+ 1 1)) (quote nil))))`. Evaluating the expression above results in the list `(+ 1 2)`. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
`(+ 1 ,(+ 1 1))
```


</td>
<td>

```clj
(+ 1 2)
```


</td>
</tr>
</table>



---


---


### ,@

The comma-at operation is used to splice in the result of a computation (that returns a list) into a list when quasiquoting. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
`(1 2 3 ,@(range 4 10))
```


</td>
<td>

```clj
(1 2 3 4 5 6 7 8 9)
```


</td>
</tr>
</table>



---

## Built-in operations


---


### eval

Evaluate data as an expression. The data must represent a valid expression. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(eval (list + 1 2))
```


</td>
<td>

```clj
3
```


</td>
</tr>
<tr>
<td>

```clj
(eval '(+ 1 2))
```


</td>
<td>

```clj
3
```


</td>
</tr>
<tr>
<td>

```clj
(eval `(+ 1 ,@(range 2 5)))
```


</td>
<td>

```clj
10
```


</td>
</tr>
</table>



---


---


### eval-program

Evaluate a list of data where each element represents an expression. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(eval-program (list (list + 1 2) (list + 3 4)))
```


</td>
<td>

```clj
7
```


</td>
</tr>
<tr>
<td>

```clj
(eval-program '( (+ 1 2) (+ 3 4)))
```


</td>
<td>

```clj
7
```


</td>
</tr>
<tr>
<td>

```clj
(eval-program (list (list define 'a 10) (list + 'a 1)))
```


</td>
<td>

```clj
11
```


</td>
</tr>
<tr>
<td>

```clj
(eval-program '( (define a 10) (+ a 1)))
```


</td>
<td>

```clj
11
```


</td>
</tr>
</table>



---


---


### type-of

The `type-of` function returns a symbol that indicates what type the argument is. The form of a `type-of` expression is `(type-of expr)`. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(type-of 1)
```


</td>
<td>

```clj
type-i
```


</td>
</tr>
<tr>
<td>

```clj
(type-of 1u)
```


</td>
<td>

```clj
type-u
```


</td>
</tr>
<tr>
<td>

```clj
(type-of 1i32)
```


</td>
<td>

```clj
type-i32
```


</td>
</tr>
<tr>
<td>

```clj
(type-of 1u32)
```


</td>
<td>

```clj
type-u32
```


</td>
</tr>
<tr>
<td>

```clj
(type-of 1i64)
```


</td>
<td>

```clj
type-i64
```


</td>
</tr>
<tr>
<td>

```clj
(type-of 1u64)
```


</td>
<td>

```clj
type-u64
```


</td>
</tr>
<tr>
<td>

```clj
(type-of 3.140000f32)
```


</td>
<td>

```clj
type-float
```


</td>
</tr>
<tr>
<td>

```clj
(type-of 3.140000f64)
```


</td>
<td>

```clj
type-double
```


</td>
</tr>
<tr>
<td>

```clj
(type-of 'apa)
```


</td>
<td>

```clj
type-symbol
```


</td>
</tr>
<tr>
<td>

```clj
(type-of (list 1 2 3))
```


</td>
<td>

```clj
type-list
```


</td>
</tr>
</table>



---


---


### sym2str

The `sym2str` function converts a symbol to its string representation. The resulting string is a copy of the original so you cannot destroy built in symbols using this function. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(sym2str 'lambda)
```


</td>
<td>

```clj
lambda
```


</td>
</tr>
<tr>
<td>

```clj
(sym2str 'lambda)
```


</td>
<td>

```clj
lambda
```


</td>
</tr>
</table>



---


---


### str2sym

The `str2sym` function converts a string to a symbol. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(str2sym hello)
```


</td>
<td>

```clj
hello
```


</td>
</tr>
</table>



---


---


### sym2u

The `sym2u` function returns the numerical value used by the runtime system for a symbol. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(sym2u 'lambda)
```


</td>
<td>

```clj
259u
```


</td>
</tr>
<tr>
<td>

```clj
(sym2u 'lambda)
```


</td>
<td>

```clj
259u
```


</td>
</tr>
</table>



---


---


### u2sym

The `u2sym` function returns the symbol associated with the numerical value provided. This symbol may be undefined in which case you get as result a unnamed symbol. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(u2sym 259u)
```


</td>
<td>

```clj
lambda
```


</td>
</tr>
<tr>
<td>

```clj
(u2sym 66334u)
```


</td>
<td>

```clj

```


</td>
</tr>
</table>



---

## Special forms

Special forms looks a lot like functions but they are allowed to break the norms when it comes to evaluation order of arguments. a special form may choose to evaluate or not, freely, from its list of arguments. 


---


### if

Conditionals are written as `(if cond-expr then-expr else-expr)`.  If the cond-expr evaluates to <a href="#nil"> nil </a> the else-expr will be evaluated.  for any other value of cond-expr the then-expr will be evaluated. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(if t 1 2)
```


</td>
<td>

```clj
1
```


</td>
</tr>
<tr>
<td>

```clj
(if nil 1 2)
```


</td>
<td>

```clj
2
```


</td>
</tr>
</table>



---


---


### cond

`cond` is a generalization of `if` to discern between n different cases based on boolean expressions. The form of a `cond` expression is: `(cond ( cond-expr1 expr1) (cond-expr2 expr2) ... (cond-exprN exprN))`. The conditions are checked from first to last and for the first `cond-exprN` that evaluates to true, the corresponding `exprN` is evaluated. 

If no `cond-exprN` evaluates to true, the result of the entire conditional is `nil`. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>


```clj
(define a 0)
(cond ((< a 0) 'abrakadabra)
      ((> a 0) 'llama)
      ((= a 0) 'hello-world))

```


</td>
<td>


```clj
hello-world
```


</td>
</tr>
<tr>
<td>


```clj
(define a 5)
(cond ((= a 1) 'doughnut)
      ((= a 7) 'apple-strudel)
      ((= a 10) 'baklava))

```


</td>
<td>


```clj
nil
```


</td>
</tr>
</table>



---


---


### lambda

You create an anonymous function with lambda. The function can be given a name by binding the lambda expression using <a href="#define">define</a> or <a href="#let">let</a>. A lambda expression has the form `(lambda param-list body-expr)`. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(lambda (x) (+ x 1))
```


</td>
<td>

```clj
(closure (x) (+ x 1) nil)
```


</td>
</tr>
<tr>
<td>

```clj
((lambda (x) (+ x 1)) 1)
```


</td>
<td>

```clj
2
```


</td>
</tr>
</table>



---


---


### closure

A <a href="#lambda"> lambda </a> expression evaluates into a closure which is very similar to a <a href="#lambda">lambda</a> but extended with a captured environment for any names unbound in the param-list appearing in the body-expr.  The form of a closure is `(closure param-list body-exp environment)`. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(lambda (x) (+ x 1))
```


</td>
<td>

```clj
(closure (x) (+ x 1) nil)
```


</td>
</tr>
<tr>
<td>

```clj
(let ((a 1))
     (lambda (x) (+ a x)))
```


</td>
<td>

```clj
(closure (x) (+ a x) ((a . 1)))
```


</td>
</tr>
<tr>
<td>

```clj
(let ((a 1)
      (b 2))
     (lambda (x) (+ a b x)))
```


</td>
<td>

```clj
(closure (x) (+ a b x) ((b . 2) (a . 1)))
```


</td>
</tr>
</table>



---


---


### let

Local environments are created using let. The let binding in lispbm allows for mutually recursive bindings. The form of a let is `(let list-of-bindings body-expr)` and evaluating this expression means that body-expr is evaluted in an environment extended with the list-of-bindings. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(let ((a 1)
      (b 2))
     (+ a b))
```


</td>
<td>

```clj
3
```


</td>
</tr>
<tr>
<td>

```clj
(let ((f (lambda (x) (if (= x 0) 0 (g (- x 1)))))
      (g (lambda (x) (if (= x 0) 1 (f (- x 1))))))
     (f 11))
```


</td>
<td>

```clj
1
```


</td>
</tr>
</table>



---


---


### loop

loop allows to repeatedly evaluate an expression for as long as a condition holds. The form of a loop is `(loop list-of-local-bindings condition-exp body-exp)`. 

The  `list-of-local-bindings` are very similar to how `let` works, just that here the `body-exp` is repeated. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>


```clj
(define sum 0)
(loop ((a 0)) (<= a 10) (progn (setq sum (+ sum a))
       (setq a (+ a 1))))
sum

```


</td>
<td>


```clj
55
```


</td>
</tr>
</table>



---


---


### define

You can give names to values in a global scope by using define. The form of define is `(define name expr)`. The expr is evaluated and it is the result of the evaluated expr that is stored in the environment. In lispbm you can redefine already defined values. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(define apa 10)
```


</td>
<td>

```clj
10
```


</td>
</tr>
</table>



---


---


### undefine

A definition in the global can be removed using undefine.  The form of an undefine expression is `(undefine name-expr)` where name-expr should evaluate to a symbol (for example `'apa`). 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(undefine 'apa)
```


</td>
<td>

```clj
t
```


</td>
</tr>
</table>
It is also possible to undefine several bindings at the same time by providing a list of names. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(undefine '(apa bepa cepa))
```


</td>
<td>

```clj
t
```


</td>
</tr>
</table>



---


---


### set

The `set` form is used to change the value of some variable in an environment. You can use `set` to change the value of a global definition or a local definition. An application of the `set` form looks like `(set var-expr val-expr)` where `var-expr` should evaluate to a symbol. The `val-expr` is evaluated before rebinding the variable. `set` returns the value that `val-expr` evaluates to. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>


```clj
(define a 10)
(set 'a 20)
a

```


</td>
<td>


```clj
20
```


</td>
</tr>
</table>
`set` works in local environments too such as in the body of a `let` or in a `progn`-local variable created using `var`. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>


```clj
(progn (var a 10)
       (set 'a 20)
       a)

```


</td>
<td>


```clj
20
```


</td>
</tr>
</table>



---


---


### setq

The `setq` special-form is similar to `set` and to `setvar` but expects the first argument to be a symbol. The first argument to `setq` is NOT evaluated. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>


```clj
(define a 10)
(setq a 20)
a

```


</td>
<td>


```clj
20
```


</td>
</tr>
</table>
Just like `set` and `setvar`, `setq` can be used on variables that are bound locally such as in the body of a `let` or a `progn`-local variable created using `var`. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>


```clj
(progn (var a 10)
       (setq a 20)
       a)

```


</td>
<td>


```clj
20
```


</td>
</tr>
</table>



---


---


### setvar

`setvar` is the exact same thing as `set` 




---


---


### progn

The progn special form allows you to sequence a number of expressions. The form of a progn expression is `(progn expr1 ... exprN)`. 

The evaluation result of a progn sequence is the value that the last `exprN` evaluated to. This is useful for sequencing of side-effecting operations. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(progn 1
       2
       3)
```


</td>
<td>

```clj
3
```


</td>
</tr>
<tr>
<td>

```clj
(progn (define a 10)
       (define b 20)
       (+ a b))
```


</td>
<td>

```clj
30
```


</td>
</tr>
</table>



---


---


### {

The curlybrace `{` syntax is a short-form (syntactic sugar) for `(progn`. The parser replaces occurrences of `{` with `(progn`. The `{` should be closed with an `}`. 

These two programs are thus equivalent: 

```clj
 (progn
   (define a 10)
   (define b 20)
   (+ a b))
 ```
 

And 

```clj
 {
   (define a 10)
   (define b 20)
   (+ a b)
 }
 ```
 




---


---


### }

The closing curlybrace `}` should be used to close an opening `{` but purely for esthetical reasons. The `}` is treated identically to a regular closing parenthesis `)`. 

The opening `{` and closing `}` curlybraces are used as a short-form for `progn`-blocks of sequences expressions. 




---


---


### var

The var special form allows local bindings in a progn expression. A var expression is of the form (var symbol expr) and the symbol `symbol` is bound to the value that `expr` evaluates to withing the rest of the progn expression. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(progn (var a 10)
       (var b 20)
       (+ a b))
```


</td>
<td>

```clj
30
```


</td>
</tr>
<tr>
<td>

```clj
(progn (var a 10)
       (var b (+ a 10))
       (+ a b))
```


</td>
<td>

```clj
30
```


</td>
</tr>
</table>



---


---


### read

Parses a string resulting in either an expression or the <a href="#read_error">read_error</a> in case the string can not be parsed into an expression. The form of a read expression is `(read string)`. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(read "1")
```


</td>
<td>

```clj
1
```


</td>
</tr>
<tr>
<td>

```clj
(read "(lambda (x) (+ x 1))"
```


</td>
<td>

```clj
(lambda (x) (+ x 1))
```


</td>
</tr>
</table>



---


---


### read-program

Parses a string containing multiple sequenced expressions. The resulting list of expressions can be evaluated as a program using <a href="#eval-program">eval-program</a>. The form of a read-program expression is `(read-program string)`. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(read-program "(define apa 1) (+ 2 apa)")
```


</td>
<td>

```clj
((define apa 1) (+ 2 apa))
```


</td>
</tr>
</table>



---


---


### read-eval-program

Parses and evaluates a program incrementally. `read-eval-program` reads a top-level expression then evaluates it before reading the next. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(read-eval-program "(define a 10) (+ a 10)")
```


</td>
<td>

```clj
20
```


</td>
</tr>
</table>
`read-eval-program` supports the `@const-start` and `@const-end` annotations which move all global definitions created in the program to constant memory (flash). 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(read-eval-program "@const-start (define a 10) (+ a 10) @const-end")
```


</td>
<td>

```clj
20
```


</td>
</tr>
</table>



---

## Lists and cons cells

Lists are built using cons cells. A cons cell is represented by the lbm_cons_t struct in the implementation and consists of two fields named the `car` and the `cdr`. There is no special meaning associated with the `car` and the `cdr` each can hold a lbm_value. See <a href="#cons">cons</a> and <a href="#list">list</a> for two ways to create structures of cons cells on the heap. 

![cons cell](images/cons_cell.png "cons cell") 

A cons cell can be used to store a pair of values. You create a pair by sticking a value in both the car and cdr field of a cons cell using either `'(1 . 2)` or `(cons 1 2)`. 

![pair](images/pair.png "pair") 

A list is a number of cons cells linked together where the car fields hold values and the cdr fields hold pointers (the last cdr field is nil). The list below can be created either as `'(1 2 3)` or as `(list 1 2 3)`. 

![list](images/list.png "list") 


---


### car

Use `car` to access the `car` field of a cons cell. A `car` expression has the form `(car expr)`. 

Taking the `car` of a number of symbol type is in general a <a href="#type_error">type_error</a>. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(car (cons 1 2))
```


</td>
<td>

```clj
1
```


</td>
</tr>
<tr>
<td>

```clj
(car (list 9 8 7))
```


</td>
<td>

```clj
9
```


</td>
</tr>
</table>



---


---


### first

`first` is an alternative  name for the `car` operation. Use `first` to access the first element of a list or pair. A `first` expression  has the form `(first expr)`. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(car (cons 1 2))
```


</td>
<td>

```clj
1
```


</td>
</tr>
<tr>
<td>

```clj
(car (list 9 8 7))
```


</td>
<td>

```clj
9
```


</td>
</tr>
</table>



---


---


### cdr

Use `cdr` to access the `cdr` field of a cons cell. A `cdr` expression has the form `(cdr expr)`. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(cdr (cons 1 2))
```


</td>
<td>

```clj
2
```


</td>
</tr>
<tr>
<td>

```clj
(cdr (list 9 8 7))
```


</td>
<td>

```clj
(8 7)
```


</td>
</tr>
</table>



---


---


### rest

`rest` is an alternative name for the `cdr` operation. Use `rest` to access all elements except the first one of a list, or to access the second element in a pair. A `rest` expression has the form `(rest expr)`. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(cdr (cons 1 2))
```


</td>
<td>

```clj
2
```


</td>
</tr>
<tr>
<td>

```clj
(cdr (list 9 8 7))
```


</td>
<td>

```clj
(8 7)
```


</td>
</tr>
</table>



---


---


### cons

The `cons` operation allocates a cons cell from the heap and populates the `car` and the `cdr` fields of this cell with its two arguments. The form of a `cons` expression is `(cons expr1 expr2)`. To build well formed lists the innermost cons cell should have nil in the cdr field. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(cons 1 (cons 2 (cons 3 nil)))
```


</td>
<td>

```clj
(1 2 3)
```


</td>
</tr>
<tr>
<td>

```clj
(cons 1 2)
```


</td>
<td>

```clj
(1 . 2)
```


</td>
</tr>
<tr>
<td>

```clj
(cons + 1)
```


</td>
<td>

```clj
(+ . 1)
```


</td>
</tr>
<tr>
<td>

```clj
(cons (cons 1 2) (cons 3 4))
```


</td>
<td>

```clj
((1 . 2) 3 . 4)
```


</td>
</tr>
</table>



---


---


### .

The dot, `.`, operation creates a pair. The form of a dot expression is `(expr1 . expr2)`. By default the evaluator will attempt to evaluate the result of `(expr1 . expr2)` unless it is prefixed with `'`. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
'(1 . 2)
```


</td>
<td>

```clj
(1 . 2)
```


</td>
</tr>
<tr>
<td>

```clj
'((1 . 2) . 3)
```


</td>
<td>

```clj
((1 . 2) . 3)
```


</td>
</tr>
</table>



---


---


### list

The `list` function is used to create proper lists. The function takes n arguments and is of the form `(list expr1 ... exprN)`. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(list 1 2 3 4)
```


</td>
<td>

```clj
(1 2 3 4)
```


</td>
</tr>
</table>



---


---


### length

Computes the length of a list. The `length` function takes one argument and is of the form `(length expr)`. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(length (list 1 2 3 4))
```


</td>
<td>

```clj
4
```


</td>
</tr>
</table>



---


---


### range

The `range` function computes a list with integer values from a range specified by its endpoints. The form of a range expression is `(range start-expr end-expr)`. The end point in the range is excluded. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(range 4 8)
```


</td>
<td>

```clj
(4 5 6 7)
```


</td>
</tr>
<tr>
<td>

```clj
(range 0 10)
```


</td>
<td>

```clj
(0 1 2 3 4 5 6 7 8 9)
```


</td>
</tr>
<tr>
<td>

```clj
(range -4 4)
```


</td>
<td>

```clj
(-4 -3 -2 -1 0 1 2 3)
```


</td>
</tr>
</table>



---


---


### append

The `append` function combines two lists into a longer list. An `append` expression is of the form `(append expr1 expr2)`. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(append (list 1 2 3 4) (list 5 6 7 8))
```


</td>
<td>

```clj
(1 2 3 4 5 6 7 8)
```


</td>
</tr>
</table>



---


---


### ix

Index into a list using the `ix` function. The form of an `ix` expression is `(ix list-expr index-expr)`. Indexing starts from 0 and if you index out of bounds the result is nil. A negative index accesses values starting from the end of the list. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(ix (list 1 2 3 4) 1)
```


</td>
<td>

```clj
2
```


</td>
</tr>
<tr>
<td>

```clj
(ix (list 1 2 3 4) -1)
```


</td>
<td>

```clj
4
```


</td>
</tr>
</table>



---


---


### setix

Destructively update an element in a list. The form of a `setix` expression is `(setix list-expr index-extr value-expr)`. Indexing starts from 0 and if you index out of bounds the result is nil. A negative value -n will update the nth value from the end of the list. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(setix (list 1 2 3 4 5) 2 77)
```


</td>
<td>

```clj
(1 2 77 4 5)
```


</td>
</tr>
<tr>
<td>

```clj
(setix (list 1 2 3 4 5) -2 66)
```


</td>
<td>

```clj
(1 2 3 66 5)
```


</td>
</tr>
</table>



---


---


### setcar

The `setcar` is a destructive update of the car field of a cons-cell. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>


```clj
(define apa '(1 . 2))
(setcar apa 42)
apa

```


</td>
<td>


```clj
(42 . 2)
```


</td>
</tr>
<tr>
<td>


```clj
(define apa (list 1 2 3 4))
(setcar apa 42)
apa

```


</td>
<td>


```clj
(42 2 3 4)
```


</td>
</tr>
</table>



---


---


### setcdr

The `setcdr` is a destructive update of the cdr field of a cons-cell. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>


```clj
(define apa '(1 . 2))
(setcdr apa 42)
apa

```


</td>
<td>


```clj
(1 . 42)
```


</td>
</tr>
<tr>
<td>


```clj
(define apa (list 1 2 3 4))
(setcdr apa (list 99 100))
apa

```


</td>
<td>


```clj
(1 99 100)
```


</td>
</tr>
</table>



---


---


### take

`take` creates a list containing the `n` first elements of another list. The form of a `take` expression is `(take list-exp n-exp)`. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>


```clj
(define apa (list 1 2 3 4 5 6 7 8 9 10))
(take apa 5)

```


</td>
<td>


```clj
(1 2 3 4 5)
```


</td>
</tr>
</table>



---


---


### drop

`drop` creates a list from another list by dropping the `n` first elements of that list. The form of a `drop` expression is `(drop list-exp n-exp)`. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>


```clj
(define apa (list 1 2 3 4 5 6 7 8 9 10))
(drop apa 5)

```


</td>
<td>


```clj
(6 7 8 9 10)
```


</td>
</tr>
</table>



---


---


### merge

`merge` merges two lists that are ordered according to a comparator into a single ordered list. The form of a `merge` expression is `(merge comparator-exp list-exp1 list-exp2)`. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>


```clj
(define a (list 2 4 6 8 10 12))
(define b (list 1 3 5))
(merge < a b)

```


</td>
<td>


```clj
(1 2 3 4 5 6 8 10 12)
```


</td>
</tr>
</table>



---


---


### sort

`sort` orders a list of values according to a comparator. The sorting algorithm used is an in-place merge-sort. A copy of the input list is created at the beginning of the sort to provide a functional interface from the user's point of view. The form of a sort expression is `(sort comparator-exp list-exp)` 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>


```clj
(define a (list 1 9 2 5 1 8 3))
(sort < a)

```


</td>
<td>


```clj
(1 1 2 3 5 8 9)
```


</td>
</tr>
</table>



---

## association lists (alists)

Association lists (alists) are, just like regular lists, built out of cons-cells. The difference is that an alist is a list of pairs where the first element in each par can be thought of as a key and the second element can be thought of as the value. So alists implement a key-value lookup structure. 

`(list '(1 . horse) '(2 . donkey) '(3 . shark))` is an example of an alist with integer keys and symbol values. 


---


### acons

The `acons` form is similar to `cons`, it attaches one more element onto an alist. The element that is added consists of a key and a value so `acons` takes one more argument than `cons`. The form of an `acons` expression is `(acons key-expr val-expr alist-expr)`. The `alist-expr` should evaluate to an alist but there are no checks to ensure this. 

Example that adds the key `4` and associated value `lemur` to an existing alist. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(acons 4 'lemur (list '(1 . horse) '(2 . donkey) '(3 . shark)))
```


</td>
<td>

```clj
((4 . lemur) (1 . horse) (2 . donkey) (3 . shark))
```


</td>
</tr>
</table>



---


---


### assoc

The `assoc` function looks up the first value in an alist matching a given a key.  The form of an `assoc` expression is `(assoc alist-expr key-expr)` 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(assoc (list '(1 . horse) '(2 . donkey) '(3 . shark)) 2)
```


</td>
<td>

```clj
donkey
```


</td>
</tr>
</table>



---


---


### cossa

The `cossa` function looks up the first key in an alist that matches a given value. The form of an `cossa` expression is `(cossa alist-expr value-expr)` 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(cossa (list '(1 . horse) '(2 . donkey) '(3 . shark)) 'donkey)
```


</td>
<td>

```clj
2
```


</td>
</tr>
</table>



---


---


### setassoc

The `setassoc` function destructively updates a key-value mapping in an alist. The form of a `setassoc` expression is `(setassoc alist-expr key-expr value-expr)`. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>


```clj
(define apa (list '(1 . horse) '(2 . donkey) '(3 . shark)))
(setassoc apa 2 'llama)

```


</td>
<td>


```clj
((1 . horse) (2 . llama) (3 . shark))
```


</td>
</tr>
</table>



---

## Arrays (byte buffers)

This document was generated by LispBM version 0.22.0 

