# LispBM Runtime Extensions Reference Manual

The runtime extensions, if present, can be either compiled in a minimal or a full mode. In the minimal mode only `set-eval-quota` is present. Minimal mode is the default when compiling LBM. To get the full mode the `-DFULL_RTS_LIB` flag must be used when compiling. 

## Environments


### env-get

`env-get` can be used to reify, turn into value, parts of the global environment. The global environment is stored as a hashtable and an index into this hashtable is used to extract the bindings stored under that hash. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(env-get 0)
```


</td>
<td>

```clj
((pretty-aligned-ontop closure (n cs) (match cs (nil [0]) (((? x) ? xs) (str-merge "\n" (pretty-ind n x) (pretty-aligned-ontop n xs)))) nil))
```


</td>
</tr>
<tr>
<td>

```clj
(env-get 1)
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
(env-get 2)
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
(env-get 3)
```


</td>
<td>

```clj
((png-count . 0))
```


</td>
</tr>
<tr>
<td>

```clj
(env-get 4)
```


</td>
<td>

```clj
((defun macro (name args body) (me-defun name args body)))
```


</td>
</tr>
<tr>
<td>

```clj
(env-get 5)
```


</td>
<td>

```clj
((render-program-res-pairs closure (rend cs) (match cs (nil t) (((? x) ? xs) (let ((cstrs (map (lambda (c) (str-merge (pretty c) "\n")) x)) (res (eval-program nil x)) (rstr (to-str res))) (progn (rend "<tr>\n") (rend "<td>\n\n") (rend "\n```clj\n") (map r
```


</td>
</tr>
<tr>
<td>

```clj
(env-get 6)
```


</td>
<td>

```clj
((newline closure nil (quote newline) nil))
```


</td>
</tr>
<tr>
<td>

```clj
(env-get 7)
```


</td>
<td>

```clj
((table closure (header data) (list (quote table) header data) nil) (render-code-res-pairs closure (rend cs) (match cs (nil t) (((? x) ? xs) (let ((x-str (if (is-read-eval-txt x) (ix x 1) (pretty x))) (x-code (if (is-read-eval-txt x) (read (ix x 1)) x)) (
```


</td>
</tr>
<tr>
<td>

```clj
(env-get 8)
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
(env-get 9)
```


</td>
<td>

```clj
((render-code-png-table closure (rend img colors c) (progn (rend "<table>\n") (rend "<tr>\n") (rend "<td> Example </td> <td> Image </td> <td> Result </td>\n") (rend "</tr>\n") (render-code-png-pairs rend img colors c) (rend "</table>\n\n")) nil))
```


</td>
</tr>
<tr>
<td>

```clj
(env-get 10)
```


</td>
<td>

```clj
((end) (is-read-eval-txt closure (x) (match x ((read-eval . _) t) (_ nil)) nil) (intersperse closure (str strs) (match strs (((? s)) s) (((? s) ? ss) (str-merge s str (intersperse str ss)))) nil))
```


</td>
</tr>
<tr>
<td>

```clj
(env-get 11)
```


</td>
<td>

```clj
((render-program-table closure (rend c) (progn (rend "<table>\n") (rend "<tr>\n") (rend "<td> Example </td> <td> Result </td>\n") (rend "</tr>\n") (render-program-res-pairs rend c) (rend "</table>\n\n")) nil))
```


</td>
</tr>
<tr>
<td>

```clj
(env-get 12)
```


</td>
<td>

```clj
((s+ closure (s ss) (cons s ss) nil) (s-exp-graph closure (img-name code) (list (quote s-exp-graph) img-name code) nil))
```


</td>
</tr>
<tr>
<td>

```clj
(env-get 13)
```


</td>
<td>

```clj
((str-merge-list closure (strs) (match strs (nil [0]) (((? s) ? ss) (str-merge s (str-merge-list ss)))) nil))
```


</td>
</tr>
<tr>
<td>

```clj
(env-get 14)
```


</td>
<td>

```clj
((ref-entry closure (str strs) (list (quote newline) (section 3 str strs) (quote newline) (quote hline)) nil) (bold closure (str) (list (quote bold) str) nil) (code-examples closure (c) (list (quote code-examples) c) nil))
```


</td>
</tr>
<tr>
<td>

```clj
(env-get 15)
```


</td>
<td>

```clj
((render-dot closure (filename code) (let ((dot-str (to-dot code)) (name-dot (str-merge "./images/" filename ".dot")) (name-png (str-merge "./images/" filename ".png")) (fp-dot (fopen name-dot "w")) (fp-png (fopen name-png "w"))) (progn (fwrite fp-dot dot
```


</td>
</tr>
<tr>
<td>

```clj
(env-get 16)
```


</td>
<td>

```clj
((evaluation-quota newline (section 3 "set-eval-quota" ((para ("`set-eval-quota` sets the number of evaluation steps that is" "given to each context when given turn to execute by the round-robin" "scheduler.")) (code ((set-eval-quota 30))) nil)) newline h
```


</td>
</tr>
<tr>
<td>

```clj
(env-get 17)
```


</td>
<td>

```clj
((chapter-scheduling section 2 "Scheduling" ((newline (section 3 "set-eval-quota" ((para ("`set-eval-quota` sets the number of evaluation steps that is" "given to each context when given turn to execute by the round-robin" "scheduler.")) (code ((set-eval-
```


</td>
</tr>
<tr>
<td>

```clj
(env-get 18)
```


</td>
<td>

```clj
((num-free newline (section 3 "mem-num-free" ((para ("`mem-num-free` returns the number of free words in the LBM memory." "This is the memory where arrays and strings are stored.")) (code ((mem-num-free))) nil)) newline hline) (code-png closure (img color
```


</td>
</tr>
<tr>
<td>

```clj
(env-get 19)
```


</td>
<td>

```clj
((longest-free newline (section 3 "mem-longest-free" ((para ("`mem-longest-free` returns the length in words of the longest" "consecutive sequence of free words in the LBM memory.")) (code ((mem-num-free))) nil)) newline hline))
```


</td>
</tr>
<tr>
<td>

```clj
(env-get 20)
```


</td>
<td>

```clj
((heap-state newline (section 3 "lbm-heap-state" ((para ("`lbm-heap-state` can be used to query information about heap usage.")) (code ((lbm-heap-state (quote get-heap-size)) (lbm-heap-state (quote get-heap-bytes)) (lbm-heap-state (quote get-num-alloc-cel
```


</td>
</tr>
<tr>
<td>

```clj
(env-get 21)
```


</td>
<td>

```clj
((chapter-memory section 2 "Memory" ((newline (section 3 "mem-num-free" ((para ("`mem-num-free` returns the number of free words in the LBM memory." "This is the memory where arrays and strings are stored.")) (code ((mem-num-free))) nil)) newline hline) (
```


</td>
</tr>
<tr>
<td>

```clj
(env-get 22)
```


</td>
<td>

```clj
((gc-stack newline (section 3 "set-gc-stack-size" ((para ("With `set-gc-stack-size` you can change the size of the stack used for heap traversal" "by the garbage collector.")) (code ((set-gc-stack-size 100))) nil)) newline hline) (tableize closure (strs) 
```


</td>
</tr>
<tr>
<td>

```clj
(env-get 23)
```


</td>
<td>

```clj
((str-merge closure nil (str-join (rest-args)) nil) (chapter-gc section 2 "GC" ((newline (section 3 "set-gc-stack-size" ((para ("With `set-gc-stack-size` you can change the size of the stack used for heap traversal" "by the garbage collector.")) (code ((s
```


</td>
</tr>
<tr>
<td>

```clj
(env-get 24)
```


</td>
<td>

```clj
((environment-get newline (section 3 "env-get" ((para ("`env-get` can be used to reify, turn into value, parts of the global environment." "The global environment is stored as a hashtable and an index into this hashtable" "is used to extract the bindings 
```


</td>
</tr>
<tr>
<td>

```clj
(env-get 25)
```


</td>
<td>

```clj
((environment-set newline (section 3 "env-set" ((para ("`env-set` destructively sets an entry in the global environment hashtable.")) (program (((if (eq (env-get 1) nil) (env-set 1 (list (quote (a . 75))))) (env-get 1)))) (para ("Note that in the example 
```


</td>
</tr>
<tr>
<td>

```clj
(env-get 26)
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
(env-get 27)
```


</td>
<td>

```clj
((local-environment-get newline (section 3 "local-env-get" ((para ("`local-env-get` can be used to reify, turn into value, the local environment.")) (code ((local-env-get))) (program (((let ((a 50)) (local-env-get))))) nil)) newline hline) (render-it clos
```


</td>
</tr>
<tr>
<td>

```clj
(env-get 28)
```


</td>
<td>

```clj
((chapter-environments section 2 "Environments" ((newline (section 3 "env-get" ((para ("`env-get` can be used to reify, turn into value, parts of the global environment." "The global environment is stored as a hashtable and an index into this hashtable" "
```


</td>
</tr>
<tr>
<td>

```clj
(env-get 29)
```


</td>
<td>

```clj
((manual (section 1 "LispBM Runtime Extensions Reference Manual" ((para ("The runtime extensions, if present, can be either compiled" "in a minimal or a full mode." "In the minimal mode only `set-eval-quota` is present." "Minimal mode is the default when 
```


</td>
</tr>
<tr>
<td>

```clj
(env-get 30)
```


</td>
<td>

```clj
((render-manual closure nil (let ((h (fopen "runtimeref.md" "w")) (r (lambda (s) (fwrite-str h s)))) (progn (gc) (var t0 (systime)) (render r manual) (print "Runtime reference manual was generated in " (secs-since t0) " seconds"))) nil) (pretty closure (c
```


</td>
</tr>
<tr>
<td>

```clj
(env-get 31)
```


</td>
<td>

```clj
((ind-spaces closure (n) (str-replicate n 32b) nil) (dot-it closure (i x) (match x (((? x) ? xs) (let ((node (str-merge "cons" (to-str i))) ((c1 str1) (dot-it (shl i 1) x)) ((c2 str2) (dot-it (+ 1 (shl i 1)) xs))) (list node (str-merge "   " node " [label
```


</td>
</tr>
</table>




---


### env-set

`env-set` destructively sets an entry in the global environment hashtable. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>


```clj
(if (eq (env-get 1) nil) (env-set 1 (list '(a . 75))))
(env-get 1)

```


</td>
<td>


```clj
((a . 75))
```


</td>
</tr>
</table>

Note that in the example code above there is no guarantee that the symbol `a` actually hashes to index 1 in the environment table. So `a` is most likely impossible to look up from this environment. The use case for `env-set` and `env-get` are rather that they are together.  Use `env-get` to extract index `i` from the table, then modify it in some way and end by using `env-set` to the same index `i`. 




---


### local-env-get

`local-env-get` can be used to reify, turn into value, the local environment. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(local-env-get)
```


</td>
<td>

```clj
nil
```


</td>
</tr>
</table>

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>


```clj
(let ((a 50))
     (local-env-get))

```


</td>
<td>


```clj
((a . 50))
```


</td>
</tr>
</table>




---

## GC


### set-gc-stack-size

With `set-gc-stack-size` you can change the size of the stack used for heap traversal by the garbage collector. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(set-gc-stack-size 100)
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

## Memory


### mem-num-free

`mem-num-free` returns the number of free words in the LBM memory. This is the memory where arrays and strings are stored. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(mem-num-free)
```


</td>
<td>

```clj
255954
```


</td>
</tr>
</table>




---


### mem-longest-free

`mem-longest-free` returns the length in words of the longest consecutive sequence of free words in the LBM memory. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(mem-num-free)
```


</td>
<td>

```clj
255884
```


</td>
</tr>
</table>




---


### lbm-heap-state

`lbm-heap-state` can be used to query information about heap usage. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(lbm-heap-state 'get-heap-size)
```


</td>
<td>

```clj
10000000u
```


</td>
</tr>
<tr>
<td>

```clj
(lbm-heap-state 'get-heap-bytes)
```


</td>
<td>

```clj
80000000u
```


</td>
</tr>
<tr>
<td>

```clj
(lbm-heap-state 'get-num-alloc-cells)
```


</td>
<td>

```clj
8681u
```


</td>
</tr>
<tr>
<td>

```clj
(lbm-heap-state 'get-num-alloc-arrays)
```


</td>
<td>

```clj
703u
```


</td>
</tr>
<tr>
<td>

```clj
(lbm-heap-state 'get-gc-num)
```


</td>
<td>

```clj
1u
```


</td>
</tr>
<tr>
<td>

```clj
(lbm-heap-state 'get-gc-num-marked)
```


</td>
<td>

```clj
2394u
```


</td>
</tr>
<tr>
<td>

```clj
(lbm-heap-state 'get-gc-num-recovered-cells)
```


</td>
<td>

```clj
9997606u
```


</td>
</tr>
<tr>
<td>

```clj
(lbm-heap-state 'get-gc-num-recovered-arrays)
```


</td>
<td>

```clj
0u
```


</td>
</tr>
<tr>
<td>

```clj
(lbm-heap-state 'get-gc-num-least-free)
```


</td>
<td>

```clj
9997606u
```


</td>
</tr>
<tr>
<td>

```clj
(lbm-heap-state 'get-gc-num-last-free)
```


</td>
<td>

```clj
9997606u
```


</td>
</tr>
</table>




---

## Scheduling


### set-eval-quota

`set-eval-quota` sets the number of evaluation steps that is given to each context when given turn to execute by the round-robin scheduler. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(set-eval-quota 30)
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

