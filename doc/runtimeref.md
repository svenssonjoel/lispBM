# LispBM Runtime Extensions Reference Manual


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

