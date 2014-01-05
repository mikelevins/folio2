# functions

Tools for working with functions

**Package:** net.bardcode.folio.functions<br>
**Exports:** :^ :-> :cascade :compose :conjoin :disjoin :flip :fn :function? :functional?
           :generic-function? :iterate :method? :partial :rpartial

The **functions** package provides a set of tools for working with functions and for making functional style more convenient in Common Lisp.

## Reference

**^** (*arg1* *arg2* ... *argk*) *body*  => Anything  &nbsp;&nbsp;&nbsp;&nbsp;[*Macro*]<br>
Returns a newly-created function. **^** is shorthand for **lambda**; it accepts the same arguments and has the same effect. Its purpose is to provide a more succinct version of **lambda** for those cases when the longer form would be verbose. An example is in passing an anonymous function inline to a search.

A synonym for **^** is **fn**.

**->** *f1*..*fk* => *fx*  &nbsp;&nbsp;&nbsp;&nbsp;[*Macro*]<br>
Returns a function *fx* that accepts *k* arguments. When applied to *k*
values, the function yields *k* results, applying *f1* to the first argument,
*f2* to the second, and so on. Combines usefully with **cascade**, e.g:

    (cascade (a b c) 
      (-> f1 f2 f3)
      (-> g1 g2 g3)
      (-> h1 h2 h3)) 
    => v1 v2 v3

where *v1* is `(h1 (g1 (f1 a)))`, *v2* is `(h2 (g2 (f2 b)))`, and  *v3* is `(h3 (g3 (f3 c)))`.

<br>
**cascade** (*arg1*..*argk*) *f1*..*fn*) => *val1*..*valk*  &nbsp;&nbsp;&nbsp;&nbsp;[*Macro*]<br>
*f1* through *fn* are all functions that accept *k* arguments and return
*k* values. **cascade** applies *f1* to arguments *arg1*..*argk*. The *k*
output values become the inputs to *f2*. *f2*'s outputs are the inputs
to *f3*, and so on. The outputs of *fn* are *val1*..*valk*

<br>
**compose** *fn1*...*fnk* => *fnx*  &nbsp;&nbsp;&nbsp;&nbsp;[*Macro*]<br>
Returns a new function *fnx* which performs the computations represented by the functions *fn1* through *fnk*.

Passing an argument A to *fnx* has the same effect as evaluating this expression:

    (*fn1* (*fn2* (*fn3*...(*fnk* A))))

<br>
**conjoin** *fn1*...*fnk* => *fnx*  &nbsp;&nbsp;&nbsp;&nbsp;[*Macro*]<br>
Returns a new function *fnx* which performs the computations represented by the functions *fn1* through *fnk* as if they are arguments to an **and** expression.

Passing an argument A to *fnx* has the same effect as evaluating this expression:

    (and (*fn1* A)
         (*fn2* A)
         ...
         (*fnk* A))

<br>
**disjoin** *fn1*...*fnk* => *fnx*  &nbsp;&nbsp;&nbsp;&nbsp;[*Macro*]<br>
Returns a new function *fnx* which performs the computations represented by the functions *fn1* through *fnk* as if they are arguments to an **or** expression.

Passing an argument A to *fnx* has the same effect as evaluating this expression:

    (or (*fn1* A)
         (*fn2* A)
         ...
         (*fnk* A))


<br>
**flip** *fn1*  => *fn2*  &nbsp;&nbsp;&nbsp;&nbsp;[*Function*]<br>
Returns a new function *fn2* that performs the same computation as *fn1*, but taking its arguments in the opposite order. *fn1* must be a function that accepts two required arguments.

<br>
**flip** is a somewhat specialized combinator, but its use-case is surprisingly common. As an example, consider filtering a list values for those that are members of another list. You can write that code this way:

    (filter (partial (flip 'member) $the-list)
            the-values)
            
The reason the call to **flip** is needed is that **member** takes its arguments in the wrong order: its first argument is the object you're testing for membership; its second is the list you're searching for members. **flip** enables us to reverse the order of arguments. (Another approach would be to use **rpartial** instead of **partial** to construct the filter function.)

Because circumstances like this one arise often, **flip** is often useful.

<br>
**fn**   => Anything  &nbsp;&nbsp;&nbsp;&nbsp;[*Macro*]<br>
Returns a newly-created function. **fn** is shorthand for **lambda**; it accepts the same arguments and has the same effect. Its purpose is to provide a more succinct version of **lambda** for those cases when the longer form would be verbose. An example is in passing an anonymous function inline to a search.

A synonym for **fn** is **^**.

<br>
**function?**  *thing*  => Boolean  &nbsp;&nbsp;&nbsp;&nbsp;[*Generic function*]<br>
Returns true if *thing* is a  function and false otherwise.

<br>
**functional?**  *thing*  => Boolean  &nbsp;&nbsp;&nbsp;&nbsp;[*Generic function*]<br>
Returns true if *thing* is a funcallable object of type function, generic function, or method, and false otherwise.

<br>
**generic-function?**  *thing*  => Boolean  &nbsp;&nbsp;&nbsp;&nbsp;[*Generic function*]<br>
Returns true if *thing* is a generic function and false otherwise.

<br>
**iterate** *fn* *arg*  => *series*  &nbsp;&nbsp;&nbsp;&nbsp;[*Macro*]<br>
Returns a series whose elements are successive results from the application of *fn*. The series' elements are:

    (*fn* *arg*)
    (*fn* (*fn* *arg*))
    (*fn* (*fn* (*fn* *arg*)))
    ...
    
...and so on.

<br>
**method?** *thing*  => Boolean  &nbsp;&nbsp;&nbsp;&nbsp;[*Generic function*]<br>
Returns true if *thing* is a method and false otherwise.

<br>
**partial** *f1* *arg1*..*argk* => *f2*  &nbsp;&nbsp;&nbsp;&nbsp;[*Function*]<br>
Returns a left section of the function *f1*, created by partially applying *f1* to the arguments *arg1* through *argk*. These arguments must be some of the arguments required by *f1*; *rpartial* partially applies *f1* to them binding them to the leftmost required arguments. 

As an example, if the function **foo** requires four arguments named a, b, c, and d, then

    (partial 'foo 1 2)
    
creates a left section in which the parameters a and b are bound to the values 1 and 2. Applying the left section to the values 3 and 4 then bind c and d to 3 and 4 and then compute and return the result of the call to foo.

<br>
**rpartial** *f1* *arg1*..*argk* => *f2*  &nbsp;&nbsp;&nbsp;&nbsp;[*Function*]<br>
Returns a right section of the function *f1*, created by partially applying *f1* to the arguments *arg1* through *argk*. These arguments must be some of the arguments required by *f1*; *rpartial* partially applies *f1* to them binding them to the rightmost required arguments.

As an example, if the function **foo** requires four arguments named a, b, c, and d, then

    (rpartial 'foo 3 4)
    
creates a right section in which the parameters c and d are bound to the values 3 and 4. Applying the right section to the values 1 and 2 then bind a and b to 1 and 2 and then compute and return the result of the call to foo.

