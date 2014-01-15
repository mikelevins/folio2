# folio reference
version 2.0

## as

A generic, extensible conversion utility.

**Package:** net.bardcode.folio.as<br>
**Exports:** as

**as** exports a single symbol named **as**, which names a generic function. The function **as** is an extensible type-conversion utility. It can be specialized as-needed to provide  conversions among arbitrary types.

The exportation of a single name is intentional. It's meant to make it easy and convenient to `USE` or `IMPORT` the **as** function and extend it as-needed.

### Reference

**as** *type* *value* &key &allow-other-keys  => Anything  &nbsp;&nbsp;&nbsp;&nbsp;[*Generic function*]<br>
Returns a new value of type *type* that is in some sense equivalent
to *value*. 

**#[&lt;type&gt;]** &nbsp;&nbsp;&nbsp;&nbsp;[*Reader macro*]<br>
The reader macro #[&lt;type&gt;] is a **type constraint**. It attempts to convert the next value read to the specified type. For example, 

    CL-USER> '(1 2)
    (1 2)
    
but:
    
    CL-USER> #[vector]'(1 2)
    #(1 2)

The reader macro works only when there is a method defined on **as** that is applicable to the supplied value and type.

## boxes
Mutable containers.

**Package:** net.bardcode.folio.boxes<br>
**Exports:** box box? set-box! unbox

The **box** type is a mutable container for arbitrary values. The functions **unbox** and **set-box!** can be used to retrieve and replace the value stored in a **box**.

Boxes provide a way to introduce mutability piecemeal into immutable and pure-functional data structures. As an example, we can use a pure-functional implementation of finite maps, but store the value elements in boxes so that they can be destructively updated. All operations on the finite map as a whole and on its keys remain purely functional, but we can destructively modify the values stored on the keys.

A type definition establishes the **box** type as a synonym for a **cons** cell whose **car** element is the keyword **:BOX**.

### Reference

**as** 'box *value* => *box*  &nbsp;&nbsp;&nbsp;&nbsp;[*Generic function*]<br>
Returns a new box that contains *value*.

**box**  &nbsp;&nbsp;&nbsp;&nbsp;[*Type*]<br>
A **box** is a mutable container for a value. folio represents a **box** as a
**cons** whose **car** is the keyword **:box**.

**box** *value* => *box*  &nbsp;&nbsp;&nbsp;&nbsp;[*Generic function*]<br>
Returns a new **box** containing *value*.

**box?** *value* => Generalized Boolean  &nbsp;&nbsp;&nbsp;&nbsp;[*Generic function*]<br>
Returns a true value if *value* is a **box**, and a false value otherwise.

**make** 'box &key (*value* nil) => Anything  &nbsp;&nbsp;&nbsp;&nbsp;[*Generic function*]<br>
Returns a new **box** whose initial value is *value*.

**set-box!** *box* *value* => Anything  &nbsp;&nbsp;&nbsp;&nbsp;[*Generic function*]<br>
Replaces the value of *box* with *value*, returning *value*.

**unbox** box => Anything  &nbsp;&nbsp;&nbsp;&nbsp;[*Generic function*]<br>
Returns the value stored in *box*.

**setf** (unbox *box*) *value* => *value*  &nbsp;&nbsp;&nbsp;&nbsp;[*Generic function*]<br>
Replaces the value of *box* with *value*, returning *value*.

## functions
Tools for working with functions.

**Package:** net.bardcode.folio.functions<br>
* **Exports:** $ ^ -> cascade compose conjoin disjoin flip fn function? functional?
           generic-function? method? partial rpartial

The **functions** package provides a set of tools for working with functions and for making functional style more convenient in Common Lisp.

### Reference

**$** (*fn* *arg1* ... *argk*) *body*  => Anything  &nbsp;&nbsp;&nbsp;&nbsp;[*Macro*]<br>
A compact synonym for Common Lisp FUNCALL. This macro is not intended as a replacement for FUNCALL, but as a convenience for cases where the longer name would be bulky or awkward, or when the clarity of functional code benefits from brevity.

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


## make
A generic, extensible utility for constructing values.

**Package:** net.bardcode.folio.make<br>
**Exports:** make

The **make** package exports the extensible generic function **make**. By specializing this generic function you can implement custom constructors for arbitrary types.

**make** is defined to accept keyword arguments, so that your specializations can use keyword parameters to exercise fine control over constructor behavior.

The exportation of a single name is intentional. It's meant to make it easy and convenient to `USE` or `IMPORT` the **make** function and extend it as-needed.

### Reference

**make** *type* &key &allow-other-keys  => Anything  &nbsp;&nbsp;&nbsp;&nbsp;[*Generic function*]<br>
Returns a new instance of *type*, constructed according to the specifications passed in the keyword parameters.

## maps
Finite map types with a common API

**Package:** net.bardcode.folio.maps<br>
**Exports:** alist alist?
   contains-key? contains-value?
   get-key
   keys
   map map? :merge
   plist plist? put-key
   values
   wb-map wb-map?

The **maps** package provides implementations of functional finite maps and a uniform API for working with them. Implementations of the **maps** functions are provided for **alists**, **plists**, and FSet's **wb-map** representation of finite maps. Adding additional representations of maps is a simple matter of specializing the generic functions in the **Maps** package.

An obvious question is why the **Maps** functions are not specialized on Common Lisp's **hash-table** type; surely a hash-table is a kind of finite map? 

The reason that folio doesn't specialize the **Maps** functions for **hash-table** is that the **Maps** protocol is a functional protocol; it assumes the maps it operates on are immutable. The whole advantage of a hash-table is that it provides efficient lookup and update through destructive modification of the table. It's certainly *possible* to implement a purely-functional API for Common Lisp's hash-tables, but it would be perverse to do so, since it would squander all of the performance advantages of hash-tables.


### Reference

<br>
**alist**   &nbsp;&nbsp;&nbsp;&nbsp;[*Type*]<br>
A Lisp list whose elements are pairs, represented as **cons** cells. The left element of each pair is a key; the right element is a value associated with the key.

<br>
**alist** (*key1* . *value1*) (*key2* . *value2*) ...  (*keyk* . *valuek*) => *alist* &nbsp;&nbsp;&nbsp;&nbsp;[*Macro*]<br>
Returns a new **map** represented as an **alist**. The keys and values are as given by *key1*...*keyk* and *value1*...*valuek*.

<br>
**alist?** *thing* => Boolean  &nbsp;&nbsp;&nbsp;&nbsp;[*Generic function*]<br>
Returns true if *thing* appears to be an **alist**, and false otherwise. **alist?** considers a value an **alist** if it's a **cons**, if it's a proper list, and if each element is also a **cons**.

<br>
**contains-key?** *map* *key* &key (test 'eql) =>   &nbsp;&nbsp;&nbsp;&nbsp;[*Generic function*]<br>
Returns true if *map* contains *key*, and false otherwise. The function passed in the **test** parameter is used to test whether *key* matches a key in *map*.

**test** is ignored when *map* is a **wb-map**.

<br>
**contains-value?** *map* *value* &key (test 'eql)  &nbsp;&nbsp;&nbsp;&nbsp;[*Generic function*]<br>
Returns true if *map* contains *value*, and false otherwise. The function passed in the **test** parameter is used to test whether *value* matches a value in *map*.

**test** is ignored when *map* is a **wb-map**.

<br>
**get-key** *map* *key* &key (test 'eql)(default nil) =>   &nbsp;&nbsp;&nbsp;&nbsp;[*Generic function*]<br>
Returns the value associated in *map* with *key*, or, if *key* is not present in *map*, returns *default*.

**test** is ignored when *map* is a **wb-map**.

<br>
**keys** *map* => *keys*  &nbsp;&nbsp;&nbsp;&nbsp;[*Generic function*]<br>
Returns a sequence of *keys* that appear in *map*.

<br>
**make** 'map &key (contents nil) => *map* &nbsp;&nbsp;&nbsp;&nbsp;[*Generic function*]<br>
Returns a new **map** instance. The keys and values of the new **map** are given as a **plist** passed in **contents**. The representation of the **map** is chosen by folio, but you can pass it to **as** to obtain a particular type of **map**.

<br>
**map?** *thing* => Boolean  &nbsp;&nbsp;&nbsp;&nbsp;[*Generic function*]<br>
Returns true is *thing* is a **map**, and false otherwise.

<br>
**merge** *map1* *map2* => *map3*  &nbsp;&nbsp;&nbsp;&nbsp;[*Generic function*]<br>
Returns a new **map** of the same type as *map1*. The new **map** contains all the keys present in both *map1* and *map2*. Where a key is present in both **maps*, the value from *map2* is chosen to be in *map3*. 

The function passed in the **test** parameter is used to test whether a *key* in *map2* matches a key in *map1*.

<br>
**plist**   &nbsp;&nbsp;&nbsp;&nbsp;[*Type*]<br>
A Lisp list with an even number of elements, representing a **map**. Elements at even-numbered indexes must be atoms, and are treated as keys. Elements at odd-numbered indexes are treated as the values associated with the keys, with each one associated with the atom that immediately precedes it.

<br>
**plist** *key1* *value1* *key2* *value2* ... *keyk* *valuek* => *plist*  &nbsp;&nbsp;&nbsp;&nbsp;[*Function*]<br>
Returns a new **map** represented as a **plist**. The keys and values are as given by *key1*...*keyk* and *value1*...*valuek*.

<br>
**plist?** *thing* => Boolean  &nbsp;&nbsp;&nbsp;&nbsp;[*Function*]<br>
Returns true if *thing* appears to be a **plist**, and false otherwise. **plist?** considers a vaue to be a **plist** if it's a **cons**, if it's a proper list, and if every even-indexed element is an atom.

<br>
**put-key** *map1* *key* *value* &key (test 'eql) => *map2*  &nbsp;&nbsp;&nbsp;&nbsp;[*Generic function*]<br>
Returns a new map that contains all the keys in *map1*, but in addition contains *key* with the associated value *value*. If *map1* contains *key* then its associated value is replaced in *map2* by *value*.

<br>
**values** *map* => *sequence*  &nbsp;&nbsp;&nbsp;&nbsp;[*Generic function*]<br>
Returns a sequence of all the values contained in *map*.

<br>
**wb-map**   &nbsp;&nbsp;&nbsp;&nbsp;[*Type*]<br>
A weight-balanced tree that represents a finite map. This type is provided by the FSet library and supported by folio's **Maps** API.

<br>
**wb-map?** *thing* => Boolean  &nbsp;&nbsp;&nbsp;&nbsp;[*Generic function*]<br>
Returns true if *thing* is an instance of an FSet **wb-map**, and false otherwise.

**{ *key1* *val1* ... *keyK* *valK* }** &nbsp;&nbsp;&nbsp;&nbsp;[*Reader macro*]<br>
The reader macro { *key1* *val1* ... *keyK* *valK* } constructs a **Map** value whose keys are *key1* ... *keyK* and whose corresponding values are *val1* ...  *valK*.

    CL-USER> { :a 1 :b 2}
    { :A 1 :B 2 }

folio chooses the type of map created, but you can use the **as** utility to convert to a type of your choice. In version 2.0, the default map type is FSET:WB-MAP.

## pairs

A simple data structure with left and right slots. An extensible synonym for `cons`.

**Package:** `net.bardcode.folio.pair`<br>
**Exports:** `left pair pair? right set-left! set-right!`

The `pair` type is an extensible synonym for Lisp's `cons` type. Any type for which the `pair` functions are specialized is a pair, which means that you can implement your own representations of pairs. 

A second use for the `pair` functions is to clearly communicate intent: using the `pair` functions with `cons` cells communicates that the programmer intends to treat them as pairs of values, as opposed to any of the many other uses to which `cons` cells can be put.

### Reference

**`as`** *Generic function* <br>
`as 'pair value => pair`<br>
Returns a new `pair` equivalent to `value`. As an example, `(as 'pair (vector 1 2 3))` returns `(1 2 3)`.

**`left`** *Generic function* <br>
`left pair => Anything`<br>
Returns the value of the `left` slot of `pair`.

**`make`** *Generic function* <br>
`make 'pair &key (left nil)(right nil) => pair`<br>
Returns a newly-constructed `pair` whose `left` slot contains `left`, and whose `right` slot contains `right`.

**`pair`** *Generic function* <br>
`pair left right => pair`<br>
Returns a newly-constructed `pair` whose `left` slot contains `left`, and whose `right` slot contains `right`.

**`pair?`** *Generic function* <br>
`pair? p => Generalized Boolean`<br>
Returns a true value if `p` is a `pair`, and a false value otherwise.

**`right`** *Generic function* <br>
`right pair => Anything`<br>
Returns the value of the `right` slot of `pair`.

**`set-left!`** *Generic function* <br>
`set-left! p value => value`<br>
Replaces the value of the `left` slot of `p` with `value`, returning `value`.

**`setf left`** *Generic function* <br>
`setf (left p) val => val`<br>
Replaces the value of the `left` slot of `p` with `value`, returning `value`.

**`set-right!`** *Generic function* <br>
`set-right! p value => value`<br>
Replaces the value of the `right` slot of `p` with `value`, returning `value`.

**`setf right`** *Generic function* <br>
`setf (right p) val => val`<br>
Replaces the value of the `right` slot of `p` with `value`, returning `value`.

## sequences

Objects that represent ordered sequences of values. An extension and generalization of Common Lisp's sequence type.

## series

Objects that represent ordered sequences of values whose length may be unbounded. An extension of sequences to represent collections with possibly infinite numbers of members. An extension to sequences that enables us to conveniently treat iterative procedures as the mapping of functions over (possibly infinite) sequences.

### Reference

<br>
**iterate** *fn* *arg*  => *series*  &nbsp;&nbsp;&nbsp;&nbsp;[*Macro*]<br>
Returns a series whose elements are successive results from the application of *fn*. The series' elements are:

    (*fn* *arg*)
    (*fn* (*fn* *arg*))
    (*fn* (*fn* (*fn* *arg*)))
    ...
    
...and so on.


## taps

Functions that generate series of values from streams, sequences, and other data structures.