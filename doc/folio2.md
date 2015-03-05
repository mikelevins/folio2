# folio 2 reference
version 2.0.2

## as

A generic, extensible conversion utility.

**Package:** net.bardcode.folio.as<br>
**Exports:** as

**as** exports a single symbol named **as**, which names a generic
  function. The function **as** is an extensible type-conversion
  utility. It can be specialized as-needed to provide conversions
  among arbitrary types.

The exportation of a single name is intentional. It's meant to make it
easy and convenient to `USE` or `IMPORT` the **as** function and
extend it as-needed.

#### Reference

**as** *type* *value* &key &allow-other-keys  => Anything  &nbsp;&nbsp;&nbsp;&nbsp;[*Generic function*]<br>

Returns a new value of type *type* that is in some sense equivalent to
*value*.

**#[&lt;type&gt;]** &nbsp;&nbsp;&nbsp;&nbsp;[*Reader macro*]<br>

The reader macro #[&lt;type&gt;] is a **type constraint**. It attempts
to convert the next value read to the specified type. For example,

    CL-USER> '(1 2)
    (1 2)
    
but:
    
    CL-USER> #[vector]'(1 2)
    #(1 2)

The reader macro works only when there is a method defined on **as**
that is applicable to the supplied value and type.

## boxes
Mutable containers.

**Package:** net.bardcode.folio.boxes<br>
**Exports:** box box? set-box! unbox

The **box** type is a mutable container for arbitrary values. The
functions **unbox** and **set-box!** can be used to retrieve and
replace the value stored in a **box**.

Boxes provide a way to introduce mutability piecemeal into immutable
and pure-functional data structures. As an example, we can use a
pure-functional implementation of finite maps, but store the value
elements in boxes so that they can be destructively updated. All
operations on the finite map as a whole and on its keys remain purely
functional, but we can destructively modify the values stored on the
keys.

A type definition establishes the **box** type as a synonym for a
**cons** cell whose **car** element is the keyword **:BOX**.

#### Reference

**as** 'box *value* => *box*  &nbsp;&nbsp;&nbsp;&nbsp;[*Generic function*]<br>

Returns a new box that contains *value*.

**box**  &nbsp;&nbsp;&nbsp;&nbsp;[*Type*]<br>

A **box** is a mutable container for a value. folio represents a
**box** as a **cons** whose **car** is the keyword **:box**.

**box** *value* => *box*  &nbsp;&nbsp;&nbsp;&nbsp;[*Generic function*]<br>

Returns a new **box** containing *value*.

**box?** *value* => Generalized Boolean  &nbsp;&nbsp;&nbsp;&nbsp;[*Generic function*]<br>

Returns a true value if *value* is a **box**, and a false value
otherwise.

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

The **functions** package provides a set of tools for working with
functions and for making functional style more convenient in Common
Lisp.

#### Reference

**$** *fn* *arg1* *arg2* ... *argk*  => Anything  &nbsp;&nbsp;&nbsp;&nbsp;[*Macro*]<br>

A compact synonym for Common Lisp FUNCALL. This macro is not intended
as a replacement for FUNCALL, but as a convenience for cases where the
longer name would be bulky or awkward, or when the clarity of
functional code benefits from brevity.

**^** (*arg1* *arg2* ... *argk*) *body*  => Anything  &nbsp;&nbsp;&nbsp;&nbsp;[*Macro*]<br>

Returns a newly-created function. **^** is shorthand for **lambda**;
it accepts the same arguments and has the same effect. Its purpose is
to provide a more succinct version of **lambda** for those cases when
the longer form would be verbose. An example is in passing an
anonymous function inline to a search.

A synonym for **^** is **fn**.

**->** *f1*..*fk* => *fx*  &nbsp;&nbsp;&nbsp;&nbsp;[*Macro*]<br>

Returns a function *fx* that accepts *k* arguments. When applied to
*k* values, the function yields *k* results, applying *f1* to the
first argument, *f2* to the second, and so on. Combines usefully with
**cascade**, e.g:

    (cascade (a b c) 
      (-> f1 f2 f3)
      (-> g1 g2 g3)
      (-> h1 h2 h3)) 
    => v1 v2 v3

where *v1* is `(h1 (g1 (f1 a)))`, *v2* is `(h2 (g2 (f2 b)))`, and *v3*
is `(h3 (g3 (f3 c)))`.

<br>

**cascade** (*arg1*..*argk*) *f1*..*fn*) => *val1*..*valk*  &nbsp;&nbsp;&nbsp;&nbsp;[*Macro*]<br>

*f1* through *fn* are all functions that accept *k* arguments and
*return k* values. **cascade** applies *f1* to arguments
**arg1*..*argk*. The *k* output values become the inputs to
**f2*. *f2*'s outputs are the inputs to *f3*, and so on. The outputs
*of *fn* are *val1*..*valk*

<br>
**compose** *fn1*...*fnk* => *fnx*  &nbsp;&nbsp;&nbsp;&nbsp;[*Macro*]<br>

Returns a new function *fnx* which performs the computations
represented by the functions *fn1* through *fnk*.

Passing an argument A to *fnx* has the same effect as evaluating this
expression:

    (*fn1* (*fn2* (*fn3*...(*fnk* A))))

<br>

**conjoin** *fn1*...*fnk* => *fnx*
  &nbsp;&nbsp;&nbsp;&nbsp;[*Macro*]<br>

Returns a new function *fnx* which performs the computations
represented by the functions *fn1* through *fnk* as if they are
arguments to an **and** expression.

Passing an argument A to *fnx* has the same effect as evaluating this
expression:

    (and (*fn1* A)
         (*fn2* A)
         ...
         (*fnk* A))

<br>
**disjoin** *fn1*...*fnk* => *fnx*  &nbsp;&nbsp;&nbsp;&nbsp;[*Macro*]<br>

Returns a new function *fnx* which performs the computations
represented by the functions *fn1* through *fnk* as if they are
arguments to an **or** expression.

Passing an argument A to *fnx* has the same effect as evaluating this
expression:

    (or (*fn1* A)
         (*fn2* A)
         ...
         (*fnk* A))


<br>
**flip** *fn1*  => *fn2*  &nbsp;&nbsp;&nbsp;&nbsp;[*Function*]<br>

Returns a new function *fn2* that performs the same computation as
*fn1*, but taking its arguments in the opposite order. *fn1* must be a
function that accepts two required arguments.

<br>

**flip** is a somewhat specialized combinator, but its use-case
is surprisingly common. As an example, consider filtering a list
values for those that are members of another list. You can write that
code this way:

    (filter (partial (flip 'member) $the-list)
            the-values)
            
The reason the call to **flip** is needed is that **member** takes its
arguments in the wrong order: its first argument is the object you're
testing for membership; its second is the list you're searching for
members. **flip** enables us to reverse the order of
arguments. (Another approach would be to use **rpartial** instead of
**partial** to construct the filter function.)

Because circumstances like this one arise often, **flip** is often
useful.

<br>
**fn**   => Anything  &nbsp;&nbsp;&nbsp;&nbsp;[*Macro*]<br>

Returns a newly-created function. **fn** is shorthand for **lambda**;
it accepts the same arguments and has the same effect. Its purpose is
to provide a more succinct version of **lambda** for those cases when
the longer form would be verbose. An example is in passing an
anonymous function inline to a search.

A synonym for **fn** is **^**.

<br>
**function?**  *thing*  => Boolean  &nbsp;&nbsp;&nbsp;&nbsp;[*Generic function*]<br>

Returns true if *thing* is a function and false otherwise.

<br>
**functional?**  *thing*  => Boolean  &nbsp;&nbsp;&nbsp;&nbsp;[*Generic function*]<br>

Returns true if *thing* is a funcallable object of type function,
generic function, or method, and false otherwise.

<br>
**generic-function?**  *thing*  => Boolean  &nbsp;&nbsp;&nbsp;&nbsp;[*Generic function*]<br>

Returns true if *thing* is a generic function and false otherwise.

<br>

**method?** *thing*  => Boolean  &nbsp;&nbsp;&nbsp;&nbsp;[*Generic function*]<br>

Returns true if *thing* is a method and false otherwise.

<br>
**partial** *f1* *arg1*..*argk* => *f2*  &nbsp;&nbsp;&nbsp;&nbsp;[*Function*]<br>

Returns a left section of the function *f1*, created by partially
applying *f1* to the arguments *arg1* through *argk*. These arguments
must be some of the arguments required by *f1*; *rpartial* partially
applies *f1* to them binding them to the leftmost required arguments.

As an example, if the function **foo** requires four arguments named
a, b, c, and d, then

    (partial 'foo 1 2)
    
creates a left section in which the parameters a and b are bound to
the values 1 and 2. Applying the left section to the values 3 and 4
then bind c and d to 3 and 4 and then compute and return the result of
the call to foo.

<br>
**rpartial** *f1* *arg1*..*argk* => *f2*  &nbsp;&nbsp;&nbsp;&nbsp;[*Function*]<br>

Returns a right section of the function *f1*, created by partially
applying *f1* to the arguments *arg1* through *argk*. These arguments
must be some of the arguments required by *f1*; *rpartial* partially
applies *f1* to them binding them to the rightmost required arguments.

As an example, if the function **foo** requires four arguments named
a, b, c, and d, then

    (rpartial 'foo 3 4)
    
creates a right section in which the parameters c and d are bound to
the values 3 and 4. Applying the right section to the values 1 and 2
then bind a and b to 1 and 2 and then compute and return the result of
the call to foo.


## make

A generic, extensible utility for constructing values.

**Package:** net.bardcode.folio.make<br>
**Exports:** make

The **make** package exports the extensible generic function
**make**. By specializing this generic function you can implement
custom constructors for arbitrary types.

**make** is defined to accept keyword arguments, so that your
  specializations can use keyword parameters to exercise fine control
  over constructor behavior.

The exportation of a single name is intentional. It's meant to make it
easy and convenient to `USE` or `IMPORT` the **make** function and
extend it as-needed.

#### Reference

**make** *type* &key &allow-other-keys  => Anything  &nbsp;&nbsp;&nbsp;&nbsp;[*Generic function*]<br>

Returns a new instance of *type*, constructed according to the
specifications passed in the keyword parameters.

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

The **maps** package provides implementations of functional finite
maps and a uniform API for working with them. Implementations of the
**maps** functions are provided for **alists**, **plists**, and FSet's
**wb-map** representation of finite maps. Adding additional
representations of maps is a simple matter of specializing the generic
functions in the **Maps** package.

An obvious question is why the **Maps** functions are not specialized
on Common Lisp's **hash-table** type; surely a hash-table is a kind of
finite map?

The reason that folio doesn't specialize the **Maps** functions for
**hash-table** is that the **Maps** protocol is a functional protocol;
it assumes the maps it operates on are immutable. The whole advantage
of a hash-table is that it provides efficient lookup and update
through destructive modification of the table. It's certainly
*possible* to implement a purely-functional API for Common Lisp's
hash-tables, but it would be perverse to do so, since it would
squander all of the performance advantages of hash-tables.


#### Reference

<br>
**alist**   &nbsp;&nbsp;&nbsp;&nbsp;[*Type*]<br>

A Lisp list whose elements are pairs, represented as **cons**
cells. The left element of each pair is a key; the right element is a
value associated with the key.

<br>
**alist** (*key1* . *value1*) (*key2* . *value2*) ...  (*keyk* . *valuek*) => *alist* &nbsp;&nbsp;&nbsp;&nbsp;[*Macro*]<br>

Returns a new **map** represented as an **alist**. The keys and values
are as given by *key1*...*keyk* and *value1*...*valuek*.

<br>
**alist?** *thing* => Boolean  &nbsp;&nbsp;&nbsp;&nbsp;[*Generic function*]<br>

Returns true if *thing* appears to be an **alist**, and false
otherwise. **alist?** considers a value an **alist** if it's a
**cons**, if it's a proper list, and if each element is also a
**cons**.

<br>
**contains-key?** *map* *key* &key (test 'eql) =>   &nbsp;&nbsp;&nbsp;&nbsp;[*Generic function*]<br>

Returns true if *map* contains *key*, and false otherwise. The
function passed in the **test** parameter is used to test whether
*key* matches a key in *map*.

**test** is ignored when *map* is a **wb-map**.

<br>
**contains-value?** *map* *value* &key (test 'eql)  &nbsp;&nbsp;&nbsp;&nbsp;[*Generic function*]<br>

Returns true if *map* contains *value*, and false otherwise. The
function passed in the **test** parameter is used to test whether
*value* matches a value in *map*.

**test** is ignored when *map* is a **wb-map**.

<br>
**get-key** *map* *key* &key (test 'eql)(default nil) =>   &nbsp;&nbsp;&nbsp;&nbsp;[*Generic function*]<br>

Returns the value associated in *map* with *key*, or, if *key* is not
present in *map*, returns *default*.

**test** is ignored when *map* is a **wb-map**.

<br>
**keys** *map* => *keys*  &nbsp;&nbsp;&nbsp;&nbsp;[*Generic function*]<br>

Returns a sequence of *keys* that appear in *map*.

<br>
**make** 'map &key (contents nil) => *map* &nbsp;&nbsp;&nbsp;&nbsp;[*Generic function*]<br>

Returns a new **map** instance. The keys and values of the new **map**
are given as a **plist** passed in **contents**. The representation of
the **map** is chosen by folio, but you can pass it to **as** to
obtain a particular type of **map**.

<br>
**map?** *thing* => Boolean  &nbsp;&nbsp;&nbsp;&nbsp;[*Generic function*]<br>

Returns true if *thing* is a **map**, and false otherwise.

<br>
**merge** *map1* *map2* => *map3*  &nbsp;&nbsp;&nbsp;&nbsp;[*Generic function*]<br>

Returns a new **map** of the same type as *map1*. The new **map**
contains all the keys present in both *map1* and *map2*. Where a key
is present in both **maps*, the value from *map2* is chosen to be in
*map3*.

The function passed in the **test** parameter is used to test whether
a *key* in *map2* matches a key in *map1*.

<br>
**plist**   &nbsp;&nbsp;&nbsp;&nbsp;[*Type*]<br>

A Lisp list with an even number of elements, representing a
**map**. Elements at even-numbered indexes must be atoms, and are
treated as keys. Elements at odd-numbered indexes are treated as the
values associated with the keys, with each one associated with the
atom that immediately precedes it.

<br>
**plist** *key1* *value1* *key2* *value2* ... *keyk* *valuek* => *plist*  &nbsp;&nbsp;&nbsp;&nbsp;[*Function*]<br>

Returns a new **map** represented as a **plist**. The keys and values
are as given by *key1*...*keyk* and *value1*...*valuek*.

<br>
**plist?** *thing* => Boolean  &nbsp;&nbsp;&nbsp;&nbsp;[*Function*]<br>

Returns true if *thing* appears to be a **plist**, and false
otherwise. **plist?** considers a vaue to be a **plist** if it's a
**cons**, if it's a proper list, and if every even-indexed element is
an atom.

<br>
**put-key** *map1* *key* *value* &key (test 'eql) => *map2*  &nbsp;&nbsp;&nbsp;&nbsp;[*Generic function*]<br>

Returns a new map that contains all the keys in *map1*, but in
addition contains *key* with the associated value *value*. If *map1*
contains *key* then its associated value is replaced in *map2* by
*value*.

<br>
**values** *map* => *sequence*  &nbsp;&nbsp;&nbsp;&nbsp;[*Generic function*]<br>

Returns a sequence of all the values contained in *map*.

<br>
**wb-map**   &nbsp;&nbsp;&nbsp;&nbsp;[*Type*]<br>

A weight-balanced tree that represents a finite map. This type is
provided by the FSet library and supported by folio's **Maps** API.

<br>
**wb-map?** *thing* => Boolean  &nbsp;&nbsp;&nbsp;&nbsp;[*Generic function*]<br>

Returns true if *thing* is an instance of an FSet **wb-map**, and
false otherwise.

**{ *key1* *val1* ... *keyK* *valK* }** &nbsp;&nbsp;&nbsp;&nbsp;[*Reader macro*]<br>

The reader macro { *key1* *val1* ... *keyK* *valK* } constructs a
**Map** value whose keys are *key1* ... *keyK* and whose corresponding
values are *val1* ...  *valK*.

    CL-USER> { :a 1 :b 2}
    { :A 1 :B 2 }

folio chooses the type of map created, but you can use the **as**
utility to convert to a type of your choice. In version 2.0, the
default map type is FSET:WB-MAP.

## pairs

A simple data structure with left and right slots. An extensible
synonym for `cons`.

**Package:** `net.bardcode.folio.pair`<br>
**Exports:** `left pair pair? right set-left! set-right!`

The `pair` type is an extensible synonym for Lisp's `cons` type. Any
type for which the `pair` functions are specialized is a pair, which
means that you can implement your own representations of pairs.

A second use for the `pair` functions is to clearly communicate
intent: using the `pair` functions with `cons` cells communicates that
the programmer intends to treat them as pairs of values, as opposed to
any of the many other uses to which `cons` cells can be put.

#### Reference

**`as`** *Generic function* <br>
`as 'pair value => pair`<br>

Returns a new `pair` equivalent to `value`. As an example, `(as 'pair
(vector 1 2 3))` returns `(1 2 3)`.

**`left`** *Generic function* <br>
`left pair => Anything`<br>

Returns the value of the `left` slot of `pair`.

**`make`** *Generic function* <br>
`make 'pair &key (left nil)(right nil) => pair`<br>

Returns a newly-constructed `pair` whose `left` slot contains `left`,
and whose `right` slot contains `right`.

**`pair`** *Generic function* <br>
`pair left right => pair`<br>

Returns a newly-constructed `pair` whose `left` slot contains `left`,
and whose `right` slot contains `right`.

**`pair?`** *Generic function* <br>
`pair? p => Generalized Boolean`<br>

Returns a true value if `p` is a `pair`, and a false value otherwise.

**`right`** *Generic function* <br>
`right pair => Anything`<br>

Returns the value of the `right` slot of `pair`.

**`set-left!`** *Generic function* <br>
`set-left! p value => value`<br>

Replaces the value of the `left` slot of `p` with `value`, returning
`value`.

**`setf left`** *Generic function* <br>
`setf (left p) val => val`<br>

Replaces the value of the `left` slot of `p` with `value`, returning
`value`.

**`set-right!`** *Generic function* <br>
`set-right! p value => value`<br>

Replaces the value of the `right` slot of `p` with `value`, returning
`value`.

**`setf right`** *Generic function* <br>
`setf (right p) val => val`<br>

Replaces the value of the `right` slot of `p` with `value`, returning
`value`.

## sequences

Objects that represent ordered sequences of values. An extension and
generalization of Common Lisp's sequence type.

**[** &nbsp;&nbsp;&nbsp;&nbsp;[*Reader macro*]<br>

**[ *val1* ... *valK* }** &nbsp;&nbsp;&nbsp;&nbsp;[*Reader macro*]<br>

The reader macro `[` *val1* ...  *valK* `]` constructs a sequence
value whose values are *val1* ...  *valK*.

    CL-USER> [1 2 3]
    (1 2 3)

folio chooses the type of sequence to construct. You can use `as` to
obtain a sequence of a specified value.


**`acons`** *Generic function* <br>
`acons k v sequence => sequence'`<br>

Returns a new sequence to which the pair `(k . v)` has been added as
an element. `acons` extends Common Lisp's `ACONS`, enabling values of
type `VECTOR` and `FSET:WB-SEQ` to be treated as association lists.

**`add-first`** *Generic function* <br>
`add-first val sequence => sequence'`<br>

Returns a new sequence whose first element is `val` and whose
remaining elements are the elements of `sequence`. Similar to Common
Lisp's `CONS`, except that `SEQUENCE` may be a value of any type that
folio treats as a sequence.

**`add-last`** *Generic function* <br>
`add-last sequence val => sequence'`<br>

Returns a new sequence whose last element is `val` and whose remaining
elements are the elements of `sequence`. Similar to `add-first` except
that it adds the new value to the end of the sequence rather than the
beginning.

**`any`** *Generic function* <br>
`any sequence => Anything`<br>

Returns a randomly-selected element of `sequence`. `any` internally
uses Common Lisp `RANDOM` to choose the element returned; the
selection of an element to return is therefore affected by the value
of `*RANDOM-STATE*`, and by the implementation of `RANDOM`.

**`append`** *Function* <br>
`append sequence1 sequence2 ... sequenceN => sequence'`<br>

Returns a new sequence constructed by appending the input sequences
one after another in order. folio attempts to make a result that is of
the same type, or failing that, a generalization of the type of the
leftmost input sequence. `append` is a generalization of Common Lisp
`APPEND` that works with all types that folio treats as sequences, and
that can be further extended by writing methods on `binary-append`.

**`apportion`** *Function* <br>
`apportion sequence &rest fn1 fn2 ... fnN => sequence1 sequence2 ... sequenceN`<br>

Returns a number of sequences equal to the number of functions `fn`,
`fn2`, ... `fnN`. `apportion` filters the elements of `sequence` by
applying each of the input functions to each element of `sequence` in
order. Those elements for which `fn1` returns a true value become
elements of `sequence1`; those for which `fn2` returns a true value
become elements of `sequence2`; and so on.

**`assoc`** *Generic function* <br>
`assoc k sequence &key (key 'cl:identity) (test 'cl:eql) => Anything`<br>

Returns the first element of `sequence` whose key is the same as `k`,
where the key is defined as the value returned by applying the `key`
parameter to the element, and "the same" is defined by applying the
`test` to `k` and the value returned by the `key`.`assoc` extends
Common Lisp's `ASSOC`, enabling values of type `VECTOR` and
`FSET:WB-SEQ` to be treated as association lists.

**`assoc-if`** *Generic function* <br>
`assoc-if predicate sequence &key (key 'cl:identity) => Anything`<br>

Applies `key` to each element of `sequence`, and then applies
`predicate` to the value returned by `key`. Returns the first element
for which `predicate` returns a true value. `assoc-if` extends Common
Lisp's `ASSOC-IF`, enabling values of type `VECTOR` and `FSET:WB-SEQ`
to be treated as association lists.

**`assoc-if-not`** *Generic function* <br>
`assoc-if-not predicate sequence &key (key 'cl:identity) => Anything`<br>

Applies `key` to each element of `sequence`, and then applies
`predicate` to the value returned by `key`. Returns the first element
for which `predicate` returns a false value. `assoc-if-not` extends
Common Lisp's `ASSOC-IF-NOT`, enabling values of type `VECTOR` and
`FSET:WB-SEQ` to be treated as association lists.


**`binary-append`** *Function* <br>
`binary-append sequence1 sequence2 => sequence'`<br>

Returns a new sequence constructed by appending the input sequences
one after the other in order. folio attempts to make a result that is
of the same type, or failing that, a generalization of the type of the
leftmost input sequence. Normally it's more convenient to call
`append`, which operates on any number of sequences. The primary use
of `binary-append` is in extending `append` to work with new types:
adding `binary-append` methods that are specialized on new types
enables `append` to work with those types.

**`by`** *Generic function* <br>
`by n sequence => sequence'`<br>

Returns a new sequence made up of subsequences of `sequence`, each of
length `n`. `by` splits the input sequence into subsequences of length
`n`; for example,

    (by 3 "abcdefghi") => #("abc" "def" "ghi")
    
If the number of elements in `sequence` is not a whole multiple of `n`
then the last subsequence is shorter than `n`. This also means that if
`n` is greater than the length of `sequence`, then the result sequence
contains a single subsequence that is equal to `sequence`.

**`count`** *Generic function* <br>
`count item sequence &key (start 0) (end nil) (key 'cl:identity) (test 'cl:eql) => Integer`<br>

Returns a count of elements in `sequence` that are the same as
`item`. "The same as" means that applying `key` to the element returns
a value that is equal to `item`, with `test` used as the equality
test. `count` extends Common Lisps's `COUNT` to work with all types
that folio treats as sequences.

**`count-if`** *Generic function* <br>
`count-if predicate sequence &key (start 0) (end nil) (key 'cl:identity) => Integer`<br>

Returns a count of elements in `sequence` satisfy
`predicate`. `predicate` is applied to the value produced by applying
`key` to each element, and the count is incremented for each element
that produces a true result. `count-if` extends Common Lisp's
`COUNT-IF` to work with all types that folio treats as sequences.

**`count-if-not`** *Generic function* <br>
`count-if-not predicate sequence &key (start 0) (end nil) (key 'cl:identity) => Integer`<br>

Returns a count of elements in `sequence` do not sastisfy
`predicate`. `predicate` is applied to the value produced by applying
`key` to each element, and the count is incremented for each element
that produces a false result. `count-if-not` extends Common Lisp's
`COUNT-IF-NOT` to work with all types that folio treats as sequences.

**`dispose`** *Generic function* <br>
`dispose sequence &rest fns => sequence1 sequence2 ...`<br>

Returns a number of sequences equal to the number of `fns`. `dispose`
maps each function in `fns` over the sequence to produce a new
sequence, returning all of the new sequences produced as multiple
values.


**`drop`** *Generic function* <br>
`drop n sequence => sequence'`<br>

Returns the elements of `sequence` after the first `n` elements are
removed. In the special case of values of type `CONS`, `drop` is a
synonym for Common Lisp's `NTHCDR`.


**`drop-while`** *Generic function* <br>
`drop-while predicate sequence => sequence'`<br>

`drop-while` applies `predicate` to each element of `sequence` until
it returns a false value. The elements of `sequence` beginning with
the first one that returns a false value from `predicate` are
collected in a new sequence and returned.


**`element`** *Generic function* <br>
`element sequence n => Anything`<br>

Returns the `nth` element of `sequence`, counting from zero.

**`empty?`** *Generic function* <br>
`empty? sequence => Boolean`<br>

Returns a true value if `sequence` contains no elements, and a false
value otherwise.

**`every?`** *Generic function* <br>
`every? predicate sequence => Anything`<br>

Returns a true value if applying *predicate* to every element of
*sequence* returns a true value. Returns `T` if *sequence* is empty.


**`filter`** *Generic function* <br>
`filter predicate sequence => sequence'`<br>

Returns a sequence of values that are elements of *sequence*. Applies
*predicate* to each element of *sequence*, returning a sequence of
those elements for which *predicate* returned a true value.


**`find`** *Generic function* <br>
`find item sequence &key (test 'eql) (start 0) end (key 'cl:identity) => Anything`<br>

Returns the first element of *sequence* that is equivalent to *item*
in a sense defined by *test*. For example, if *test* is `equalp` then
the first element equal to *item* is returned. If *key* is supplied
then the value returned by applying *key* to each element is used for
the comparison. If *start* is supplied then the search begins with the
element at index *start*; otherwise it begins at element zero. If
*end* is supplied then the last element tested will be the one at
index one less than *end*.

**`find-if`** *Generic function* <br>
`find-if predicate sequence &key (start 0) end (key 'cl:identity) => Anything`<br>

Returns the first element of *sequence* for which *predicate* returns
a true value when applied to the element. If *key* is supplied
then the value returned by applying *key* to each element is used for
the comparison. If *start* is supplied then the search begins with the
element at index *start*; otherwise it begins at element zero. If
*end* is supplied then the last element tested will be the one at
index one less than *end*.


**`find-if-not`** *Generic function* <br>
`find-if-not predicate sequence &key (start 0) end (key 'cl:identity) => Anything`<br>

Returns the first element of *sequence* for which *predicate* returns
a false value when applied to the element. If *key* is supplied then
the value returned by applying *key* to each element is used for the
comparison. If *start* is supplied then the search begins with the
element at index *start*; otherwise it begins at element zero. If
*end* is supplied then the last element tested will be the one at
index one less than *end*.

**`first`** *Generic function* <br>
`first sequence => Anything`<br>

Returns the first element of *sequence*. A synonym for `first` is `head`.

**`head`** *Generic function* <br>
`head sequence => Anything`<br>

Returns the first element of *sequence*. A synonym for `head` is `first`.

**`image`** *Generic function* <br>
`image function sequence => sequence'`<br>

Returns a sequence of values computed by applying *function* to each
element of *sequence*. `image` is a generic, extensible synonym for
Common Lisp's `MAPCAR` which observes folio's style and
conventions. It's also a synonym for FSet's function of the same name.


**`indexes`** *Generic function* <br>
`indexes sequence => sequence'`<br>

Returns a sequence of integers corresponding to the indexes of the
elements of `sequence`. `indexes` is useful for composing functions
that map over indexed sequences such as lists, vectors, and series.

**`interleave`** *Generic function* <br>
`interleave sequence1 sequence2 => sequence3`<br>

Returns a sequence of elements from *sequence1* and *sequence2* in
alternation.


**`interpose`** *Generic function* <br>
`interpose item sequence => sequence'`<br>

Returns a sequence of elements from *sequence*, separated by *item*.


**`join`** *Generic function* <br>
`join cupola sequences => sequence'`<br>

Returns a sequence constructed by appending *sequences* with *cupola*
interposed between them.


**`last`** *Generic function* <br>
`last sequence => Anything`<br>

Returns the last element of *sequence*. folio's `last` differs from
Common Lisp's `last` in that it's generic, operating on all sequence
types, and in that it returns the last element of the sequence rather
than the last `cons` of a list.


**`leave`** *Generic function* <br>
`leave n sequence => sequence'`<br>

Returns a sequence of the last *n* elements of *sequence*.


**`length`** *Generic function* <br>
`length sequence => integer`<br>

Returns the count of elements in *sequence*. folio's `length` differs from
Common Lisp's `length` in that it's generic, operating on all sequence
types.


**`mismatch`** *Generic function* <br>
`mismatch sequence1 sequence2 &key (test 'cl:eql) key (start1 0) (start2 0) (end1 nil) (end2 nil) => integer`<br>

Returns the index at which *sequence2* is different from *sequence1*,
or `nil` if they are equivalent. The function *test* is used to
distinguish elements. The comparison starts at indexes *start1* and
*start2*, and ends with indexes one less than *end1* and *end2*, or at
the end of the shorter sequence if the *end* values are not supplied.


**`partition`** *Generic function* <br>
`partition predicate sequence1 => sequence2 sequence3`<br>

Returns two sequences: *sequence2* contains those elements of
*sequence1* for which *predicate* returns a true value when applied to
the element; *sequence3* contains those elements of *sequence1* for
which *predicate* returns a false value when applied to the element.


**`penult`** *Generic function* <br>
`penult sequence => Anything`<br>

Returns the next-to-last element of *sequence*.


**`position`** *Generic function* <br>
`position item sequence (test 'cl:eql) (start 0) end (key 'cl:identity) => Anything`<br>

Returns the index at which *item* can be found in *sequence*, or `nil`
if it doesn't appear. *test* is used to compare *item* to the result
of applying *key* to the elements of *sequence*. The search begins at
index *start* and ends with index one less than *end*, or at the end of
*sequence* if *end* is not supplied. A generic synonym for Common
Lisp's `position`.


**`position-if`** *Generic function* <br>
`position-if predicate sequence (start 0) end (key 'cl:identity) => Anything`<br>

Returns the index of an element that satisfies the test given by
*predicate*, or `nil` if no element satisfies the test. `position-if`
applies the function *key* to each element of *sequence*, then applies
*predicate* to the value returned by *key*. The first index whose
element yields a true value from this procedure is the index returned
by `position-if`. A generic synonym for Common Lisp's `position-if`.


**`position-if-not`** *Generic function* <br>
`position-if-not predicate sequence (start 0) end (key 'cl:identity) => Anything`<br>

Returns the index of an element that failes to satisfy the test given
by *predicate*, or `nil` if no element satisfies the
test. `position-if-not` applies the function *key* to each element of
*sequence*, then applies *predicate* to the value returned by
*key*. The first index whose element yields a false value from this
procedure is the index returned by `position-if-not`. A generic
synonym for Common Lisp's `position-if-not`.


**`prefix-match?`** *Generic function* <br>
`prefix-match? sequence value &key (test 'cl:equal)(key 'cl:identity) => generalized Boolean`<br>

Returns a true value if sequence is a prefix of value. *test* is used
to compare *sequence* to a sequence computed from *value* by applying
*key*.


**`range`** *Generic function* <br>
`range start end &key (by 1) => sequence`<br>

Returns a sequence of integers beginning with *start* and ending if
one greater or less than *end*, depending on whether the result is
monotonically increasing or decreasing.

For example:

    FOLIO> (range 1 10)
    (1 2 3 4 5 6 7 8 9)

    FOLIO> (range 10 1)
    (10 9 8 7 6 5 4 3 2)

The parameter *by* specifies the interval between elements:

    FOLIO> (range 0 10 :by 2)
    (0 2 4 6 8)



**`reduce`** *Generic function* <br>
`reduce fn sequence  &key (key 'cl:identity) (initial-value nil) => Anything`<br>

Returns a value computed by applying *fn* to successive pairs of
values found in *sequence*. Before applying *fn*, `reduce` applies
*key* to each of the two values to obtain the value that is to be
combined. *initial-value* provides a starting value that is combined
with the first element of *sequence*. A common idiom is to combine the
results of a reduction into a list, and so the default value of
*initial-value* is the empty list--`nil`. `reduce` is a generic
synonym for Common Lisp's `reduce`.


**`remove`** *Generic function* <br>
`remove item sequence &key (test 'eql) (start 0) end (key 'cl:identity) => sequence'`<br>

Returns a sequence equivalent to *sequence*, but with *item*
removed. *test* is used to determine which elements of *sequence* are
equivalent to *item*. *key* is applied to each element to obtain the
value that is compared to *item*. The search starts with index
*start*, and ends at the index before *end*, or with the last element
of *sequence* if end is `nil`. A generic synonym for Common Lisp's
`remove`.


**`remove-if`** *Generic function* <br>
`remove-if predicate sequence &key (test 'eql) (start 0) end (key 'cl:identity) => sequence'`<br>

Returns a sequence equivalent to *sequence*, but with elements that
satisfy *predicate* removed. *key* is applied to each element to
obtain the value that is tested with *predicate*. The search starts
with index *start*, and ends at the index before *end*, or with the
last element of *sequence* if end is `nil`. A generic synonym for
Common Lisp's `remove-if`.


**`remove-if-not`** *Generic function* <br>
`remove-if-not predicate sequence &key (test 'eql) (start 0) end (key 'cl:identity) => sequence'`<br>

Returns a sequence equivalent to *sequence*, but with elements that
fail to satisfy *predicate* removed. *key* is applied to each element
to obtain the value that is tested with *predicate*. The search starts
with index *start*, and ends at the index before *end*, or with the
last element of *sequence* if end is `nil`. A generic synonym for
Common Lisp's `remove-if-not`.


**`remove-duplicates`** *Generic function*  => sequence'`<br>
`remove-duplicates sequence &key

Returns a sequence equivalent to *sequence*, but with duplicate of
elements removed. *test* is used to determine whether two elements are
to be considered duplicates. *key* is applied to each element to
obtain the value that is tested with *test*. The search starts with
index *start*, and ends at the index before *end*, or with the last
element of *sequence* if end is `nil`. A generic synonym for Common
Lisp's `remove-duplicates`.


**`rest`** *Generic function* <br>
`rest sequence => sequence'`<br>

Returns a sequence containing all but the first element of
*seqence*. A generic synonym for Common Lisp's `rest`.

**`reverse`** *Generic function* <br>
`reverse sequence => sequence'`<br>

Returns a sequence containing all elements of *sequence* in reverse
order. A generic synonym for Common Lisp's `reverse`.


**`search`** *Generic function* <br>
`search sequence1 sequence2 &key (start1 0) end1 (start2 0) end2 (test 'cl:equal) (key 'cl:identity) => integer or nil'`<br>

Returns the index in *sequence2* where *sequence1* appears as a
subsequence, or `nil` if *sequence1* isn't found. *test* is used to
compare the sequences. *key* computes a value for each sequence for
comparison. The search starts at indexes *start1* and *start2*, and
ends with index one less than *end1* and *end2*, or with the end of
the shorter sequence if *end* values are not given. A generic synonym
for Common Lisp's `search`.


**`second`** *Generic function* <br>
`second sequence => Anything`<br>

Returns the second element of *sequence*. A generic synonym for Common
Lisp's `second`.


**`select`** *Generic function* <br>
`select sequence indexes => sequence'`<br>

Returns the elements of *sequence* at the indexes given in *indexes*.

**`sequence`** *Generic function* <br>
`sequence &rest elements => sequence`

Returns a sequence of *elements*. folio chooses the sequence type. You
can obtain a specific type of sequence using `as`.

**`sequence?`** *Generic function* <br>
`sequence? thing => Boolean`

Returns true if *thing* is an instance of a sequence type.

**`shuffle`** *Generic function* <br>
`shuffle sequence => sequence'`

Returns a new sequence containing the elements of *sequence* in some
arbitrary, rearranged order.

**`some?`** *Generic function* <br>
`some? predicate sequence &rest sequences => Anything`

Returns the first element of *sequence* that satifies the test given
by *predicate*. Predicate may be a function of more than one argument
if the count of *sequences* plus one equals the number of arguments
required by *predicate*.

**`sort`** *Generic function* <br>
`sort sequence predicate &key (key 'cl:identity) => sequence'`

Returns a new sequence of the elements of *sequence* sorted according
to the two-argument predicate given by *predicate*. The function *key*
is used to compute the values that are compared by *predicate*.

**`split`** *Generic function* <br>
`split sequence sentinel &key (key 'cl:identity) (test 'cl:eql) => sequence'`

Returns a sequence of subsequences of *sequence*. `split` separates
*sequence* into subsequences on boundaries defined by the value
*sentinel*; each place where *sentinel* appears becomes the end of a
subsequence. *key* is applied to each element of *sequence* before
*test* is used to determine whether it is equivalent to *sentinel*.

**`subsequence`** *Generic function* <br>
`subsequence sequence start &optional (end nil) => sequence'`

Returns a new sequence that is a subsquence of *sequence*. The
subsequence starts with the elements from index *start* od *sequence*,
and collects elements at indexes up to one less than *end*, or the end
of *sequence* if *end* is not given.

**`substitute`** *Generic function* <br>
`substitute new-item old-item sequence &key (key 'cl:identity) (test 'cl:eql) => sequence'`

Returns a new sequence equivalent to *sequence* except that
occurrences of *old-item* are replaced by *new-item*. *test* is used
to determine whether elements are equivalent to *old-item*. *key* is
applied to each element to obtain the value that is tested.

**`substitute-if`** *Generic function* <br>
`substitute-if new-item predicate sequence &key key => sequence'`

Returns a new sequence equivalent to *sequence* except that
occurrences of elements that satisfy *predicate* are replaced by
*new-item*. *key* is applied to each element to obtain the value that
is tested.

**`substitute-if-not`** *Generic function* <br>
`substitute-if-not new-item predicate sequence &key key => sequence'`

Returns a new sequence equivalent to *sequence* except that
occurrences of elements that fail to satisfy *predicate* are replaced
by *new-item*. *key* is applied to each element to obtain the value
that is tested.

**`suffix-match?`** *Generic function* <br>
`suffix-match? sequence suffix &key (test 'cl:equal) (key 'cl:identity) => Generalized Boolean`

Returns true if *suffix* is a suffix of *sequence*. *test* is used to
compare sequences. *key* is applied to each sequence to obtain the
values that are compared.

**`tail`** *Generic function* <br>
`tail sequence => anything`

Returns a new sequence equivalent to *sequence* except that the first
element is removed.

**`tails`** *Generic function* <br>
`tails sequence &key by => list`

Returns a sequence of sequences: the first element of the result is
*sequence*; the second is `(tail sequence)`; the third is `(tail (tail
sequence))`; and so on.

**`take`** *Generic function* <br>
`take n sequence => sequence'`

Returns a new sequence containing the first *n* elements of *sequence*.

**`take-by`** *Generic function* <br>
`take-by m n sequence => sequences`

Returns a new sequence of sequences. Each new sequence is constructed
by taking *m* elements from *sequence*. The first sequence collects
the first *m* elements of *sequence*. The second collects the first
*m* elements starting from index *n*. The third collects the first *m*
elements starting from (* 2 n); and so on.

**`take-while`** *Generic function* <br>
`take-while predicate sequence => sequence'`

Returns a new sequence of elements from *sequence*. `take-while`
collects elements of *sequence* from left to right until it finds an
element of *sequence* that fails to satisfy *predicate*. It then
returns the found elements that satisfied *predicate* thus far.

**`unzip`** *Generic function* <br>
`unzip sequence1 => sequence2, sequence3`

*sequence1* must be a sequence of **pairs**. `unzip` returns two
 values: the left elements of the pairs, and the right elements of the
 pairs.

**`wb-seq?`** *Generic function* <br>
`wb-seq? thing => Boolean`

Returns true if *thing* is an instance of `FSET:WB-SEQ`.

**`zip`** *Generic function* <br>
`zip sequence1 sequence2 => sequence3`

Returns a sequence of pairs in which the left elements of the pairs
are the elements of *sequence1*, and the right elements of the pairs
are the elements of *sequence2*.

## series

Objects that represent ordered sequences of values where the length of
each sequence may be unbounded. An extension of sequences to represent
collections with possibly infinite numbers of members. An extension to
sequences that enables us to conveniently treat iterative procedures
as the mapping of functions over (possibly infinite) sequences.

The series package provides generalized versions of many sequence
functions that work with series, as well as some series-specific
functions.

#### Reference

**`add-first`** *Generic function* <br>
`add-first val series => series'`<br>

Returns a new series whose first element is `val` and whose
remaining elements are the elements of `series`. 


**`by`** *Generic function* <br>
`by n series => series'`<br>

Returns a new series made up of subseries of `series`, each of
length `n`. `by` splits the input series into subseries of length
`n`; for example,

    (by 3 "abcdefghi") => #("abc" "def" "ghi")

**`coalesce`** *Generic function* <br>
`coalesce fn &rest sers => ser'`

Returns a new series by combining one or more input series. The number
of *sers* must be equal to the number of required arguments to
*fn*. `coalesce` constructs the first element of its result series by
applying *fn* to the first elements of *sers*. It constructs the
second element by applying *fn* to the second elements of *sers*. It
continues in this fashion as long as there are elements of all
*sers*. THe result may be of infinite length if all the input *sers*
are infinitely long.

For example:

`FOLIO> (take 3 (coalesce #'* (range-from 0)(range-from 1)))`

`#Z(0 2 6)`


**`dispose`** *Generic function* <br>
`dispose series &rest fns => series1 series2 ...`<br>

Returns a number of series equal to the number of `fns`. `dispose`
maps each function in `fns` over the series to produce a new
series, returning all of the new series produced as multiple
values.


**`drop`** *Generic function* <br>
`drop n series => series'`<br>

Returns the elements of `series` after the first `n` elements are
removed. In the special case of values of type `CONS`, `drop` is a
synonym for Common Lisp's `NTHCDR`.


**`drop-while`** *Generic function* <br>
`drop-while predicate series => series'`<br>

`drop-while` applies `predicate` to each element of `series` until
it returns a false value. The elements of `series` beginning with
the first one that returns a false value from `predicate` are
collected in a new series and returned.


**`element`** *Generic function* <br>
`element series n => Anything`<br>

Returns the `nth` element of `series`, counting from zero.


**`filter`** *Generic function* <br>
`filter predicate series => series'`<br>

Returns a series of values that are elements of *series*. Applies
*predicate* to each element of *series*, returning a series of
those elements for which *predicate* returned a true value.


**`first`** *Generic function* <br>
`first series => Anything`<br>

Returns the first element of *series*. A synonym for `first` is `head`.


**`head`** *Generic function* <br>
`head series => Anything`<br>

Returns the first element of *series*. A synonym for `head` is `first`.


**`image`** *Generic function* <br>
`image function series => series'`<br>

Returns a series of values computed by applying *function* to each
element of *series*. 


**`indexes`** *Generic function* <br>
`indexes series => series'`<br>

Returns a series of integers corresponding to the indexes of the
elements of `series`. `indexes` is useful for composing functions
that map over indexed series such as lists, vectors, and series.


**`interleave`** *Generic function* <br>
`interleave series1 series2 => series3`<br>

Returns a series of elements from *series1* and *series2* in
alternation.


**`interpose`** *Generic function* <br>
`interpose item series => series'`<br>

Returns a series of elements from *series*, separated by *item*.

<br>
**iterate** *fn* *arg*  => *series*  &nbsp;&nbsp;&nbsp;&nbsp;[*Macro*]<br>

Returns a series whose elements are successive results from the
application of *fn*. The series' elements are:

    (*fn* *arg*)
    (*fn* (*fn* *arg*))
    (*fn* (*fn* (*fn* *arg*)))
    ...
    
...and so on.


**`partition`** *Generic function* <br>
`partition predicate series1 => series2 series3`<br>

Returns two series: *series2* contains those elements of
*series1* for which *predicate* returns a true value when applied to
the element; *series3* contains those elements of *series1* for
which *predicate* returns a false value when applied to the element.


**`prefix-match?`** *Generic function* <br>
`prefix-match? series value &key (test 'cl:equal)(key 'cl:identity) => generalized Boolean`<br>

Returns a true value if series is a prefix of value. *test* is used
to compare *series* to a series computed from *value* by applying
*key*.

**`range-from`** *Generic function* <br>
`range-from (start integer) &key (by 1) => series`<br>

Returns an infinite series of integers, beginning with *start*, and
continuing by adding *by* to each successive element.

**`remove`** *Generic function* <br>
`remove item series &key (test 'eql) (start 0) end (key 'cl:identity) => series'`<br>

Returns a series equivalent to *series*, but with *item*
removed. *test* is used to determine which elements of *series* are
equivalent to *item*. *key* is applied to each element to obtain the
value that is compared to *item*. The search starts with index
*start*, and ends at the index before *end*, or with the last element
of *series* if end is `nil`. A generic synonym for Common Lisp's
`remove`.


**`remove-if`** *Generic function* <br>
`remove-if predicate series &key (test 'eql) (start 0) end (key 'cl:identity) => series'`<br>

Returns a series equivalent to *series*, but with elements that
satisfy *predicate* removed. *key* is applied to each element to
obtain the value that is tested with *predicate*. The search starts
with index *start*, and ends at the index before *end*, or with the
last element of *series* if end is `nil`. A generic synonym for
Common Lisp's `remove-if`.


**`remove-if-not`** *Generic function* <br>
`remove-if-not predicate series &key (test 'eql) (start 0) end (key 'cl:identity) => series'`<br>

Returns a series equivalent to *series*, but with elements that
fail to satisfy *predicate* removed. *key* is applied to each element
to obtain the value that is tested with *predicate*. The search starts
with index *start*, and ends at the index before *end*, or with the
last element of *series* if end is `nil`. A generic synonym for
Common Lisp's `remove-if-not`.

**`repeat`** *Generic function* <br>
`repeat expr => series`

Returns an infinitely-long series of the values returned by repeatedly evaluating
*expr*.

For example:

    FOLIO> (defparameter $count 0)
    $COUNT

    FOLIO> (take 5 (repeat (incf $count 3)))
    #Z(3 6 9 12 15)


**`rest`** *Generic function* <br>
`rest series => series'`<br>

Returns a series containing all but the first element of
*seqence*. A generic synonym for Common Lisp's `rest`.

**`scan`** *Generic function* <br>
`scan sequence => series`

Returns a new finite series containing the elements of *sequence*.

**`second`** *Generic function* <br>
`second series => Anything`<br>

Returns the second element of *series*. A generic synonym for Common
Lisp's `second`.


**`select`** *Generic function* <br>
`select series indexes => series'`<br>

Returns the elements of *series* at the indexes given in
*indexes*. Both *series* and *indexes* may be infinitely long. If they
are, then *series'* is also infinitely long.


**`series`** *Generic function* <br>
`series &rest elements => series`

Returns a new series containing *elements*.

**`series?`** *Generic function* <br>
`series? thing => Boolean`

Returns true if *thing* is an instance of a series type.

**`subsequence`** *Generic function* <br>
`subsequence sequence start &optional end => sequence'`

Returns a new sequence or series containing the elements of *sequence*
beginning with index *start*. If *end* is given, the result contains
those elements up to the index one before *end*. If not, then the
result series contains all elements after *start*, and may be
infinitely long.

**`substitute`** *Generic function* <br>
`substitute new-item old-item series &key key => series'`

Returns a new series equivalent to *series* except that
occurrences of *old-item* are replaced by *new-item*. *test* is used
to determine whether elements are equivalent to *old-item*. *key* is
applied to each element to obtain the value that is tested.

**`substitute-if`** *Generic function* <br>
`substitute-if new-item predicate series &key key => series'`

Returns a new series equivalent to *series* except that
occurrences of elements that satisfy *predicate* are replaced by
*new-item*. *key* is applied to each element to obtain the value that
is tested.


**`substitute-if-not`** *Generic function* <br>
`substitute-if-not new-item predicate series &key key => series'`

Returns a new series equivalent to *series* except that
occurrences of elements that fail to satisfy *predicate* are replaced
by *new-item*. *key* is applied to each element to obtain the value
that is tested.


**`tail`** *Generic function* <br>
`tail series => anything`

Returns a new series equivalent to *series* except that the first
element is removed.


**`tails`** *Generic function* <br>
`tails series &key by => list`

Returns a series of series: the first element of the result is
*series*; the second is `(tail series)`; the third is `(tail (tail
series))`; and so on.


**`take`** *Generic function* <br>
`take n series => series'`

Returns a new series containing the first *n* elements of *series*.

**`take-by`** *Generic function* <br>
`take-by m n series => series`


Returns a new series of series. Each new series is constructed
by taking *m* elements from *series*. The first series collects
the first *m* elements of *series*. The second collects the first
*m* elements starting from index *n*. The third collects the first *m*
elements starting from (* 2 n); and so on.


**`take-while`** *Generic function* <br>
`take-while predicate series => series'`

Returns a new series of elements from *series*. `take-while`
collects elements of *series* from left to right until it finds an
element of *series* that fails to satisfy *predicate*. It then
returns the found elements that satisfied *predicate* thus far.

**`unzip`** *Generic function* <br>
`unzip series1 => series2, series3`

*series1* must be a series of **pairs**. `unzip` returns two
 values: the left elements of the pairs, and the right elements of the
 pairs.


**`zip`** *Generic function* <br>
`zip series1 series2 => series3`

Returns a series of pairs in which the left elements of the pairs
are the elements of *series1*, and the right elements of the pairs
are the elements of *series2*.

## taps

Functions that generate series of values from streams, sequences, and
other data structures. Taps can be used to present data in a form that
is convenient for sequence and series operations.

#### Reference

**`characters`** *Generic function* <br>
`characters string-or-stream => a series of characters`

Returns a series of characters read from *string-or-stream*.

**`elements`** *Generic function* <br>
`elements sequence => a series of values found as elements of a sequence`

Returns a series of the elements of *sequence*.

**lines`** *Generic function* <br>
`lines string-or-stream => a series of lines (strings)`

Returns a series of strings read one line at a time from *string-or-stream*.

**`octets`** *Generic function* <br>
`octets sequence-or-stream => a series of octets`

Returns a series of octets read from *string-or-stream*.

**`slots`** *Generic function* <br>
`slots map-or-instance => a series of pairs`

Returns a series pairs in which the left element of each pair is a
slot name of *map-or-instance*, and the right element is the
corresponding value.

**`objects`** *Generic function* <br>
`objects string-or-stream => a series of Lisp objects produced by READ`

Returns a series of objects read by the Lisp reader from
*string-or-stream*.

## Bugs

Higher-order folio functions--those that accept functions as
arguments--generally do not work correctly when passed an fbound
symbol. You must use an actual function object instead--that is, an
expression like `(function foo)` or `#'foo`.