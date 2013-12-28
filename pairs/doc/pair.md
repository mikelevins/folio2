# pair
A simple data structure with left and right slots. An extensible synonym for `cons`.

**Package:** `net.bardcode.folio.pair`<br>
**Exports:** `left pair pair? right set-left! set-right!`

The `pair` type is an extensible synonym for Lisp's `cons` type. Any type for which the `pair` functions are specialized is a pair, which means that you can implement your own representations of pairs. 

A second use for the `pair` functions is to clearly communicate intent: using the `pair` functions with `cons` cells communicates that the programmer intends to treat them as pairs of values, as opposed to any of the many other uses to which `cons` cells can be put.

## Reference

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
