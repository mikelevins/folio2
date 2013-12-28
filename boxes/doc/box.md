# box
A mutable container for values.

**Package:** `net.bardcode.folio.boxes`<br>
**Exports:** `box box? set-box! unbox`

The `box` type is a mutable container for arbitrary values. The functions `unbox` and `set-box!` can be used to retrieve and replace the value stored in a `box`.

Boxes provide a way to introduce mutability piecemeal into immutable and pure-functional data structures. As an example, we can use a pure-functional implementation of finite maps, but store the value elements in boxes so that they can be destructively updated. All operations on the finite map as a whole and on its keys remain purely functional, but we can destructively modify the values stored on the keys.

A type definition establishes the `box` type as a synonym for a `cons` cell whose `car` element is the keyword `:BOX`.

## Reference

**`as`** *Generic function* <br>
`as 'box value => box`<br>

**`box`** *Type* <br>
A `box` is a mutable container for a value. folio represents a `box` as a
`cons` whose `car` is the keyword `:box`.

**`box`** *Function* <br>
`box value => box`<br>
Returns a new box containing `value`.

**`box?`** *Generic function* <br>
`box? value => Generalized Boolean`<br>
Returns a true value if *`value`* is a `box`, and a false value otherwise.

**`make`** *Generic function* <br>
`make 'box &key (value nil) => Anything`<br>
Returns a new `box` whose initial value is `value`.

**`set-box!`** *Generic function* <br>
`set-box! box value => Anything`<br>
Replaces the value of `box` with `value`, returning `value`.

**`unbox`** *Generic function* <br>
`unbox box => Anything`<br>
Returns the value stored in `box`.

**`setf unbox`** *Generic function* <br>
`setf (unbox b) val => val`<br>
Replaces the value of `b` with `value`, returning `value`.





