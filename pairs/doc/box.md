# box
A mutable container

A **box** is a mutable container that can be used to bridge the
advantages of mutable data with those of immutable data and pure
functions.

Pure-functional data structures and immutable data offers certain
strong advantages, but can also sometimes impose awkward
restrictions. By selectively including boxes as elements in immutable
structures, you can introduce just as much mutability as you need, and
no more.

**`as`** *Generic function* 

`as` `'box` *`value`* => `box`<br>

**`box`** *Type* 

A `box` is a container for a value. folio represents a `box` as a
`cons` whose `car` is the keyword `:box`. A type definition
establishes such values as members of the type `box`.

**`box`** *Function* 

`box` *`value`* => `box`<br>

**`box?`** *Generic function* 

`box?` *`value`*  => *`Generalized boolean`*<br>
Returns a true value if *`value`* is a `box`, and a false value otherwise.

**`make`** *Generic function* 

`make` `'box` `&key` *`value`* => `box`<br>

**`set-box!`** *Generic function* 

`set-box!` *`box`* *`value`* => `Anything`<br>

**`unbox`** *Generic function* 

`unbox` *`box`*  => `Anything`<br>
Returns the value stored in *`box`*.

`setf (unbox ` *`b`* `)` *`value`*  => `Anything`<br>





