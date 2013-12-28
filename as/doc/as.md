# as
A generic, extensible conversion utility

**`as`** *Generic function* 

`as` *`type`* *`value`* `&key &allow-other-keys`  => *`Anything`*<br>

Returns a new value of type *`type`* that is in some sense equivalent
to *`value`*. 

`as` is the only symbol exported from the package
`net.bardcode.folio.as`; it is intended to be imported in other
packages and used as a convenient and extensible utility for type
conversion.

`as` is a generic function; add methods to it as-needed in order to
facilitate type conversions.