# make
A generic, extensible construction utility

**`make`** *Generic function* 

`make` *`type`* `&key &allow-other-keys`  => *`Anything`*<br>

Returns a new value of type *`type`*. Attributes of the value, if any,
reflect the initialization values provided by keyword arguments.

`make` is the only symbol exported from the package
`net.bardcode.folio.make`; it is intended to be imported in other
packages and used as a convenient and extensible utility for
constructing values of various types.

`make` is a generic function; add methods to it as-needed in order to
facilitate construction.