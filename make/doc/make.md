# make

A generic, extensible value-construction utility.

**Package:** `net.bardcode.folio.make`<br>
**Exports:** `as`

`make` exports a single symbol named `make`, which names a generic function. The function `make` is an extensible value-construction utility. It can be specialized as-needed to provide a constructor for arbitrary datatypes.

The `make` package is intended to work well with `use`; it exports only the one symbol, minimizing the chance of name conflicts. The generic function `make` is intended to be specialized in order to provide convenient value construction as-needed.

## Reference

**`make`** *Generic function* <br>
`make type &key &allow-other-keys  => Anything`<br>
Returns a new value of type `type`. Keyword parameters, if any, are used to initialize the new value.
