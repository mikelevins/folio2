# as

A generic, extensible conversion utility.

**Package:** `net.bardcode.folio.as`<br>
**Exports:** `as`

`as` exports a single symbol named `as`, which names a generic function. The function `as` is an extensible type-conversion utility. It can be specialized as-needed to provide type conversion among datatypes.

The `as` package is intended to work well with `use`; it exports only the one symbol, minimizing the chance of name conflicts. The generic function `as` is intended to be specialized in order to provide convenient type conversion as-needed.

## Reference

**`as`** *Generic function* 

`as type value &key &allow-other-keys  => Anything`<br>
Returns a new value of type `type` that is in some sense equivalent
to `value`. 
