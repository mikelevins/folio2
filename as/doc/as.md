# as

A generic, extensible conversion utility.

**Package:** net.bardcode.folio.as<br>
**Exports:** as

**as** exports a single symbol named **as**, which names a generic function. The function **as** is an extensible type-conversion utility. It can be specialized as-needed to provide  conversions among arbitrary types.

The exportation of a single name is intentional. It's meant to make it easy and convenient to `USE` or `IMPORT` the **as** function and extend it as-needed.

## Reference

**as** *type* *value* &key &allow-other-keys  => Anything  &nbsp;&nbsp;&nbsp;&nbsp;[*Generic function*]<br>
Returns a new value of type *type* that is in some sense equivalent
to *value*. 
