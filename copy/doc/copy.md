# copy

A generic, extensible copying utility.

**Package:** `net.bardcode.folio.copy`<br>
**Exports:** `copy`, `deep-copy`, `shallow-copy`

`copy` exports three symbols named `copy`, `deep-copy`, and
`shallow-copy`. These symbols name generic functions that are intended
to be imported and specialized as-needed to provide value-copying
semantics.

Each of the provided generic functions has a lambda list with &key and
&allow-other-keys keywords, so that specializations may introduce
additional keywords to control copying strategies. The provided
implementations of the generic functions do not use these keyword
parameters.

The `copy` package is intended to work well with `use`; it exports
only the three symbols, minimizing the chance of name conflicts.

## Reference

**`copy`** *Generic function* 

`copy value &key &allow-other-keys => Anything`<br> 
Returns a new copy of `value` using a default strategy for the type of
`value`. The normal default strategy is shallow copy.

**`deep copy`** *Generic function* 

`deep-copy value &key &allow-other-keys => Anything`<br> 
Returns a new copy of `value` using a strategy that recursively copies
each and every component element of `value`. As an example, calling
`deep-copy` on a list makes copies not only of the cons cells that
form the list, but also of the elements of the list, and of any
component parts of those values.

**`shallow-copy`** *Generic function* 

`shallow-copy value &key &allow-other-keys => Anything`<br> 
Returns a new copy of `value` using a strategy that copies only the
topmost element or elements of the value. As an example, applying
`shallow-copy` to a list makes copies of the cons cells that for the
spine of the list, but does not copy the elements of the list; the
elements in the new list are EQ to the values that were in the
original list.



