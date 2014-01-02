# copy

A generic, extensible utility for comparing values.

**Package:** net.bardcode.folio.copy<br>
**Exports:** copy deep-copy shallow-copy

The **copy** package exports the extensible generic functions **copy**, **deep-copy**, and **shallow-copy**. By specializing these generic functions for your types you can provide copying semantics for them.

When specializing the copying operators, it's usually best to avoid changing the definition of **copy**, unless you know that you need behavior that's different from the default. The default behavior of **copy** is to call **shallow-copy**. Only if you're sure that's the wrong behavior should you specialize **copy**.

All three generic functions are defined to accept keword arguments, so that your specializations can use keyword parameters to exercise fine control over copying behavior.

## Reference

**copy** *value* &key &allow-other-keys  => Anything  &nbsp;&nbsp;&nbsp;&nbsp;[*Generic function*]<br>
Returns a new copy of *value*. The default implementation of **copy** calls **shallow-copy**.

**deep-copy=** *value* &key &allow-other-keys  => Anything  &nbsp;&nbsp;&nbsp;&nbsp;[*Generic function*]<br>
Returns a new copy of *value* constructed by instantiating a value of the same type and initializing its slots or contents with new copies of the slots or contents in *value*. The slot contents or elements used in the new copy are themselves also new copies; they are not the values from the original *value*.

**shallow-copy** *value* &key &allow-other-keys  => Anything  &nbsp;&nbsp;&nbsp;&nbsp;[*Generic function*]<br>
Returns a new copy of *value* constructed by instantiating a value of the same type and initializing its slots or contents with the slots or contents in *value*. Only the top-level *value* is copied. The slot contents or elements are the same values that were found in the original *value*, not copies. In other words, if you **shallow-copy** a list of strings, the returned list is a new value, but the strings are exactly the same ones that were in the riginal list; they are not copies of those strings. To copy a value by making new copies of its contents, use **deep-copy**.