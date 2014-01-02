# make

A generic, extensible utility for constructing values.

**Package:** net.bardcode.folio.make<br>
**Exports:** make

The **make** package exports the extensible generic function **make**. By specializing this generic function you can implement custom constructors for arbitrary types.

**make** is defined to accept keword arguments, so that your specializations can use keyword parameters to exercise fine control over constructor behavior.

The exportation of a single name is intentional. It's meant to make it easy and convenient to `USE` or `IMPORT` the **make** function and extend it as-needed.

## Reference

**make** *type* &key &allow-other-keys  => Anything  &nbsp;&nbsp;&nbsp;&nbsp;[*Generic function*]<br>
Returns a new instance of *type*, constructed according to the specifications passed in the keyword parameters.

