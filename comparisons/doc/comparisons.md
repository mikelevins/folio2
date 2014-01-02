# comparisons

A generic, extensible utility for comparing values.

**Package:** net.bardcode.folio.comparisons<br>
**Exports:** > >= < <= binary-> binary->= binary-< binary-<= binary-equivalent? equivalent?

The **comparisons** package exports extensible versions of Common Lisp's comparison functions, > >= < and <=. In addition, it exports equivalent?, a customizable equivalence test, and a set of binary operators named binary-> binary->= binary-< binary-<=, and binary-equivalent? that make it easy to customize the behavior of the comparison functions.

When extending the comparison functions in **comarisons**, you should avoid specializing > >= < <= and equivalent?. Instead, specialize the binary-* operators. The operators > >= < <= and equivalent? use the binary-operators to perform their comparisons; specializing the binary operators automatically provides your specialized behavior in the other operators.

## Reference

**>** *value* * &key &allow-other-keys  => Generalized Boolean  &nbsp;&nbsp;&nbsp;&nbsp;[*Generic function*]<br>
Returns true if all values are monotonically decreasing, and false otherwise.

**>=** *value* * &key &allow-other-keys  => Generalized Boolean  &nbsp;&nbsp;&nbsp;&nbsp;[*Generic function*]<br>
Returns true if all values are monotonically decreasing or equal, and false otherwise.

**<** *value* * &key &allow-other-keys  => Generalized Boolean  &nbsp;&nbsp;&nbsp;&nbsp;[*Generic function*]<br>
Returns true if all values are monotonically increasing, and false otherwise.

**<=** *value* * &key &allow-other-keys  => Generalized Boolean  &nbsp;&nbsp;&nbsp;&nbsp;[*Generic function*]<br>
Returns true if all values are monotonically increasing or equal, and false otherwise.

**equivalent?** *value* * &key &allow-other-keys  => Generalized Boolean  &nbsp;&nbsp;&nbsp;&nbsp;[*Generic function*]<br>
Returns true if all values are equivalent, and false otherwise.


**binary->** *value1* *value2* * &key &allow-other-keys  => Generalized Boolean  &nbsp;&nbsp;&nbsp;&nbsp;[*Generic function*]<br>
Returns true if *value1* is greater than *value2*, and false otherwise.

**binary->=** *value1* *value2* * &key &allow-other-keys  => Generalized Boolean  &nbsp;&nbsp;&nbsp;&nbsp;[*Generic function*]<br>
Returns true if *value1* is greater or equal to than *value2*, and false otherwise.

**binary-<** *value1* *value2* * &key &allow-other-keys  => Generalized Boolean  &nbsp;&nbsp;&nbsp;&nbsp;[*Generic function*]<br>
Returns true if *value1* is less than *value2*, and false otherwise.

**binary-<=** *value1* *value2* * &key &allow-other-keys  => Generalized Boolean  &nbsp;&nbsp;&nbsp;&nbsp;[*Generic function*]<br>
Returns true if *value1* is less than or equal to *value2*, and false otherwise.

**binary-equivalent?** *value1* *value2* * &key &allow-other-keys  => Generalized Boolean  &nbsp;&nbsp;&nbsp;&nbsp;[*Generic function*]<br>
Returns true if *value1* is equivalent to *value2*, and false otherwise.

