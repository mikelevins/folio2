# folio
A library of functional idioms for Common Lisp
version 2.0

by mikel evins

folio is a collection of small libraries that provide support for
functional idioms and data structures and a common set of APIs for
working with them.

folio offers the following features:

- series and pure-functional maps and sequences
- extension of Common Lisp sequence functions to support the new types
- extensible generic versions of many Common Lisp  functions
- an extensible type-conversion utility
- an extensible contructor function for arbitrary values
- extensible comparison functions

folio incorporates and depends on Zach Beabe's Quicklisp, and on three
other libraries available through Quicklisp:

* **FSet**, is Scott Burson's library of efficient pure-functional
  data structures

* **SERIES**, Richard Waters' set of tools for describing loops and
  iterative processes as functions mapped over (possibly unbounded)
  sequential data structures

* **Alexandria**, a collection of portable public-domain utilities for
  Common Lisp

You can learn more about folio by reading doc/folio.md.

You can learn more about each sub-library in folio by reading the
doc/*.md file for the sub-library. For example, the functions library
is described in functions/doc/functions.md, and the maps library is
described in maps/doc/maps.md.

folio is distributed under the terms of the Lisp GNU Lesser General
Public License, also known as the LLGPL. See the file LICENSE for more
information.


