# folio

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

## Types

A **box** is a simple, mutable container for a value. The most common
use for a box is to store a mutable value in an immutable data
structure, enabling you to introduce just as much mutability as you
need into an otherwise pure-functional algorithm or data structure.

A **pair** is an object that associates two values, called its
**left** and **right** elements. The most obvious example of a
pair is Common Lisp's `CONS` type, but the folio pair API is
generic and extensible; you can add your own pair types.

The **Maps** package provides functional implementations of finite
maps, and provides a common, extensible API for several different
representations.

The **Sequences** package provides a uniform API for constructing and
manipulating ordered collections of values, including both standard
Common Lisp sequence types and additional types, including functional
collection types provided by Scott Burson's FSet package.

The **Series** package extends **Sequences** to work with Richard
Waters' SERIES package, and to conveniently handle series as unbounded
sequences.

## Functions and conveniences

**As** provides a single extensible generic function, `as`, for
converting values from one type to another.

**Comparisons** offers generic, extensible versions of magnitude
comparisons and equivalence predicates.

**Copy** provides the generic functions `copy`, `deep-copy`, and
`shallow-copy`, which together provide an extensible common framework
for copying values.

**Functions** provides various conveniences for working with
functions, including partial application and composition operators. It
also offers a compact shorthand for Common Lisp's LAMBDA, to reduce
the visual clutter of using anonymous functions.

**Make** provides a single extensible generic function for
constructing values.

A **Tap** is a function that constructs a series that produces values
by reading some data structure or stream. The **Taps** package
provides a set of such functions, For example, the `characters`
function returns a series of the characters from a file or a
string. The `slots` function returns a series of pairs whose left
elements are the names of slots on a map, a hash-table, or an instance
of a CLOS class, and whose right elements are the associated values.


## Style

folio code has its own style and flavor, which emphasizes generating
collections of values and mapping functions over them. This style is
derived from the experimental Lisp dialect Bard, which in turn owes a
considerable debt to Dylan, ML, Scheme, and Haskell.

For examples of folio style, see the sample code provided in the
examples directory.

## Dependencies and Acknowledgements

folio depends on several other Common Lisp libraries. 

Quicklisp is Zach Beane's indispensible Lisp library manager. All of
folio's dependencies are available through QcuikLisp.

http://www.quicklisp.org/beta/

**FSet** is Scott Burson's library of efficient pure-functional data
structures. folio relies on FSet structures to implement
pure-functional maps and sequences.

http://common-lisp.net/project/fset/Site/index.html

**SERIES** is Richard Waters' set of tools for describing loops and
iterative processes as functions mapped over (possibly unbounded)
sequential data structures. In folio, series are simply sequences that
happen to be capable of being infinitely long.

http://series.sourceforge.net/

**Alexandria** is a collection of portable public-domain utilities for
Common Lisp designed to provide some common conveniences in a
conservative manner. folio 2 uses Alexandrai versions of some
utilities--notably partial application--rather than relying on
homegrown versions.

http://common-lisp.net/project/alexandria/

