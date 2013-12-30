# folio

folio is a collection of libraries that extend Common Lisp, adding
some data types and extending Common Lisp functions to handle more
types in a more uniform way.

A more succinct way to describe folio is that it offers a Common Lisp
implementation of many of the common idioms from the Bard programming
language.

- folio adds series and pure functional maps and sequences
- it extends Common Lisp sequence functions to support the added types
- it adds extensible support for easily converting among many types
- it adds extensible support for easily constructing values of many types
- it provides generic, extensible versions of Common Lisp functions
  that are not generic or extensible in the standard

## Types

A **box** is a simple, mutable container for a value. The most common
use for a box is to store a mutable value in an immutable data
structure, enabling you to introduce just enough mutability into an
otherwise pure-functional algorithm or data structure.

A **pair** is an object that associates two values, called its
**left** and **right** elements. The most obvious example of a
pair is Common Lisp's `CONS` type, but the folio pair API is
generic and extensible; you can add your own pair types.

The **Maps** package provides functional implementations of finite
maps.

The **Sequences** package provides a uniform API for constructing and
manipulating several types of sequences, including Common Lisp's
standard lists, vectors, and strings, but also FSet's pure-functional,
immutable sequences. 

The **Series** package extends **Sequences** to construct and handle
sequences with unbounded length using Richard Waters' SERIES package.

The **Sets** package extends **Sequences** to support treating
sequences as sets.

The **Taps** package adds **taps**. A tap is a function that creates a
sequence or a series of the elements in a data structure or a
stream. A tap can present a map as a sequence of pairs, for example,
or present a file as a series of lines.

## Functions and conveniences

**As** provides a single extensible generic function, `as`, for
converting values from one type to another.

**Comparisons** offers generic, extensible versions of magnitude
comparisons and equivalence predicates.

**Functions** provides various conveniences for working with
functions, including partial application and composition operators. It
also offers a compact shorthand for Common Lisp's LAMBDA, to reduce
the visual clutter of using anonymous functions.

**Make** provides a single extensible generic function for
constructing values.

