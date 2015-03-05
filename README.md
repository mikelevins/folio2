# folio 2

folio 2 is a collection of small libraries that provide support for
functional idioms and data structures in Common Lisp and a common set
of APIs for working with them.

It's a direct descendant of the older and simpler folio library, with
a greatly expanded and reorganized API, and support for more data
structures and procedures.

folio 2 is organized so that users can load and use the entire
collection of functions, macros, and types, or just those parts of
the library that are needed. It provides several purely-optional
syntactic extensions for convenience.

folio 2 includes a small but nontrivial example program that
illustrates the use of several of its features to provide good
expressive power in compact, readable code.

folio 2 has been tested with SBCL, Clozure Common Lisp 1.10, and
Lispworks 6.1.

## Features

folio 2 offers the following features:

- series and pure-functional maps and sequences
- extension of Common Lisp sequence functions to support the new types
- extensible generic versions of many Common Lisp functions
- an extensible type-conversion utility
- an extensible contructor function for arbitrary values
- seamless integration of the SERIES and FSet libraries

## Types

A **box** is a simple, mutable container for a value. The most common
use for a box is to store a mutable value in an immutable data
structure, enabling you to introduce just as much mutability as you
need into an otherwise pure-functional algorithm or data structure.

A **pair** is an object that associates two values, called its
**left** and **right** elements. The most obvious example of a
pair is Common Lisp's `CONS` type, but the folio 2 pair API is
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

## Included libraries

folio 2 depends on six other libraries: **Quicklisp**, **FSet**,
**SERIES**, **Alexandria**, **Closer-MOP**, and **ASDF**. The features
provided by these libraries are available when folio 2 is loaded. In
particular, the full range of functional data structures from FSet,
and the functions and macros provided for working with them, are
available in the FSET package. Similarly, the series, generator, and
gatherer data structures from the SERIES library, and all of the
documented functions and macros for working with them are available in
the SERIES package.

### FSet

For documentation and other information about FSet, see the FSet Tutorial:

http://common-lisp.net/project/fset/Site/FSet-Tutorial.html

### SERIES

For documentation and other information about SERIES, see the SERIES homepage:

http://series.sourceforge.net/

and appendices A and B of *Common Lisp the Language, 2nd Edition*:

http://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node347.html#SECTION003400000000000000000

http://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node362.html#SECTION003500000000000000000

### ASDF

You can find documentation of ASDF at

http://common-lisp.net/project/asdf/

## Style and conventions

folio 2 code has its own style and flavor, which emphasizes generating
collections of values and mapping functions over them. This style is
derived from the experimental Lisp dialect Bard, which in turn owes a
considerable debt to Dylan, ML, Scheme, and Haskell.

For examples of folio 2 style, see the sample code provided in the
examples directory.

folio 2 tries to produce results of predictable type. Its usual
convention is that when its inputs are sequences or series, the output
will be of the same type as the leftmost input. In a few cases, folio
2 breaks this rule in order to avoid results that would be
inconvenient or at odds with the spirit of a function.


## Using folio 2

folio 2 includes an umbrella system definition in folio2.asd. If you want
to use all of folio 2, the easiest way to do it is to depend on that
system definition. All of the function, macros, and type names defined
in folio 2 are exported from the package `net.bardcode.folio2`, which
defines the nickname `folio2`. With the umbrella system loaded, you can
use any folio 2 feature by prefixing its name with the package nickname
`folio2`. For example:

    CL-USER> (folio2:any (as 'cl:list (folio2:scan "abcdefgh")))
    #\h

On the other hand, experience has taught me that people using folio 2
often want to use some specific part of the library, but not all of
it. For that reason, folio 2 is organized so that you can load parts
of it without requiring the whole thing. There are a few
dependencies within the library. The **as** and **make** subsystems
are used by all of the data-structure sections. **taps** relies on
**series**, which in turn relies on **sequences**. You shouldn't need
to concern yourself with these dependencies, though. The ASDF system
definitions declare the needed dependencies, so you can simply load
the folio 2 subsystem you want, and rely on ASDF to ensure that any
needed dependencies are also loaded.

### Reader macros

Three folio 2 subsystems provide reader macros that extend the lexical
syntax of Common Lisp with notational conveniences. Common Lisp
programmers don't always like reader macros. Although they can be very
convenient, they can also conflict with locally-defined reader macros.

In order to avoid problems caused by reader-macro conflicts, the folio
2 reader macros are entirely optional. You can choose not to load them
if they would cause problems for you, or if you simply don't like
reader macros. Each syntax extension is loaded by its own separate
ASDF system definition. If you want to avoid loading the reader
macros, simply don't load those systems.

### Systems and packages

Following are the subsystems and packages provided by folio 2:

| subsystem | type | purpose |
|-----------|------|---------|
| as[1] | a single generic function | extensible type-conversion utility |
| boxes | data structures and API | mutable container |
|functions | functions and macros | functional idioms |
| make | a single generic function | extensible value constructor |
| maps[1] | data structures and API | functional finite maps |
| pairs | data structures and API | common extensible pair API |
| sequences[1] | data structures and API | common extensible sequences API |
| series | data structures and API | extends the sequences API to work with unbounded series |
| taps | data structures and API | an API for constructing series from inputs and data structures |

[1] these subsystems provide optional reader macros

folio 2's subsystems have the following library dependencies:

| subsystem | dependencies |
|-----------|------|---------|
|functions | Alexandria
| maps | FSet
| sequences | FSet
| series | FSet, SERIES
| taps | FSet, SERIES, Closer-MOP


The most convenient way to use folio 2 is usually to just load the
umbrella system, `folio2`. If you prefer to customize loading and
control which names are exported to your code, you may want to make
your own umbrella system. In that case, the definition of `folio2` in
folio2.asd serves as a guide. You will probably also want to define
your own package in order to control the visibility of names from the
folio 2-package.lisp, which defines the umbrella package for folio 2.

An approach that works well is to define a common package for your
code, use the COMMON-LISP package, and import the folio 2 symbols you
want to be accessible. Again, folio2-package.lisp can offer some
guidance. Symbols in the :shadowing-inport-from lists in that file are
defined in the COMMON-LISP package, and you'll need to similarly use
shadowing import if you want to import them. Symbols in the
:import-from lists are defined in the folio 2 sources or the libraries
it depends on, and should be safe to import directly, assuming they
don't conflict with any symbols you've defined yourself.

The **as** and **make** subsystems should be safe to `USE`, unless
you've defined your own functions or macros named `as` and
`make`. Each of those subsystems defines just one function and exports
just one symbol. They are intended to be safe for `USE`.

For an example of code that uses the folio 2 umbrella package, see
examples/name-generator.lisp.

## The name

folio 2 is named for its direct predecessor, folio.

The name "folio" is a little obscure and arbitrary. It's a term from
the craft of printing that refers to certain esoteric details about
how books are printed, but I chose it for its relation to the works of
Shakespeare: the **First Folio** is an early printed edition of
Shakespeare's plays, published in 1623.

What has William Shakespeare to do with Lisp libraries? Nothing in
particular; the choice of "folio" as a name for this library
encapsulates a little bit of personal history.

The folio library has its origin in work I've done on an experimental
dialect of Lisp named **Bard**. The folio library reflects the style
and design of Bard in several respects.

Bard in turn incorporates influences from several programming
languages, but undoubtedly the most important is Dylan. Although Bard
no longer particularly resembles Dylan, it began years ago as a simple
Dylan clone, based on the versions of Dylan before it lost its
s-expression syntax.

Dylan got its name during an extended discussion among its designers
and users at Apple, Inc. During development it was called "Ralph",
after Ralph Ellison, but the consensus was that it needed a more
euphonious name. Many were proposed, most of them not as good as
"Ralph". Late in the game, some of us raised the point that there was
a venerable tradition of naming languages for inspiring people--for
scientists such as Blaise Pascal, mathematicians such as Haskell
Curry, and intellectual pioneers such as Lady Ada Lovelace. In that
context, the name "Dylan" was proposed. It seemed apropos because, on
the one hand it was a reference to Dylan Thomas, a poet that many of
us loved, and on the other hand it was easy to conceive of it as a
contraction of "dynamic language", which aptly described Dylan.

In later years, when I was trying to think of a name for the
Dylan-influenced language I'd been working on, I hit upon the idea of
calling it "Bard". A bard is a poet, which made the name an oblique
reference to its ancestor, Dylan. It seemed appropriate that the
reference was oblique and general, because the language had evolved
away from Dylan as I worked on it.

At the same time, "the Bard" (or "the Bard of Avon") is of course a
conventional way to refer to William Shakespeare. Besides working on
Dylan and the Newton, I spent part of my time at Apple working on SK8,
a very powerful authoring and application-development tool that was
written in Common Lisp--one of Dylan's immediate ancestors. Metaphors
drawn from theater and acting were common in SK8--for example, the
abstract container that represented all the objects visible on the
screen during a session was called the **stage**.

"Bard" was therefore an oblique reference to two past projects that
had influenced and informed the design of the language.

When it came time to give a name to the Common Lisp library I was
using to support and expedite my work on Bard, I naturally turned to
thoughts of the literary, of poets, and of Shakespeare. "folio" is the
name that fell out.


