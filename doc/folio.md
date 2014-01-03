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

## Syntax

folio provides a few extensions to Common Lisp syntax for the sake of convenience. Some, particularly those in the **functions** package, are regular macros. A few are reader macros which alter the lexical syntax of Common Lisp.

Because reader macros can sometimes conflict with reader macros defined by other libraries or by user code, the folio syntax extensions that define reader macros are optional. They're provided by separate subsystems that can be loaded or not, as you prefer.

### Type-constraint syntax

The **as** package provides a reader macro that makes inline type-conversions easy. For example:

    CL-USER> #[list](list 1 2 3)
    (1 2 3)
    CL-USER> #[vector](list 1 2 3)
    #(1 2 3)
    CL-USER> #[fset:wb-seq](list 1 2 3)
    #[ 1 2 3 ]

When you write

    #[vector]
    
it means that the value of the next expression read is to be treated as a vector. More specifically, the reader macro causes this expression:

    #[vector](list 1 2 3)

to be read as this one:

    (as 'vector (list 1 2 3))

In order for the reader macro to work properly, a method definition must exist for the function **as** that specializes on the target type and the type of the input expressions. The vector example works because the **sequences** package provides a specialization for **as** on `(vector cons)`.


### Map and sequence syntax

The optional syntax extension of the **sequences** package provides a succinct way of writing literal lists:

    CL-USER> [1 2 3]
    (1 2 3)

By combining this syntax with the type-constraint syntax, you can easily construct various types of sequences with compact expressions:

    CL-USER> #[list][1 2 3]
    (1 2 3)
    CL-USER> #[vector][1 2 3]
    #(1 2 3)
    CL-USER> #[fset:wb-seq][1 2 3]
    #[ 1 2 3 ]

Similarly, the **maps** package provides a compact syntax for maps:

    CL-USER> {}
    { }
    CL-USER> {:a 1 :b 2}
    { :A 1 :B 2 }

The **maps** reader macro constructs a map of type `fset:wb-map`. As with the literal syntax for lists, you can use **as** or the type-constraint syntax to convert maps to any supported map representation.

Again, each of these syntax extensions is optional; you can elect not to load them if you choose not to use them,

## Style and conventions

folio code has its own style and flavor, which emphasizes generating
collections of values and mapping functions over them. This style is
derived from the experimental Lisp dialect Bard, which in turn owes a
considerable debt to Dylan, ML, Scheme, and Haskell.

For examples of folio style, see the sample code provided in the
examples directory.

folio itself observes a few conventions in its code. When computing new sequences, maps, and series from inputs that are sequences, maps, or series, folio in most cases makes an effort to return a result that is of the type as the first input. In a few cases, where this convention seems clearly contrary to the spirit of the function, this rule is broken.

## The name

The name "folio" is a little obscure and arbitrary. It's a term from the craft of printing that refers to certain esoteric details about how books are printed, but I chose it for its relation to the works of Shakespeare: the **First Folio** is an early printed edition of Shakespeare's plays, published in 1623.

What has William Shakespeare to do with Lisp libraries? Nothing in particular; the choice of "folio" as a name for this library encapsulates a little bit of personal history.

The folio library has its origin in work I've done on an experimental dialect of Lisp named **Bard**. The folio library reflects the style and design of Bard in several respects.

Bard in turn incorporates influences from several programming languages, but undoubtedly the most important in Dylan. Although Bard no longer closely resembles Dylan in its design, it began years ago as a simple Lisp that closely resembled Dylan before that language gained its infix syntax and lost its Lisp-like s-expressions.

Dylan in turn got its name during an extended discussion among its designers and users at Apple, Inc. During development it was called "Ralph", after Ralph Ellison, but the consensus was that it needed a more euphonious name. Many were proposed, most of them not as good as "Ralph". Late in the game, some of us raised the point that there was a venerable tradition of naming languages for inspiring people--for scientists such as Blaise Pascal, mathematicians such as Haskell Curry, and intellectual pioneers such as Lady Ada Lovelace. In that context, the name "Dylan" was proposed. It seemed particularly apropos because, on the one hand it was a reference to Dylan Thomas, a poet that many of us loved, and on the other hand it was easy to conceive of it as a contraction of "Dynamic Language", which it most certainly was.

In later years, when I was trying to think of a name for the Dylan-influenced language I'd been working on, I hit upon the idea of calling it "Bard". A bard is a poet, which made the name an oblique reference to its ancestor, Dylan. It seemed appropriate that the reference was oblique and general, because the language had evolved away from Dylan as I worked on it.

At the same time, "the Bard" (or "the Bard of Avon") is of course a conventional way to refer to William Shakespeare. Besides working on Dylan and the Newton, I spent part of my time at Apple working on SK8, a very powerful authoring and application-development tool. Metaphors drawn from theater and acting were common in SK8--for example, the abstract container that represented all the objects visible on the screen during a session was called the **stage**.

It's always been my ultimate intention to recapitulate some of the best features of these older programming tools, including SK8. That being the case, it seemed satisfying to give my experimental programming language a name that referred to Shakespeare as well as the poetic tradition, and that thereby connected it, however loosely, with Dylan and SK8. 

When it came time to give a name to the Common Lisp library I was using to support and expedite my work on Bard, I naturally turned to thoughts of the literary, of poets, and of SHakespeare. "folio" is the name that fell out.

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

