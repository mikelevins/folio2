# Folio 2

## functional idioms in Common Lisp

folio is a collection of utilities intended to make it more convenient
to program in a functional style in Common Lisp. Common Lisp is a
multi-paradigm language in which many styles of programming are
possible, but none is made especially convenient at the expense of
others. folio is a set of enhancements that make functional idioms a
little more convenient.

folio is also a kind of miniature version of Bard, a non-pure
functional dialect of Lisp. folio's style and vocabulary are taken
from the design of Bard, and its features are mainly a subset of
Bard's.

folio has been used in building at least two commercial products, and
has seen heavy use on both. used judiciously, it provides compact and
clear idioms for expressing operations over complex data structures,
and good performance.

that said, of course it can always be improved, and I welcome comments
and criticism. you can reach me at `mevins @ me . com` if you care to
comment.

## changes in folio 2

folio 2 reorganizes folio's various subsystems to better reflect their
practical use. it makes it easier to use parts of the library without
needing to load all of it. 

I renamed the various packages that make up folio in response to a
criticism from whism on Reddit:

http://www.reddit.com/r/lisp/comments/1032hm/folio_is_a_set_of_modules_that_make_some/

he's right that the previous package names--although they were clear
and convenient--play poorly with other packages in the global
namespace. I've therefore reorganized and renamed the packages to
address this objection. folio packages now use the de facto standard
reverse-DNS naming convention. all folio packages have names
descending from `net.bardcode.folio`.

if you want shorter package names for use in your code, see the
discussion of package naming in "the folio umbrella system", below.

## dependencies

folio has a small number of essential dependencies. first, it assumes
it's being loaded by Zach Beane's Quicklisp:

http://www.quicklisp.org/beta/

It's certainly possible to load and use folio without Quicklisp, but
it's substantially quicker and easier with it.

folio depends critically on Scott Burson's FSet library of functional
data structures:

http://common-lisp.net/project/fset/Site/index.html

FSet is a great collection of efficient functional data structures
embedded in Common Lisp, and is very useful without folio. folio adds
to it a layer that provides uniform operators across both FSet and
Common Lisp data structures, as well as additional extensions and
support for Series.

beginning with version 2, folio also depends on Richard C. Waters'
SERIES package:

http://series.sourceforge.net/

the SERIES package provides a system of features that enable iterative
algorithms to be expressed as if they were functional operations on
finite and infinite series of values. folio 2 extends its syntactic
conveniences to support SERIES as well as FSet's functional data
structures. it provides common APIs for working with FSet's sequences,
sets, and maps, with SERIES, and with Common Lisp's native lists and
arrays.

## concerning hash-tables

a notable omission from folio is support for hash-tables. I've omitted
hash-table support intentionally because traditional
hash-tables--including Common Lisp's--are a poor fit for functional
idioms. hash-table are stateful by their nature; they are designed for
efficient mutation, and you lose most of their benefits if you try to
treat them in a functional fashion. it's certainly *possible* to
implement a pure-functional API for Common Lisp hash-tables, but it
would be perverse to do so, since the whole point of hash-tables is
that they provide an efficient way to record mutable data as a
key/value store.

for key/value storage that better fits the folio model, use FSet's
maps.

## organization

folio is intended to be useful piecemeal; that is, it should be
convenient to use those parts of folio that you find useful without
needing to load all of it. to this end, folio provides a separate
system definition for each of its subsystems, so that they can be used
independent of one another. another consequence of this goal is that
in a few cases auxiliary code is duplicated so that there are no
complicated dependencies among folio's parts.

the one common subsystem that must be loaded, no matter which parts of
folio you want to use, is `net.bardcode.folio.packages`. this
subsystem simply loads all of the folio package definitions, so that
all the package symbols are accessible. 

I use this strategy because, although each folio feature is defined
and named in one specific package, some of them are implemented in
several different ones. an example is the `type-for-copy` generic
function; it's defined in the Constructing subsystem, but there are
method definitions in other subsystems to support types dealt with in
those subsystems. 

it makes sense for the generic function to be defined in the
Constructing package, but it's more useful for method definitions
dealing with specific types to be made in the subsystems that deal
with those types. That way, you load those definitions only if you
have use for them, and you don't have to load subsystem you don't
need, just to get implementations of `type-for-copy`. but it also
means that you need the package in which the name `type-for-copy`
resides, so the Packages subsystem defines all of them in one go.

## syntactic sugar

some folio subsystems, such as Sequences and Tables, define reader
macros that provide shorthand syntaxes for commonly-used data
structures. For example, with Tables-syntax loaded you can create an
associative array with the syntax `#{ :a 1 :b 2 :c 3 }`. Not all Lisp
programmers like this kind of syntactic sugar, and some dislike custom
readtables; for that reason, these bits of syntactic sugar are
provided by separate subsystems. you don't have to load Tables-syntax
in order to use Tables; you can load the syntax extensions or not, as
you prefer.

## the folio umbrella system

folio provides an umbrella subsystem named `net.bardcode.folio` that
loads all of the other folio subsystems. use this subsystem if you
want all folio features.

besides loading everything in folio, the umbrella subsystem also
provides a convenient shorthand nickname. the package associated with
the umbrella system, `net.bardcode.folio`, has the nickname `folio`,
and it imports and re-exports all external symbols in all the folio
packages. if you want to use all of folio, and you want a convenient
shorthand name for package qualifiers, then the umbrella system is for
you.

you can use the same technique in your own code to create convenient
access to those parts of folio you want to use. for example, if you'd
like to use the functions from the Sequences subsystem along with the
`$` and `^` syntactic sugar from the Functions subsystem, here's how
you can do it:

let's suppose you want to use the package name `fn`; define your own
package with that name (or give it the nickname `fn`). in the package
definition, import all the external symbols from the folio Functions
and Sequences subsystems. Then export the names you want to
use. Here's what that looks like:


    (defpackage :net.example.functions
      (:nicknames :fn)
      (:use :cl :net.bardcode.folio.functions
            :net.bardcode.folio.sequences)
      (:export
       ;; functions
       :$ :^
       ;; sequences
       :add-first :add-last :any :by :drop :element :empty? :filter 
       :first :generate :last :length :map :next-last :partition
       :reduce :rest :reverse :scan :second :some? :take :take-by))

now, by using or importing your `fn` package, you can get access to
exactly the folio features you want with convenient (i.e. short)
names. for example:

    (fn:filter (fn:partial fn:some? (complement #'whitespacep))
               (fn:filter (complement
                           (fn:^ (line) 
                             (or (fn:empty? line)
                                 (comment? line))))
                          lines))


