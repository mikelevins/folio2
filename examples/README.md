# name-generator

The name-generator program uses a simple travesty technique to turn a
finite list of sample names into an infinite supply of
randomly-generated names with a similar style.

The method is straightforward: read a list of names and break them
into three-character chunks. Choose an arbitrary starting chunk. Find
a random chunk that can be fitted to the starting chunk. Repeat until
no candidate chunks are to be found. Return the result.

The implementation here is not dsigned to be particularly efficient or
optimal in any other way. Its sole purpose is to provide a simple, but
not trivial, illustration of the use of a variety of folio
features. With luck, the implementation of this travesty generator in
70 lines of code--which are not particularly optimized for
terseness--should demonstrate some opportunities for expressiveness
that folio 2 offers.



