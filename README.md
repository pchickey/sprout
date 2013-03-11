Sprout language
===============

Sprout is a toy language for a small vm.

Sprout expressions use GADTs to enforce a well-typed interface, and compile to an
untyped AST, in Sprout.Language.Expressions. This code is largely is cribbed from
[Tom Hawkins's Atom language][atomlang], but with only a subset of the types and
expressions, for simplicity.

At some point soon I will add frontend for a shallow embedding in Haskell.


The Sprout AST can be compiled to a very simple VM in Sprout.Language.Machine.
At some point soon I will add a haskell implementation of the VM.

[atomlang]: http://github.com/tomahawkins/atom
