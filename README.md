bracer
======

Bracer is a system for parsing, rewriting, and extending the capabilities of curly-brace languages such as C, Go, Javascript, and Swift. It is written in Haskell, piggybacking on top of Edward Kmett's [trifecta][trifecta] library and Patrick Bahr's [compdata][compdata] for creating and rewriting compositional data types. 
It is intended to be simple, powerful, and easy to extend, making tradeoffs for flexibility rather than strict language compliance.
It is in very early alpha; only the C backend works right now.
Pull requests are enthusiastically accepted.


Infrequently Asked Questions
----------------------------

**What's interesting about Bracer?**

Bracer is much more minimal and generalizable than existing analysis/rewriting infrastructures such as CIL. It uses a non-traditional syntax tree based on compositionality (using coproducts of functors) rather than monolithic inheritance. It uses the sum-of-products approach to solve the [expression problem][exprob], combined with a novel use of GHC's support for type families to provide extensible parser constructs. Rather than relying on OO-style concepts like visitors for tree traversals, it provides modular rewriting constructs: morphisms, structured recursion schemes, and tree and macro automata. 

**Why not use libclang for this?**

Clang is, needless to say, a brilliant piece of infrastructure. However, it has some disadvantages: it is extremely monolithic, very C++ focused, and has a significant learning curve.


[compdata]: http://hackage.haskell.org/package/compdata
[trifecta]: http://hackage.haskell.org/package/trifecta
[exprob]: http://en.wikipedia.org/wiki/Expression_problem
