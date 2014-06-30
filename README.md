bracer
======

Bracer is a system for parsing, rewriting, and extending the capabilities of curly-brace languages such as C, Go, Javascript, and Swift. It is written in Haskell, piggybacking on top of Edward Kmett's [trifecta][trifecta] library and Patrick Bahr's [compdata][compdata] for creating and rewriting compositional data types. 
It is intended to be simple, powerful, and easy to extend, making tradeoffs for flexibility rather than strict language compliance.
It is in very early alpha; only the C backend works right now.
Pull requests are enthusiastically accepted.


Infrequently Asked Questions
----------------------------

**Why not use libclang for this?**

Clang is, needless to say, a brilliant piece of infrastructure. However, it has some disadvantages: it is extremely monolithic, very C++ focused, and has a significant learning curve.


[compdata]: http://hackage.haskell.org/package/compdata
[trifecta]: http://hackage.haskell.org/package/trifecta
