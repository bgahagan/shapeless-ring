Shapeless Ring
==============

An experiment re-implementing parts of clojure's ring library in a type-safe manner using shapeless records and labeled generics.


Development
-----------

Due to what I assume is a compiler bug, changing app.scala and incrementally re-compiling can cause compile errors.

Develop in sbt using:
```
~ ; clean ; run-main App
```
