# Racket solutions for nested data structure traversal

This nested data structure traversal problem is interesting due to the
combination of counter state transition and structural transformation of the
input data.

The sample solution's use of mutable state for both state transition and
structural transformation makes it both succinct and easy to understand.
However, it modifies the input data, which may not be desirable.

A purely functional approach produces a new result without modifying the input
data, but as other language solutions in this repository have shown, the
implementations are typically more complex and can be significantly more
difficult to understand.  The complexity is due to the entangling of state
transitions with structural transformation.

Luckily, we can compromise on functional purity to get the best of both worlds:

- use functional update for structural transformation of the data to avoid
  modifying the input
- use mutable counters to minimize concern with state transition during
  structural transformation

This compromise works well for this particular problem, and is expressed in its
simplest form in the `benign-mutation-X.rkt` examples.

However, a language like Racket supports unusual control operators which can
allow normal-looking code to be re-entered multiple times.  A transformation
which uses control operators to interrupt itself, and then resume from the same
point multiple times may corrupt the mutable counter state.  This is
demonstrated in the `benign-mutation-demonstrating-control-corruption.rkt`
example.

An alternative solution, "algebraic effects", demonstrates a technique that is
safe in the presence of arbitrary control transfer, while still maintaining the
ergonomics of mutable counters.  In this case, if the structural transformation
were changed to interrupt itself and resume multiple times, this would not
corrupt the counter state.  This is demonstrated in the
`algebraic-effects-demonstrating-control-safety.rkt` example.

Two variants of each solution are given: one direct, and another using an
auto-incrementing counter abstraction for additional convenience.


## References

- [Racket Continuations](https://docs.racket-lang.org/reference/cont.html)
- [Eff programming language](https://www.eff-lang.org/)
- [Delimited Continuations for Everyone by Kenichi Asai](https://www.youtube.com/watch?v=QNM-njddhIw)
