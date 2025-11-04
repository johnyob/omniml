The Damas-Hindley-Milner (ML) type system owes its success to
_principality_, the property that every well-typed expression has
a unique most general type.  This makes inference predictable and
efficient. Yet many extensions of ML (for example GADTs, higher-rank
polymorphism, and static overloading) endanger principality by
introducing _fragile_ constructs that resist principal
inference. Existing approaches recover principality through
_directional_ inference algorithms, which propagate _known_ type
information in a fixed order (e.g. as in bidirectional typing) to
disambiguate such constructs. However, the rigidity of a fixed
inference order often causes otherwise well-typed programs to be
rejected.

We propose _omnidirectional_ type inference, in which typing
constraints may be solved in any order, suspending when progress
requires known type information and resuming once it becomes
available, using _suspended match constraints_. This approach is
straightforward for simply-typed systems, but extending it to ML is
challenging due to _let-generalization_. Existing ML inference
algorithms type `let`-bindings `let x = e1 in e2` in a fixed order:
type `e1`, generalize its type, and then type `e2`. To overcome this,
we introduce _incremental instantiation_, allowing partially solved
type schemes containing suspended constraints to be instantiated, with
a mechanism to incrementally update instances as the scheme is
refined.

It is also difficult to give a good declarative specification to this
inference mechanism -- the natural attempts to do this would allow
"out of thin air" behavior due to causality cycles between suspended
constraints. If time allows we will discuss the importance of
declarative semantics for type-inference algorithms, and sketch
our declarative semantics for suspended constraints.
