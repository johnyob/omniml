The Damas-Hindley-Milner (ML) type system owes its success to _principality_,
the property that every well-typed expression has a unique most general type.
This makes inference predictable and efficient. Yet, principality is _fragile_:
many extensions of ML—GADTs, higher-rank polymorphism, and static
overloading—break it by introducing _fragile_ constructs that resist
principal inference. Existing approaches recover principality through
_directional_ inference algorithms, which propagate _known_ type information in
a fixed (or _static_) order (e.g. as in bidirectional typing) to disambiguate
such constructs. However, the rigidity of a _static_ inference order often
causes otherwise well-typed programs to be rejected.

We propose _omnidirectional_ type inference, where type information flows in a
_dynamic_ order. Typing constraints may be solved in any order, suspending when
progress requires known type information and resuming once it becomes
available, using _suspended match constraints_. This approach is
straightforward for simply typed systems, but extending it to ML is challenging
due to _let-generalization_. Existing ML inference algorithms type
$\textsf{let}$-bindings $\textsf{let } x = e_1 \textsf{ in } e_2$ in a fixed
order—type $e_1$, generalize its type, and then type $e_2$. To overcome this,
we introduce _incremental instantiation_, allowing partially solved type
schemes containing suspended constraints to be instantiated, with a mechanism
to incrementally update instances as the scheme is refined. Omnidirectionality
provides a _general framework_ for restoring principality in the presence of
fragile features. We demonstrate its versatility on two fundamentally different
features of OCaml: static overloading of record labels and datatype
constructors and semi-explicit first-class polymorphism. In both cases, we
obtain a _principal_ type inference algorithm that is more expressive than
OCaml's current typechecker.
