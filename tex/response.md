## [Reviewer B](https://popl26.hotcrp.com/paper/127#r127B)

Now, in the interest of helping you strengthen your paper for a possible
future submission, I will just say that I think you somewhat oversimplify
the difference between static and dynamic overloading. It seems to me that
most dynamic overloading approaches can be made to support static
overloading simply by adding a global variable that tracks what overload has
been used, making sure that by the end of constraint solving, exactly one
overload has been used (this global variable is what I was referring to, in
my review, as a "monomorphic row variable" and SML's approach). This
approach is mostly orthogonal to the rest of the overloading resolution
process, which deals with handling ambiguity at the type level and can
leverage let-polymorphism, and it trivially solves the problem you described
in:

>   one key difference is that a conditional constraint (in Pottier's work)
>   that never fires is considered a success

In principle, the approach I describe above can be used to adapt most
existing work on dynamic overloading, such as qualified types and the system
of Pottier [2000], to address static overloading instead.

For instance, if we wanted to adapt qualified types, let's consider
what would happen with a variation on your ex8 below:

        let getx r = r.x in (getx {x = 1; y = 1}, getx {x = "ok"; y = 1})

A natural qualified type to infer for getx would be getx : forall a
b. HasField a "x" b => a -> b. Now, turning to static overloading, we would
like to add a monomorphic/global "choicen" variable to the qualifier,
yielding getx : forall a b. HasField a "x" b _choice => a -> b. The
processing of the body of the binding would then instantiate the type twice:

- once into ?a0 -> ?b0 where HasField ?a0 "x" ?b0 _choice and ?a0 = One
  Int, resulting in resolved constraints ?b0 = Int and _choice = One. 

- once into ?a1 -> ?b1 where HasField ?a1 "x" ?b1 _choice and ?a1 = One
  String, resulting in resolved constraints ?b1 = String and _choice = One. 

Overall, constraint solving would succeed, and the program could be compiled
to statically select the correct field projection, based on the value of
_choice.

Naturally, both of the examples below would instead fail successfully:

        let getx r = r.x in ()
        let getx r = r.x in (getx {x = 1; y = 1}, getx {x = 1; z = 1})

as _choice would become respectively underdetermined and overdetermined.

(This _choice variable is evidently quite similar to an OCaml
"weakly polymorphic" type variable.) 

To be convincing, your paper should probably acknowledge that this approach
is feasible and explain in detail why your own approach is better and
justifies its complexity cost.


## Gabriel 

We thank the reviewer for their time and careful consideration. We are
almost done implementing our revision plan for the paper, and will
submit it somewhere else. (We are thinking TOPLAS, due to difficulties
attending other SIGPLAN conferences next year.)

In particular we are grateful for the post-rebuttal comments in review
B. We will improve our presentation to discuss this, but we thought
that you may be interested in a direct reply as well (you may be
curious about this point, but unwilling to go back to a final version
of our work in a distant future).

Why do we want suspended constraint rather than this trick of
constraining qualified types with a global weak type variable?

1. Suspended constraints are strictly more expressive, because
   arbitrary constraint fragments can be delayed. In the proposed
   approach of "global qualified types", things work well for record
   fields because the typing rule for those record fields is fairly
   simple (it's basically one unification) and uniform (all record
   rules are handled the same).
   
   But in fact we want static overloading to resolve constructs where
   the typing rule may carry arbitrary constraints, and more
   importantly the typing rule can depend on the disambiguation
   choice:
   
   - In OCaml, GADTs and non-GADTs constructors have different typing
     rules, for example in pattern-matching GADTs must introduce
     existential variables according to their declaration, with the
     right-hand-side of the clause checked in this context. We of
     course cannot tell whether a constructor introduces existential
     GADT variables before disambiguating it.
   
   - Dually, monomorphic and polymorphic record fields have different
     typing rules, as polymorphic record fields involve polytype-like
     rules. Another way to view it is that all record fields are
     monomorphic, but some carry polytype-boxed types, and those have
     non-uniform typing rules after disambiguation.

2. The proposal does not solve the problem of providing a declarative
   semantics, which is one of the key contribution of our
   work. Suppose type-checking succeeds without forcing a choice for
   the `_choice` variable; what should the type-checker do? Of course
   this depends on how `_choice` is introduced by the constraint
   generator. A simple idea is to introduce it via an existential
   quantification at the toplevel, but this is not the semantics that
   we want and that you have in mind: to be complete with respect to
   this semantics, the solver should start exhaustively listing
   possible record types and picking one that works.

   Review B instead suggests (and we agree) that under-determined
   choice variables should result in the constraint failing. But how
   do we specify declarative semantics for the constraint language
   that do this?  Basically we want to succeed when the choice is
   "known" from the context, and fail otherwise. This is precisely the
   purpose of the unicity conditions that we introduce in our work.

In summary, we would say that this idea of mixing qualified types with
global choice variables is a reasonable implementation device for some
of the features we consider (not all of them), but that it is unclear
how to specify its behavior via declarative constraint semantics or
declarative typing rules.

This illustrates a tension that we have in presenting our work. We
don't think that starting with polytypes (or GADTs!) to expose the
idea is reasonable, as they are fairly complex. We tried to show the
simplest possible case of tuples first, but that is then too simple
and row variables provide an acceptable solution. For now we are
planning to stick to monomorphic records as our driving vehicle, but
better highlight that we are really looking for a general solution,
rather than support for this particular language feature.
