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

I thought a bit about the scheme proposed by review B to turn dynamic
overloading into static overloading via global weak variables (a.k.a. our
"choice points"). I see several points:

- It is unclear to me that arbitrary constraints can be encoded in this
  way. I'm not sure it works for features where the type-checking rules depend
  on the disambiguation choice--for example using GADT pattern-matching
  rules after the construc tor has been disambiguated into a GADT
  constructor.

- Every time we use a polymorphic name that contains a suspended constraint
  encoded in this way, in his example (HasField a "x" b _choice), an
  instance of the type will be taken, and it will then be re-generalized when
  the current region gets generalized. This can lead to a quadratic blowup in
  the number of copies of these suspended-constraint fragments sharing a
  single weak choice variable, for example

        let f3 =
          let f2 =
            let f1 : forall a b . HasField a "x" b _choice = ... in
            ... f1, ... f1
          in ... f2, ... f2, ... f2

  In this example, f3 will get 2*3 = 6 different copies of (Hasfield a_i "x"
  b_i _choice) in its generalized type. 

- We still need to provide declarative semantics for these choice points, and
  the obvious choice does not work: if the weak global variable choice is
  quantified existentially at the toplevel, then the semantics force the
  solver to backtrack over any potential choice to be complete. We want
  declarative semantics that express that _choice must be _known_, and this is
  one of the problems we are solving.

