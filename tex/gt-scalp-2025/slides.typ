#import "@preview/touying:0.6.1": *
#import themes.metropolis: *

#import "@preview/curryst:0.5.1": rule, prooftree

#show: metropolis-theme.with(
  aspect-ratio: "16-9",
  footer: self => self.info.institution,
  config-info(
    title: [Type inference any way],
    author: [
      #let face(name) = [
        #image("pictures/" + name + ".jpg", height:8em)
        #v(0.6em)
      ]
      #grid(columns: (1fr,) * 3,
        face("Alistair_OBrien"),
        face("Didier_Remy"),
        face("Gabriel_Scherer"),
        [Alistair O'Brien \ Cambridge],
        [Didier Remy \ INRIA Paris],
        [Gabriel Scherer (speaker) \ INRIA & IRIF, Paris],
      )
    ],
    date: datetime.today(),
    institution: [GT SCALP 2025],
  ),
)

#show raw: set text(size: 18pt)

#show link: set text(blue)

#title-slide()

#let h1 = h(1fr)
#let v1 = v(1fr)

#let sh = $sigma.alt$
#let letin(x,t,u) = $"let" #x = #t "in" #u$
#let lam(x,t) = $lambda #x. thin #t$
#let app(t,u) = $#t med #u$
#let bind(x,A) = $#x ":" #A$
#let annot(t, A) = $(#t : #A)$
#let tylam(alpha, t) = $Lambda #alpha. thin #t$
#let tyapp(t, ty) = $#t med [#ty]$
#let hole = $square$
#let all(alpha, A) = $forall #alpha. thin #A$
#let tycon(c, As) = $app(#As, #c)$
#let ctx = $cal(C)$

#let matchwith(alpha, sh, C) = $"match" #alpha "with" #sh -> #C$
#let unicityH(E, sh, es) = $#E lr([hole gt.tri #sh | #es thin])$

#let erase(t) = $lr(floor.l #t floor.r)$
#let ground(A) = $A_"ground"$
#let magic(sq, ts) = $lr([#sq mid(|) #ts thin])$


#let der = $thick tack.r thick$
#let derr(label) = $thick attach(tack.r, tr: #label) thick$
#let derinf = derr("inf")
#let sat = $thick tack.r.double thick$

#let constraint_gen(t, A) = $attach(br: #A, bracket.l.double #t bracket.r.double)$

#let infer(..args) = prooftree(min-premise-spacing: 1.5em, rule(..args))



= Context: type-based disambiguation for OCaml

== Example

```ocaml
type boolean = True | False
```
#pause
```ocaml
type formula = True | False | And of formula * formula
```
#pause
```ocaml
let rec eval : formula -> boolean =
  function
  | True -> True
  | False -> False
  | And (f1, f2) ->
    match eval f1, eval f2 with
    | True, True -> True
    | _ -> False
```

== Features

Type-based disambiguation of:

- #pause ```ocaml Printf.printf "%d: %s"```: string or format? (since forever)
- #pause ```ocaml Foo, x.foo```: constructors and fields (since 4.01 (2013))
- #pause ```ocaml let (~x, ..) = point in x+1```: labeled tuples (since 5.4)
- #pause ```ocaml ([| 0. |] : _ Iarray.t), ([| 0. |] : floatarray)```: array literals (since 5.4)

== Failures

#v1

Error or warnings when type information is not *(robustly) known*:

#v1

```ocaml
let xp1 point =                 let (~x, ..) = point in x+1
(*                                  ^^^^^^^^       
Error: Could not determine the type of this partial tuple pattern. *)

let point = (~x:41, ~y:31) in   let (~x, ..) = point in x+1;;
(* - : int = 42 *)
```

#v1#pause

```ocaml
let of_bool b = if b then (True : boolean) else False
(*                                              ^^^^^
  Warning 18 [not-principal]: this type-based
  constructor disambiguation is not principal. *)
```

#v1

== Robustly known?

#v1

Two complementary mechanisms:

1. Syntax-directed propagation of type annotations (bidirectional type inference).

   ```ocaml
   let rec eval : formula -> boolean = function
     | True -> True | [...]
   ```

#v1

2. Propagation from `let`-definitions to their uses. (Hindley-Milner type inference)

   ```ocaml
   let point = (~x:41, ~y:31) in
   let (~x, ..) = point in [...];;
   ```

#v1
  
not 3. unification alone is *not* sufficient

```ocaml
  if b then True else (False : boolean) (* fails *)
  if b then (True : boolean) else False (* warns *)
```

#v1

== Problem: we always want more

#v1

```ocaml
type point2 = { x : int; y : int }
type point3 = { x : int; y : int; z:int }

let f (p : point2) =
  let {x; _} = p in x+1
(*    ^^^^^^
Warning 41 [ambiguous-name]: these field labels belong to several
types: "point3" "point2"
The first one was selected. Please disambiguate if this is wrong.

Error: The value "p" has type "point2" but an expression
was expected of type "point3"                            *)
```

#v1


= Practice: \ #h1 omnidirectional inference

== Omni-directional

#v1

If we don't have type information yet, _wait_.

#v1

```ocaml
  if b then True else (False : boolean)
```

#v1

We introduce *suspended* type-inference constraints
\ that get *discharged* when type information becomes available.

$ matchwith(alpha, sh, C) $

#v1#pause

If a constraint remains suspended at the end,
it is an *error*.

#v1

== Generation of suspended constraints

#v1

#let condcolor = red
#let testcolor = olive
#let thencolor = blue
#let elsecolor = purple

$
  #text(fill: condcolor)[if]
  #text(fill: testcolor)[$b^alpha$]
  #text(fill: condcolor)[then]
  #text(fill: thencolor)[$"True"^beta$]
  #text(fill: condcolor)[else]
  #text(fill: elsecolor)[$("False" : "boolean")^gamma$]
$

$ ==> $
#pause
$
    #text(fill: testcolor)[$alpha = "bool"$]
    quad and quad
    #text(fill: thencolor)[$(matchwith(beta, tycon(d, "_"), C))$]
    quad and quad
    #text(fill: elsecolor)[$gamma = "boolean"$]
    quad and quad
    #text(fill: condcolor)[$beta = gamma$]
$

#v1

== Difficulty: `let`-generalization

#v1

`let`-generalization in one simplified rule:

$
    #infer($Gamma der letin(x, t, u) : B$,
           $Gamma der t : A[overline(alpha)]$,
           $quad overline(alpha) in.not Gamma$,
           $quad Gamma, bind(x, forall overline(alpha). A[overline(alpha)]) der u : B$)
$

#alternatives[The still-undetermined inference variables of $t$
can be made polymorphic.][What if $t$ contains suspended constraints?]

#v1#pause#pause

#grid(columns: (1fr, 0.4fr, 1fr),
[
*Easy* case (j.w.w. Olivier Martinot):

no overlap between $overline(alpha)$ and $(matchwith(beta, sh, C))$

keep suspended
], [],
[ #pause
*Hard* case (this work):

$(matchwith(beta, sh, C))$ mentions some $overline(alpha)$

how to generalize?
]
)
#v1

== Solution

#v1

*Hard* case: $(matchwith(beta, sh, C))$ mentions some $overline(alpha)$

#v1

#grid(columns: (1fr, 1fr),
[ 
```ocaml
let myfun li =
  let add y = (Foo y) :: li in
  [...]
```
],[
Situation: ```ocaml Foo y``` is suspended on `li`; \
`y` may or may not be generalizable.
])

#v1#pause

Idea: continue checking `[...]` to infer `li`. But when `add` is used?

#v1#pause

Maybe we can just suspend uses of `add`?

#pause Nope: #h1 ```ocaml ((add [...]) : foo list) ``` #h1

#v1#pause

*Incremental* instantiation and generalization:
- start with a partial scheme: $forall alpha beta . quad alpha -> beta "list"$
- once we unsuspend `Foo`, update all instances of $alpha$

#v1#pause

#h1 This works even if $beta in overline(alpha)$: *back-propagation*.

#v1

== So it's easy!

#v1

Within each generalization region:
1. infer as much as you can, until the rest is suspended
   #h1 $==>$ partial type scheme
2. continue inferring outside
   #h1 $==>$ partial instances
3. whenever you make progress, update all instantiations
   #h1 $==>$ incremental instantiation
4. if a suspended constraint remains at the end,
   #h1 fail

#v1

Hard to implement correctly.
#pause
I failed, Alistair did it!

#v1

A prototype with an efficient-in-practice implementation:
#link("https://github.com/johnyob/mlsus/")

#v1

= Theory: \ #h1 unicity conditions

== Declarative typing rules

#v1

$
  Gamma der t : A
  quad<==quad
  Gamma derinf t : A
$

#v1

#grid(columns: 3, gutter: 1em,
[$Gamma der t : A$], [$quad$], [the full type system],
[$Gamma derinf t : A$], [], [a _declarative_ subset we know how to infer],
[$(exists A. thin Gamma derinf t : A)$], [], [decidable]
)

#v1

_declarative_ is better than _algorithmic_ for users.

#v1

Hard to achieve!

#v1

== Fragile features

#v1

Fragile features:
*explicit* (annotated) form $(app(K^d, overline(t)))$
vs. *implicit* form $(app(K^?, overline(t)))$.

#v1

Explicit:
#h1 $//
  #infer($Gamma derinf app(K^d, overline(t)) : tycon(d, overline(A))$,
    $Gamma derinf K : overline(B) -> app(overline(A), d)$,
    $Gamma derinf overline(t) : overline(B)$
  )
$ #h1
#pause

#v1

Implicit ?
#h1 $//
  #infer($Gamma der app(K^?, overline(t)) : tycon(d, overline(A))$,
    $Gamma der K : overline(B) -> app(overline(A), d)$,
    $Gamma der overline(t) : overline(B)$
  )
$
#h1

This rule is natural, declarative,
but it *guesses* the datatype constructor $d$. \
(Would require backtracking + breaks principality.)

#v1

Wanted: implicit inference rule.

#v1

== Solution: contextual rules + unicity

#v1

Implicit, contextual
#h1 $//
  #infer($Gamma derinf ctx[app(K^?, overline(t))] : C$,
    $unicityH(ctx, tycon(d, "_"), overline(t))$,
    $Gamma derinf ctx[app(K^d, overline(t))] : C$
  )
$
#h1

#v1

$unicityH(ctx, tycon(d, "_"), overline(t))$: a *unicity* condition \ The context $ctx[square]$ and the subterms $overline(t)$ _uniquely determine_ the shape of $hole$ to be $(tycon(d, "_"))$.

#v1

"Robustly known" := "uniquely determined".

#v1

== How to define the unicity condition?

#v1

#grid(columns: (2fr, 1fr),
[
Behold the _hole with subterms_ term-former $magic(square, overline(t))$. \ The $overline(B)$ can be anything.
], [#h1
$ #infer($Gamma derinf magic(square, overline(t)) : A$,
         $Gamma derinf overline(t) : overline(B)$) $
])

#v1#pause

#grid(columns: (2fr, 1fr),
[We can now _erase_ fragile constructs:],[#h1
$
  & erase(app(K^d, overline(t)))
  & quad := quad &
  app(K^d, erase(overline(t)))
  \
  & erase(app(K^?, overline(t)))
  & quad := quad &
  magic(square, erase(overline(t)))
$
])

#v1#pause

#grid(columns: (1fr, 1fr),
[And now the grand finale!],[#h1#pause
$
unicityH(ctx, tycon(d, "_"), overline(t))
\
:= \
forall thin Gamma, C, ground(A), \
  Gamma derinf
    erase(ctx [magic(square, overline(t)) : ground(A)]) : D
    quad==>quad
    tycon(d, "_") <= ground(A)
$
])

#v1

//= Conclusion

== Conclusion

#v1

*Omni-directional* type inferenence:

- suspended constraints
- incremental generalization and instantiation
- hard to implement
- hard to specify: unicity conditions

#v1

Prototype: #link("https://github.com/johnyob/mlsus/")

Draft: #link("https://www.ajo41.dev/papers/suspended-final.pdf")

#v1

#[
#set text(size: 30pt)
#h1 Thanks! #h1 Questions? #h1
]

#v1

// = References

// #bibliography("../suspended.bib", title: none, style: "./gasche-author-date.csl")
