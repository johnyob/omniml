

From the reviewers:

Referee: 1

Comments to the Author
# Summary

Principality is one fundamental property of type systems. However, principality
is also often fragile; many common language features can easily break it. This
paper refers to these features as _fragile constructs_. Traditional type
inference systems, such as bidirectional typing, often employ a static inference
order, which however causes certain programs to be rejected even when a unique,
valid typing exists.

This paper proposes _omidirectional type inference_, a framework characterized
by three key perspectives. First, based on constraint generation and solving,
the system allows constraints to be resolved in any order. Second, the system
supports _suspended constraints_, which are constraints that are suspended when
not enough type information is available. Third, the system also supports typing
let bindings in a dynamic order, which is non-trivial, as traditional let
polymorphism typically requires the binding to be fully generalized before the
body is typed.

The key inspiration of this system is that every fragile construct has a
corresponding robust form, where type information is explicitly provided as type
annotations. Following this idea, the system enables "known information" to
propagate from the rest of the program (rather than just immediate type
annotations) to a fragile construct, effectively elaborating fragile constructs
into their robust forms. However, because this type information may arrive from
remote contexts or only after other constraints are resolved, _suspended
constrains_ defer resolution, waiting for sufficient information to become
available.

To formally characterize "known information" within a declarative system, the
paper introduces the _unicity conditions_. A type is considered _known_ when it
is the unique type that can be inferred within the surrounding term context.
These unicity conditions are realized in three systems: (1) as a unique typing
condition in the declarative specification; (2) as a unique
constraint-satisfaction condition in the semantics of constraints; and (3) as
three syntactic rules in the constraint solver.

Supporting a dynamic order for typing let expressions introduces additional
complexity. To address that, the paper introduces _incremental instantiations_,
through a form of constraint sharing between different instantiations of the
same generalization constraint.

The paper demonstrates these concepts in a type system with two fragile features
found in OCaml: static overloading, where label names can be shared among
different nominal records, and polymorphic object methods, where methods within
an object have polymorphic types.

The paper then contributes: (1) a declarative characterization of "known type
information" based on the "unique" shape required to make a program type-check;
(2) a constraint language with constraint generation; (3) a rewriting-based
constraint solver; and (4) prototype implementations that incorporate efficient
implementation techniques.

# Assessments

Overall, I found this paper to be an engaging and insightful read. It makes a
compelling case for omnidirectional type inference, backed by a solid formal
foundation.

+ The core concept of "known type information" is both elegant and intuitive.
  While existing systems, such as QuickLook for first class polymorphism, rely
  on similar intuitions to maintain principality, this paper is the first to
  provide a precise, declarative characterization of the idea. The formalism of
  unicity conditions is particularly neat and well-conceived.

+ The system is remarkably thorough. The authors provide a complete package,
  including the declarative specification, a dedicated constraint language, and
  detailed processes for constraint generation and resolution.

+ The properties of soundness, completeness, and principality are formally
  proven. Upon quick review, the lemma statements and general proof structures appear
  robust and well-organized.

+ The paper provides prototype implementations that incorporates efficient
  techniques, demonstrating its potential for practical settings.


On the other hand, while the system is conceptually intuitive, its realization
is arguably complex. Part of this complexity seems to stem from the "Not too
hot, not too cold" limitation discussed in Section 2.5, specifically, that
unicity relies on the erasure of implicit constructs. That said, these
complexities are likely an unavoidable trade-off for this approach. In any case,
the system stands as a clear and significant advancement in the field.

A few suggestions:

- While the concept of _omnidirectional typing_ is clear in retrospect, I found
  the initial description as a 'dynamic order of inference' somewhat confusing.
  Since most constraint-based algorithms allow for flexible solving orders, this
  doesn't immediately make it clear what "omnidirectional" implies. In my view,
  the most defining aspects of your system are the suspension mechanism and
  incremental instantiation for let-generalization. Making these two pillars
  more explicit at the beginning would help clarify the unique nature of
  'omnidirectional' inference for the reader.

- The background section could be more streamlined:

  + The flow of Section 2.1. is somewhat confusing. The same set of examples is
  discussed twice: once assuming no defaulting rules, and again with them. It is
  quite confusing to see the same example (`ex1`) marked red and then orange
  (`getx`) for OCaml.

  Consider significantly shortening or removing the discussion of defaulting
  rules here. You could keep the footnote 2 to mention that while OCaml has a
  default resolution strategy, its behavior is unpredictable and out of scope
  for this paper. Keep the "no defaults, by default" discussion in Section 2.5.

  + Section 2.2, Semi-explicit first-class polymorphism

  I found the connection between the "annotation variables" in Figure 1 and the
  eventual formal system unclear. It seems your system does not need annotation
  variables? If so, the detailed discussion of Figure 1 and the rules from
  Garrigue and Remy (1999) feels unnecessary.

  Consider shortening it significantly and/or moving parts to Related Work. Be
  explicit that while these historical systems used annotation variables, your
  work does not.

  + Section 2.3, 𝜋-directional type inference

  This section also relies on annotation variables and feels redundant. I think
  the discussion on bidirectional type checking suffices. A brief mention of
  𝜋-directionality within the broader discussion of the "limitations of
  directional type inference" would likely suffice.

- Section 5.2 and 5.3 on incremental instantiation

  This is a complex part of the paper with mysterious notations that I took a
  while to understand, and would benefit from a running example.

  Showing an example of incremental instantiation in Section 5.2 and continuing
  its resolution in Section 5.3 would make the mechanism much more accessible.

- Section 6, Implementations

  It is unclear why two separate implementations are mentioned here, especially
  since the second isn't mentioned again until the very end (Section 7, page
  39). I suggest focusing on your primary implementation in Section 6 and refer
  to the second implementation as in Section 7.

- The paper would be strengthened by a discussion on the feasibility of
  integrating this technique into OCaml. Specifically, how might this system
  interact with existing OCaml features, and what unique engineering or
  theoretical challenges should be anticipated during such an integration?


# Detailed comments

Below are the notes I took while reading the paper. Most of these are minor
presentational issues, along with a few clarification questions.

- page 2, line 47, polymorphic parameters [White 2023].

You cite "Semi-explicit polymorphic parameters" as both [White 2013] and [White
2023]. Please make things consistent (e.g., citing the most recent or both
consistently).

- page 4, line 11, the example `ex2`

Why exactly is this rejected by OCaml? A brief clarification would be helpful.

- page 4, line 27-28, the example `let get x r = r.x`

This seems the identical as `ex1`. Why not simply refer back to `ex1`?

- page 4, line 36, the example `ex4`

What's the difference between `ex4` and `ex3`? Does this let expression change
the behavior compared to the tuple in `ex3`?

- The `(𝜎1 : 𝜎 : 𝜎2)` judgment remains somewhat mysterious. If you want to
retain the rules, consider providing a concrete example of this judgment.

- page 6, line 6-7: `𝜎1 = 𝜃(𝜂1 (𝜎))and 𝜎2 = 𝜃(𝜂2 (𝜎))`

Why is `𝜎1` the result of applying `𝜃` to `𝜎`, because type variables in `𝜎` are
considered existential variables?

- page 6, line 26-27, "A type ... is considered known if its annotation
  variables are eligible for generalization"

This sentence feels bit repetitive, as a similar point is made on page 5, line
34.

- page 6, line 27, "monomorphic annotation variables"

what is a monomorphic annotation variable? Maybe variables that cannot be
generalized?

- page 7, line 18, "on the left-hand side"

the left-hand side?

- page 12, fig 2, rule Annot

I was surprised to see the result type as `𝜏[𝛼 := 𝜏]`, rather than `∃𝛼.𝜏`. Is
this specific to how OCaml handles existential refinement?

- page 13, line 14, "in §1" -> "in §2"

- page 15, rule Inst-Shape

You mentioned on line 14 that "By construction, we require 𝛾 to be exactly the
free variables of 𝜏." If that's the case, the free variables of `𝜈𝛾1.𝜏` will be
empty, so why is that premise `𝛾2 #𝜈 𝛾1.𝜏` needed?

- page 15, line 25-26, Definition 3.1

It seems you only reason about canonical principal shapes. Would it simplify the
presentation to introduce canonical shapes directly?

- page 15, line 37-39, `𝜈𝛾.[∀𝛼.([∀𝛽.(𝛽 →𝛾)∗ 𝛽])→𝛼 →𝛼].`

It was not clear to me until this example that the holes (`𝛾`) cannot refer to
bound variables (e.g. `𝛼` and `𝛽`). I think this is a crucial detail worth
stating explicitly.

- page 16, Fig 3, line 5, `ℰ ::= □|ℰ 𝑒 |𝑒ℰ |...`

Can `ℰ` appear within lambdas? Example 3.3 suggests yes, but the definition in
Appendix A suggests no.

- page 16, Fig 3, rule Proj-I

This rule appears before you have introduced tuples (in footnote 9).

- page 16, Fig 3, `ℰ [𝑒 ⊳ 𝜍 | 𝑒s] `

I'm confused about why you require the list of `𝑒s`, as it does not seem to be
used in the rules.

- page 17, line 21, the omnidirectional recipe

The distinction between introduction and elimination forms here feels
reminiscent of bidirectional typing.

- page 17, line 49, `𝜆𝑟.(𝑟.x,𝑟.point.y)` from §2.1.

The example differs slightly from the version in §2.1, which was: `𝜆𝑟.(𝑟.x,𝑟:point.y)`

- page 23, line 6-7, `𝜌 ::= ... | rcd 𝑡 _ | ...`

It was initially confusing why the field types of the record are omitted. A
brief explanation of why these are not required for this part of the formalism
would be useful.

- page 24, "a instance" -> "an instance"

- page 27, line 43, "𝛼 ≺+𝑈 𝛼"

Could you clarify the notation `≺+𝑈`? Additionally, how does it rule out "𝛼 =
𝛼"?

- page 29, line 35, "However, this is incomplete"

Incomplete with respect to? It's true that it would reject `ex8`.

- page 31, line 35, "a nullary shape"

This is the first mention of a "nullary shape." I assume this refers to a unit type.

- page 31, line 36-37, "The copied variable 𝛽"

The copied variable in rule S-Inst-Poly is `𝛼'`

- page 36, line 21, "implement unification (of) dependently-typed systems"

- page 36, line 49-50, "Furthermore, OutsideIn forgoes a declarative specification"

"OutsideIn" should be OutsideIn(X) when referring to [Vytiniotis, Jones,
Schrijvers and Sulzmann 2011]

- page 60, line 23-24, "This simplifies the proof, but introduces a circular
dependency between Theorem B.5 and Lemma B.6. "

Theorem B.5 does not seem to use Lemma B.6 (or Corollary B.7).

- page 62, line 39-40, "𝜙 ⊢𝒞2 [match 𝜏 := 𝜍 with¯ 𝜒,𝐶2] By i.h."

Should it be `𝜙 ⊢𝒞2 [𝐶2] By i.h.`?

- page 66, Lemma C.11, "If 𝒞 is normalized"

What is a normalized constraint? that no further rewriting rule applies?


Referee: 2

Comments to the Author
# Summary

ML-style type inference is great -- it has principal types! But
advanced features such as static overloading and semi-explicit
first-class polymorphism breaks principality of ML-style type
inference. Previous work recovers principality by requiring explicit
annotations. But too many annotations are required.

This paper proposes OmniML, a novel type inference approach that
extends ML-style type inference with better support (i.e., requiring
less user-annotations) for advanced features such as static
overloading and semi-explicit first-class polymorphism. In
particular, the paper gives a declarative specification of OmniML
using a novel notion of *unicity conditions*, gives a constraint
language for describing the type inference with a novel notion of
*suspended match constraints*, and gives a constraint solving algorithm
with novel techniques such as *partial type schemes*. Metatheories for
the algorithm including soundness, completeness, and principal types
are proved. Implementations are also provided.


# Comments

This is a strong paper that proposes several novel concepts as
listed above. The paper is well-written and easy to follow. The
motivation for having a dynamic order of information flow is clearly
explained. The resulting system is very powerful and even supports back
propagation. In general, I enjoyed reading this paper and I really
like the ideas of unicity conditions and suspended match constraints.
I believe these ideas are influential and can be generalised to other
type inference problems.

On the other hand, I have some technical comments and questions about
the paper which I will list below. I recommend the paper for
acceptance with minor revisions to address my concerns (or correct me
if I'm wrong).


## Technical Comments

### Differences between omnidirectional type inference and bidirectional type inference

Perhaps I was just mislead by the word "omnidirectional". When I
started to read this paper, I expected to see something that extends
bidirectional type inference to allow more powerful type information
flow, so that I can use this approach to improve previous
bidirectional type systems. However, it turns out that OmniML is
actually more like an extension to global type inference (such as
ML-style type inference), and it does not subsume bidirectional
typing.

My concern is that the paper focuses on semi-explicit first-class
polymorphism which requires polytypes to be boxed and requires
explicit syntactic annotations for generalisation `[e]` and
instantiation `<e>`. However, bidirectional type systems for
higher-rank/first-class polymorphism (such as Dunfield and
Krishnaswami's ICFP 2013 paper, QuickLook, and Frost) usually directly
support the syntax of higher-rank polymorphic types and do not require
extra syntax for generalisation and instantiation. This is a substantial
difference, but the paper does not explain it clearly. The only
discussion about this I can find is on page 37 line 17, where the
authors simply say "(higher-rank types) can be encoded back into
(boxed) polytypes". I don't think the existence of such an encoding is
a good justification for the ergonomics of semi-explicit first-class
polymorphism. Writing explicit syntax for generalisation and
instantiation (even though it is just a pair of brackets) can still be
verbose. One main point of many type inference systems for
first-class polymorphism (including QuickLook and Frost, and those not
mentioned in the paper such as Serrano et al.'s GI and Daan Leijen's
HMF) is to avoid such verbosity by supporting implicit generalisation and
instantiation triggered by bidirectional information flow or other
heuristics.

I spent some time trying to extend OmniML to support the kind of
``implicit'' first-class polymorphism of bidirectional type systems
such as QuickLook, but I failed. The main challenge is that in OmniML,
uses of unicity conditions are triggered by certain syntax such as
`[e]` and `<e>`. If we were to support implicit type abstraction
everywhere, we would need to insert unicity conditions everywhere,
which I don't know how to make it work. Is such an extension possible?

I'm not against the design choice of semi-explicit first-class
polymorphism -- I think it makes sense particularly given that this is
what OCaml supports. I hope the paper can clarify on the limitations
of OmniML compared to bidirectional type inference:

1. The dynamic order of OmniML is not strictly better than the static
   order of bidirectional type inference. OmniML cannot subsume many
   bidirectional type systems. OmniML focuses on improving global type
   inference.
2. The restriction to semi-explicit first-class polymorphism is
   needed. Type inference for semi-explicit first-class polymorphism
   is an easier problem than type inference for first-class
   polymorphism that does not require boxes for polytypes and
   explicit syntax for generalisation and instantiation. Bidirectional
   type systems support the latter.

### Similarities between omnidirectional type inference and bidirectional type inference

Though I have the above complaints, I do see some similarities between
OmniML and bidirectional type inference. My favourite two similarities are:

1. Bidirectional typing has two modes: checking and inference.
   Contextual typing (Xue and Oliveira 2024) extends modes to count
   arguments. The intuitive idea is that modes track information about
   the surrounding context. I think this is exactly the idea of the
   one-hole term context in the contextual typing rules in Section
   3.4. The one-hole term contexts can be understood as the most
   precise version of modes.
2. OmniML has the most precise version of modes, but by default does
   not use information from it, different from bidirectional typing
   which always either checks or infers. Instead, OmniML has two
   unicity conditions for using information from modes in different
   directions: the first condition asks the mode to determine the
   shape of a term (like inference), and the second condition asks the
   mode to determine the shape of a hole (like checking).

### Semi-explicit first-class polymorphism with bidirectional typing

On page 7 line 24, the paper introduces how to switch from
the π-directional type inference of the system in Garrigue and
Rémy 1999 to bidirectional type inference. I think this hypothetical
bidirectional type system is wrong because it lacks the unicity
conditions. For instance, if we want to infer a type for the program
`\lambda x . <x>`, can we guess a type `[σ]` for `x`? I think the
answer is yes, because `[σ]` is a monomorphic type. Then we lose
principal types because `<x>` can have any polymorphic type `σ`.
OmniML is fine, because `<x>` is rejected due to the unicity
conditions.

On page 8 line 48, the paper says that "under a bidirectional approach
(using Syn-App), only ex62 is rejected." It's unclear why this is true.
For ex62 why cannot I just guess the correct instantiation for `app`?
What is the typing rule for instantiation in this hypothetical
bidirectional type system?

I understand that the point here is to demonstrate the restrictions of
static order, but including a problematic or underspecified
hypothetical type system may not be a good idea.

### Principal Shapes

On page 15 line 38, why the principal shape is not `ν γ . [∀a . γ -> a -> a]`?


### Terminology of "polytypes"

It's a bit confusing to me that the paper uses the term "polytypes" to
refer to polymorphic types with boxes. Polytypes to me are just
abbreviations for polymorphic types. I understand that this is also
the terminology used in Garrigue and Rémy 1999. I would appreciate if
the paper can explicitly use something like ``boxed polytypes''. Feel
free to ignore this comment if you'd like to keep "polytypes".


### Related work

1. Page 8 line 9: "so some well-typed programs are rejected as ill-typed (e.g. ex62, ex63)."

It's unclear to me what this sentence means. Because contextual typing
does not have polymorphism. In their POPL26 paper on local contextual
type inference which supports polymorphism, I think ex62 fails but
ex63 succeeds, because their approach is order sensitive.

2. Page 8 line 11 and page 37 line 19

It's probably worth mentioning that QuickLook and Frost focuses on
implicit first-class polymorphism instead of semi-explicit first-class
polymorphism, which is a more challenging problem.


## Minor Comments and Typos

* Page 5 line 46: considered of => considered as
* Page 11 line 30: `r.x` => `r.color`
* Page 12 line 45: The sentence is difficult to parse
* Page 15 Theorem 3.2: I think a principal shape is already non-trivial, why repeating here?
* Page 16 Figure 4: why not showing the erasure of `[e:∃a.σ]`?
* Page 18 line 35: ``it is a given that''
* Page 21 line 30: In the rule Match-Nat, I think there should be `Φ|-` before the first premise
* Page 21 line 40: I think some steps are omitted. It would be helpful to show the full derivation
* Page 21 line 40: This is a good example but boring, because there
  is a unique solution for `α`. It might be helpful to show another
  more interesting example without a unique solution.
* Page 24 line 37: I think you wanted to say "every instance of s is an instance of x"
* Page 35 line 21: ``unification dependently-typed systems''


## Non-technical Comments

In section 2, I think the colours you use for success and warnings
can be challenging to distinguish for colour-blind readers. I would
suggest using colour-blind safe colours.

Referee: 3

Comments to the Author
Summary: This submission develops a type inference scheme for
first-class polymorphism and records, which are typical causes of
breaking the principal type property.  For example, given a record
projection x.l, if l belongs to multiple named record types, the "most
general" type of x can't be decided, at least locally.  A key idea to
overcome this difficulty is to use contextual information, which may
have sufficient information to determine the "shape" (the name of a
record type) of the type of x.  It amounts to a type inference
algorithm that waits for the unique shape of x to be determined,
rather than guessing something, as unification proceeds.  (If the shape
is not determined uniquely, the inference simply fails.) The proposed
type inference scheme is called "omnidirectional" because the order
in which the types of subexpressions are decided is not fixed only by
term constructors, as opposed to bidirectional typing.  The authors
give a formal definition of the unicity condition ("a context uniquely
determines the shape of the type of a given (sub)expression"), which
is critical in discussing the correctness of the omnidirectional type
inference.

Main technical contributions are:

* A formal definition of the OmniML calculus, a declarative type
  system, which is stated to have a principal type property.

  There is no discussion about soundness with respect to an ordinary
  type system but I think it's straightforward.

* A formal language of constraints with its semantics.  The language
  contains a construct for pattern-matching a type against a type
  shape.  The semantics also takes into account the context-dependency
  of the matching construct.

* A constraint generation algorithm which is sound and complete w.r.t.
  the OmniML type system.

* A constraint-solving algorithm and its correctness theorem.

* The system has been implemented and made public (although I didn't try).


Evaluation:  Accept after minor revision.

First of all, the contributions made in this work are novel and
significant enough to warrant publication at a highly esteemed venue
like TOPLAS.  The manuscript formalizes and proves what is expected of
work on type inference, namely, a declarative type system, constraint
language, constraint generation, constraint solving algorithm, and
correctness theorems.  It also gives a gentle introduction to the idea
with reviews and comparisons of existing work.

In my opinion, the main idea of "waiting for the shape to be
determined until the context resolves" is something people could think
of, but it IS impressive that the authors have realized it and proved
a principal type property, whose statement is not so tricky!  The
unicity condition is very interesting -- it is rather subtle, but it
captures the intuition correctly.

The manuscript is overall well-written, but there are a few places
that are hard to understand.  In particular, regional let-constraints, partial
type schemes, and incremental instantiation (found on pp.28-30) are
hard to understand.  I'd suggest the authors improve the presentation.

Also, the second implementation, which is (somewhat strangely)
discussed in the related work section, is not very clear, either.

What I'd miss is a comparison with bidirectional typing from the
viewpoint of (typing) errors.  One of the often claimed benefits of
bidirectional typing is its predictability and locality, which may
lead to a better experience of what is wrong in an (ill-typed)
program.  I'm wondering if the authors could give some informal
experience report.

Finally, I could only skim proofs and not investigate them in detail.
The theory is rather complicated (by necessity), and its correctness is
far from trivial, but the main text provides only the main theorems.
Perhaps, it is a good idea to discuss key lemmas in the main text.


Minor comments:

p.4, L18 and L26: There are two references to footnote 2.  Is this
intended?

p.6, L32, "For OCaml, ...":  It wasn't very clear what you meant.

p.7, L18, "on the left-hand side": "below"?

p.9, L46: What is a "ground record name"?  (Are there non-ground record names?)

p.10: The last two paragraphs of Sec 2.4 are not very clear.  In
particular, it's not clear how the heading "The forest, not the trees."
and the contents match.

p.12, L50:  A relevant citation for modular implicits is desirable.

p.13, L24: "possible empty" -> "possibly empty"

p.15:  Isn't the premise of Inst-Shape always satisfied?  The set of
free variables of the right operand of # seems to be empty, because
you "require \gamma to be exactly the free variables of \tau."
Maybe "\nu \gamma_1" should be removed?

Fig. 3: Proj-I is for tuple projection but tuples are not discussed, so
far.

p.17, LL13-20: I had this very question immedietely after looking at
the definition of the unicity condition and had a hard time reading the
paragraphs before this one.  I wonder if it's a good idea to
distinguish two typing relations, one defined by the rules in Fig. 2
and Hole, and the other for the full language.  Then, the unicity
condition (for expressions without fragile constructs) can be
explained and defined before presenting Fig. 3.

Example 3.3:  In Section 2.1, ex_3 was defined by using (r : point).y.
Are r.point.y and (r : point).y equilavent?

p.25, L18: "can [be?] the required type information"

p.27, Def. 5.1: Later you use \hat C and \hat{\mathcal{C}} but they
don't appear to be defined.

p.28: The discussion about regional let-constraints is hard to
understand.  The semantics of a regional let is defined in Fig. 9 and
explained from L33.  The use of \phi confuses me.  When you say "A
ground region r is a pair (g, \phi)", is the \phi the same as the \phi
in \phi(\lambda \alpha[\alpha].C)?  Maybe, some explanation about its
intuition is helpful.

p.29, L39:  What does \bar \epsilon stand for?

pp.29-30: Again, the discussion about partial type schemes and
incremental instantiation is hard to understand.  I'd like to see a concrete example of constraint solving that involves incremental
instantiation constraints.

p.32, rule S-Inst-Copy:  What does i^x(\bar \beta) ~> \bar \beta' stand for?
Is it a sequence of i^x(beta1) ~> beta1, ..., i^x(beta_n) ~> beta_n combined by conjunction?

p.38-39: The discussion on semi-unification is not very informative,
although it sounds interesting.  I don't quite see the relationship to
the omni-directional inference or a second implementation.  I think
more elaboration is needed.

