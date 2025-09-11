We are grateful to our reviewers for their interest in our work and their copious, high-quality feedback. Thanks!

We are particularly grateful for the very detailed criticisms in review B.  We agree with some of the criticism and have proposals for addressing it.  Some comments suggest that we did not get some important points across as clearly as we should have, and they also inform our plan to clarify the paper.

Our response is too long, but it discusses the important points first that we hope may be of interest to all reviewers, and you can stop at any point. In the following:

1. We do the exercise of shortly restating the key points of our paper.

2. We discuss the feature space of ML-family-and-neighbors type inference topics related to overloading, guided by the helpful reference list of reviewer B, in relation to the focus of this work clarified in (0).

3. We propose a workable plan to revise the paper to make it more focused and more self-contained, and to rewrite the introduction so that *future readers* don't need to ask your important questions.

4. We convince reviewer B that providing declarative semantics for suspended constraints is in fact the opposite of "rather straightforward": a substantial scientific contribution.

5. Finally we discuss local comments of each review in order, so that our scientific exchange is as satisfactory as possible if you have the curiosity to read them.

## 1. The key points.

This work started from the question of how to improve OCaml's support for type-based disambiguation, which currently uses a mix of pi-directional and bi-directional type inference, and is found to be lacking by users. The OCaml features we consider are using a form of static overloading (but semi-explicit first-class polymoprhism is not really overloading), and it would not be acceptable to consider using dynamic overloading as it would change the dynamic semantics (and thus observable side-effects), program efficiency, etc.

The natural idea of delaying type-disambiguated feature "until the type is known" is in fact very difficult to combine with local let-polymorphism, which is a key feature of Hindley-Damas-Milner type inference and not something OCaml would consider giving up.

Our work explains how to delay type-inference decisions "until the type we need is known" in presence of local let-polymorphism, which is difficult to (1) implement and to (2) provide precise declarative semantics for. We solve these two significant problems. We believe this solution is of interest to many other HM-based languages.

## 2. The research space

A common and elegant approach to perform type inference with overloading is to abstract over the still-undetermined overloading points at generalization time. This approach is exemplified by qualified types: a remaining constraint "Eq a" when inferring "let f = ..." will lead to the type scheme "f : Eq a => ...". This is simple in theory and easy to implement, but it crucially introduces *dynamic* overloading: each user/caller of `f` can provide their own instance of `Eq`. This approach cannot be used for *static* overloading, which imposes that a single global solution is selected within the definition of `f`.

Among the works cited in review B, [Kaes, 1992] and [Smith, 1994] and [Pottier, 2000] all infer constrained type schemes in this way, so they correspond to a form of dynamic overloading rather than static overloading. (Dynamic overloading requires either passing extra values at runtime or user a richer runtime where values carry an extra header that can be examined at runtime.)

If we consider static overloading, but work in absence of local let-generalization, then delayed constraints can simply bubble up to each toplevel declaration (since unification constraints can be solved in any order), and there they can be resolved arbitrarily (or fail to force the user to disambiguate). Delaying constraints in this way is commonly done in dependent-type systems.

Among the works cited in review B, this is the approach of [Beneš and Brachthaüser, 2025]. Their work discusses the interesting problem of solving variational-type constraints at the toplevel, but it does not support local let-generalization. The details of how [Chen and Erwing, 2016] deal with local let-generalization are less clear -- our current understanding is that their "reconciliation" process will fail with an error if not enough information is known at generalization point, even if a solution might have been found by delaying the problem until later, so we would also qualify it as not interacting so well with let-polymorphism.

The work of [Pottier, 2000] on conditional constraints deserves a specific mention: as review B points out, it has similarities: it describes a general mechanism to "delay" the resolution of some constraints, and is demonstrated to cover several different language features in interesting ways. Besides the fact that conditional constraints are turned into constrained type scheme at generalization, one key difference is that a conditional constraint (in Pottier's work) that never fires is considered a *success*, whereas a suspended constraint (in our work) that never gets discharged is considered a *failure* (type-inference fails if a construction cannot be disambiguated). This is a big change to the semantics, and it means that the constraint-level feature will be applied to very different user-facing type-system features -- it does not solve our problem.

Our failure semantics may a-priori mean that a complete solver should exhaustively try all possibilities to discharge a suspended constraint. [Beneš and Brachthaüser, 2025] go impressively far in this direction: they avoid backtracking and share a lot of common work between the different possibilities, but still explore the whole space, at the cost of a worst-case blow up.  We favor implementations that fail if the solution is not propagated by unification, then solving constraints locally but just looking at the toplevel shape of one constraint at a time. However, capturing this notion of being "known" rather than "guessed by luck or backtracking" at the level of the semantics is very difficult, and this is the key contribution of our unicity conditions.

Finally, we certainly agree that MLF is a promising way to handle polymorphism: it does better than semi-explicit polymorphism as in OCaml, but to our knowledge no one knows how to scale MLF to the full set of feature that OCaml contains so adopting MLF there is not an option for now. But MLF does not deal with type-based disambiguation or in general static overloading, so it does not answer the research problem of our paper.

## 3. Revision plan

A common theme across reviews is that the paper is difficult to read:
definitions are scattered, essential material is deferred to
appendices, and the line of contributions is obscured by staging and
breadth. We acknowledge these issues, which we believe comes from bad
decisions we made to fit the page limit, too close to the deadline for
comfort. Below we outline concrete changes we will make in a revision.

### Contributions

We will shorten and rewrite the introduction (~2 pages) to adequately highlight
the novelty of omnidirectionality *with* local let-generalization.

We will state our main contributions earlier (and more directly):

1. A novel characterization of 'known' type information that does *not* rely on
   a *static* ordering of solving inference constraints (unlike bidirectional
   or pi-directional approaches), but a *dynamic* ordering.

   We will illustrate that this is more flexible by typing more OCaml programs
   that would otherwise be ill-typed under the current approach (the
   combination of pi-directionality and bidirectional).

2. The introduction of partial type schemes: the device that permits our
   approach to scale to ML in a modular fashion. In particular, they handle the
   interaction between suspended constraints and let-generalization.

We will emphasize that preserving local let-generalization with suspended
constraints is mandatory in the setting of OCaml, and a key scientific
contribution of our work.

Prior work (helpfully highlighted in review B) typically only deals
with top-level let-generalization. Local let bindings are treated
monomorphically, and therefore do not address this troubling
interaction.

### Related work

We will expand and restructure related work into three focused parts:

1. Works involving 'delayed' or 'suspended' constraints. We will discuss
   Pottier's conditional constraints (B), OutsideIn(X) (in more detail), and
   dependent type systems.

2. Works involving overloading in ML. We will discuss qualified types (and why
   they're not suitable for our setting) and choice types (and why existing
   work in this direction does not handle local-let generalization).

3. Polymorphism. We will relate polytypes to MLF and recent bidirectional
   accounts (e.g. DK, Haskell's Quic).

        [XXX: What is Haskell's "Quic"? Do you mean Quicklook?]

### Removing content

We want to emphasize that suspended constraints are a general approach
to handle several language features. But it was too ambitious to try
to cover (1) tuples, (2) records, and (3) semi-explicit
polymorphism (and some other forms of ad hoc static overloading, which we also
explored but intendedly omitted).  We will remove tuples to save space --
they are a didactic example, but the least convincing of the three.

### Consolidation of technical material

Reviews B and C found the paper insufficiently self-contained, with
definitions introduced haphazardly and many details relegated to appendices.

We will make the sections presenting technical contributions more
self-contained and easier to navigate:

- Section 3 (Constraints). A single, consolidated presentation of the
  constraint language: syntax and semantics of constraints, shapes, and patterns.

- Section 4 (The OmniML calculus). Definition of OmniML with complete syntax,
  representative typing rules (records and polytypes), and the formal constraint
  generation function, followed by the metatheory.

  This ordering addresses the concerns about the missing typing rules and
  constraint generation (B, C).

- Relocate examples. Move several of the more involved constraint generation
  examples (currently in Section 3) into the consolidated account in Section 4.

Figures in each section will be consolidated, acting as notational cheat sheets
within the main body of the paper. We cannot bring the entire technical reference
(in the appendix) into the paper, but we hope this addresses review B and C's
comments regarding difficulty reading technical sections.

### Space

We fund the added self-contained material by (i) removing tuple overloading from
the main text (~1-2 pages), (ii) pruning conclusion/future work (~1 page),
(iii) consolidating split definitions into single figures (net-neutral), and
(iv) removing duplicated exposition by consolidating sections (net-neutral).

### Overview: Revised paper sections

Below is the revised paper sections:

1. Introduction

   1.1. Contributions

2. Overview

   2.1. Static overloading of constructors and record labels

   2.2. Polymorphic methods
  
    - Semi-explicit first-class polymorphism

   2.3 Directional type inference
   
    - $\pi$-directional type inference
    - Bidirectional type inference
    - Limitations of directional inference

   2.4. Omnidirectional type inference
  
    - Suspended constraints
      - Two mini examples of constraint generation for records and polytypes
    - Scaling to ML
      - Back-propagation and the requirement of local let-generalization

3. Constraints

   - *Cheat sheet: syntax and semantics*
   - Syntax and semantics outlined.

   3.1. Shapes and patterns

   3.2. Suspended constraints
 
     - Examples

4. The OmniML calculus

   - *Cheat sheet: syntax*
   - Syntax outlined

   4.1. Typing rules

     - *Cheat sheet: typing rules and unicity definition*
     - Examples

   4.2. Constraint generation
     
     - Examples

   4.3. Metatheory

5. Solving constraints

   - *Cheat sheet (constraints): new syntax and semantics. e.g. unification problems, regional let constraints, partial instantiations*

   5.1. Unification

     - Re-frame unification as an abstract rewriting relation that
     satisfies termination, preservation, progress. This parameterizes
     our solver by an arbitrary equational system.

     - Definition of solved form

   5.2. Solving rules

    - *Cheat sheet: rewriting rules for constraint solving and associated definitions (e.g. C determines 'bs)*

    - Basic rules
    - Let constraints
    - Suspended match constraints
    - Back-propagation

   5.3. Metatheory

6. Implementation

   - Remains unchanged

7. Related work

   - Suspended constraints
     - OutsideIn and OutsideIn(X)
     - Pottier's conditional constraints
     - Dependent type systems
   - Polymorphism
     - MLF
     - Bidirectional approaches (DK, Quic)
   - Overloading
     - Qualified types
     - Choice types

8. Conclusion

   - Future work


### Timeline

In this proposal the introduction is entirely new, and the related
work section is significantly rewritten, but for the other parts of
the paper this is mostly a matter of moving content around. We are
confident that we can achieve this within the required time frame --
we already have a draft of the new introduction.


## 4. The difficulty of finding good declarative semantics

> 2. **Lack of comparison with alternatives.** _The purported advantage
>    of the presented system over existing work, such as OutsideIn(X),
>    is its support for fine-grained partial types schemes; however,_
>
>     - _This seems to be more of an engineering problem without a very
>       deep theoretical underpinning – sure, the semantics for
>       suspended match constraints is not as obvious as one would
>       think, but it is still rather straightforward._

We firmly disagree. It was a lot of work to come up with satisfying declarative semantics for suspended constraints, we went through many iterations which ended up not working, and had to work through the detailed proofs to find a suitable definition. We are convinced that coming up with a declarative semantics is a significant contribution --- the main reason why our work was not published a few years before, as semantics were the missing piece.

To be more objective and less anecdotal about this: we see a connection between our difficulties finding a declarative semantics and what happened in the OutsideIn(X) work. In the conference version of the OutsideIn paper there is a declarative semantics, which is rather subtle and elegant. In the journal version of the work, the declarative semantics is gone, because the authors failed to generalize it to other surface-language features than GADTs. (Note: our paper as submitted claims that OutsideIn does not come with a declarative semantics, because we had only looked at the journal version at first. We re-read the conference version after submission, realized our mistake, and fixed it in our current draft.)

- conference version: https://www.microsoft.com/en-us/research/wp-content/uploads/2009/09/implication_constraints.pdf
- journal version: https://simon.peytonjones.org/assets/pdfs/outsideinx.pdf

Let us quote the journal version (section 6.5, pages 50-51).

> _To sum up, our specification accepts some “bad” programs (ones that are ambiguous, or lack a principal type), and the OutsideIn(X) algorithm rejects some “good”ones.
> The latter is no great surprise. For example, the Hindley-Milner algorithm accepts only λ-abstractions whose binder has a monotype. We accept that a tractable algorithm cannot work magic, so instead we tighten the specification so that it matches what the algorithm can achieve.
> The obvious way to restore completeness is is to tighten up the specification, so that it rejects both (a) bad programs and (b) programs that the inference algorithm cannot type. The trouble is that the cure is worse than the disease: the specification becomes as complicated and hard to understand as the algorithm.
> For one such attempt the reader is encouraged to read our earlier version of the OutsideIn(X) algorithm, which had a fairly complicated specification, and one that worked only for the special case of GADTs in (Schrijvers et al., 2009), and neglected ambiguity entirely. For the general case of arbitrary constraint domains and local constraints, we are not optimistic about this approach._

We believe that our declarative semantics is at least of comparable difficulty to the declarative semantics that the OutsideIn(X) authors were looking for and failed to be able to formulate in a general enough way. Here is an explanation for our intuition: in the OutsideIn work, each GADT match is type-checked independently with no flow of information from inside to outside, so in particular there is no communication between GADT matches that are not syntactically nested. In contrast, our suspended constraints can exchange information, discharging one can disambiguate another and lead to its discharge; our declarative semantics has to support this "causality" between syntactically-separate constraints (without allowing self-justification cycles, which has plagued several of our attempts).

Simplifying their specification was also a major reason why the JFP paper removes local let-generalization (end of page 32):

> _In summary, generalisation of local let bindings (without annotations) is a device that is almost never used, and its abolition yields a dramatic simplificationin both the specification and implementation of a typechecker._

Note that we are not covering the same language features and design issues as the OutsideIn(X) work (we do not consider type families, for example), so we are not claiming to subsume their work, but we claim that the difficulty (the non-straightforwardness) is at least comparable.

## 5. Local comments

### Review A

>  _A general concern I have is that usually you first define the
>  declarative type system and then constraint generation.
>  Maybe this would be a better presentation, or at least you could
>  discuss the choice?_

We hesitated as well. The opinion  that the formulation of uniqueness in constraints seemed more general and more regular, so hopefully easier to understand, than the uniqueness conditions in the type systems which are slightly different for each feature. 

Also, it may be the case that some type-inference authors will find constraints easier to relate to their own work, rather than the particular surface-syntax type system we present. (It sometimes help to think of constraints first.)

This being said, we are still torn on this question. The proposed revision plan still puts constraints first before the type system, but we might swap them around depending on how the writing goes.

> _Fig.1 and its explanation: could you specify which constraint forms
> are novelties of your approach, and which are introduced in [Pottier
> and Remy 2005]? Certainly suspended match constraints are a novelty,
> but, e.g., constraint abstraction and application?_

Constraint abstraction and application are not new, mostly a syntactic reformulation that also appears in previous works, for example [Pottier, 2014]. (Technically there is a difference between solved constraint-abstractions and type schemes, but it is mostly a superficial/cosmetic difference.)
We will definitely clarify this in the presentation.

[Pottier, 2014] Hindley-Milner Elaboration in Applicative Style
https://pauillac.inria.fr/~fpottier/publis/fpottier-elaboration.pdf


> _451: the set of types associated to term variables is infinite, right?
> I see at line 498 that membership test is done by substitution, do you
> not need then to have a finite representation of the environment?_

Just to make sure there is no misunderstanding, we are defining declarative semantics here, not a decision procedure / a judgment that would be implemented. (Our solver corresponds to a program, and it is shown to capture satisfiability in the semantics.)  Line 498 mentions a logical equivalence between membership and satisfaction in a substituted context, but none of the two sides are computed.


### Review B

Let us mention again that we are grateful to review B for its detailed and well-structured exposition of its criticism. We believe that we have implicitly addressed each point of the "few concerns" list in the main body of our response. (We did not discuss the related works in detail, but this will go in the revised paper.) This sub-section discusses the more minor/local comments.


>    - _There is no quantitative and little qualitative evaluation to
>       indicate that the presented system indeed performs better than
>       the existing ones. The authors write, in the context of
>       OutsideIn(X), “we believe that we have solved the troubling
>       interactions between let-generalization and suspended
>       constraints in this work”, but this belief is never
>       substantiated. However, it is a at the core of deciding whether
>       the paper makes a significant contribution or not!_

We apologize for the too-careful wording. Let us *claim* that we have solved the interaction between suspended constraints and let-generalization (via partial type schemes), both in theory and in practice: this is the very point of our submission indeed.

>    _Now, one could imagine implicitly boxing every record field and
>     every argument and implicitly unboxing all parameters and field
>     selections. This could allow users to explicitly annotate record
>     fields and parameters with polymorphic types, while leaving their
>     provider and consumer expressions handle that polymorphism
>     implicitly._

This is precisely the elaboration of polymorphic methods into semi-explicit first-class polymorphic primitives, as used by OCaml, including in the type-checker implementation. For *nominal* record fields the situation is a bit simpler, as OCaml knows (after type-based disambiguation) which record fields are polymorphic, and at which type. Object types are structural, so polymorphic methods need the approach you mention.


>  _What is the worst-case complexity of your approach? You might want to
>  consider a simplified setting with no let-generalization to make this
>  analysis more informative. With let-generalization, does your approach
>  make the complexity of constraint solving significantly worse? What
>  makes you think it is still practical in usual user-written programs?_

With no let-generalization, solving constraints becomes plain unification, so there is no extra cost to suspended constraints. (Implementation-wise: it's possible to add a check on each variable-structure unification, and discharge any constraint suspended on this variable). Note: suspended constraints without generalization are folklore in other type-inference systems (eg. dependent type-checkers) and not something we claim is novel.

The interesting setting is in presence of _local_ let-generalization. ML inference is doubly-exponential, but let us assume that let-nesting depth is bounded. A key point is that we never backtrack nor duplicate the body of suspended constraints.  When a polymorphic region makes progress (discharges a suspended constraint), our partial instantiation machinery implies some extra work for each instantiation of the partial polymorphic scheme.  We cannot make precise complexity claims at this point, but we have tried our best to avoid unnecessary repeated work and use appropriate data structures for that purpose.

> _What are the error messages like, in your new framework? OCaml’s type
> error messages are notoriously bad. Could they now get even worse? Or
> do you have a specific plan to handle this problem?_

Not yet, unfortunately. (We would live in a better world indeed if each type-system publication was conditioned on a solid plan for error messages :-)

Optimistically, it may be the case that our system may make the system feel more pleasant to end-user, thanks to our support for more propagation of type-information: OCaml errors related to type-directed disambiguation come when the program is not annotated enough, but sometimes users are irritated because the type-checker obviously _should see_ that a unique type is possible here. 

In any case, we could point the user to a precise program location where an implicit construct is missing some known (shape) information, and it should be obvious for the user to tell at this location what the shape should be. Hence, we do not think that suspended constraints should worsen the understanding of error messages.


### Review C

> _There is a claim about the description of an *efficient*
> implementation. However, despite a few explanations in section 6 about
> efficiency-driven decisions, the efficiency of the implementation is
> not validated at all; there are no benchmarks comparing with other
> approaches and/or showing the benefits of said decisions. I'd suggest
> to tone down that efficiency claim._

It is difficult to benchmark new type-systems, as there are no large, representative user programs to measure, and writing large synthetic terms is hardly representative. (One could also think of translating existing ML programs to our system, but few only cover a few representative features and practical programs are outside this subset.)

    [XXX: I don't understand "few only cover a few representative features]

On the other hand, we do have expertise in implementing ML-family type-inference engines. Our implementation is state-of-the-art in its choice of data structures (union-find rather than substitutions for unification, efficient level-based generalization, etc.). Previous prototypes written by some of us in this style have been observed to have at least comparable performance to the OCaml type-checker on certain programs. We expect this one to compare favorably as well: it should be as efficient as OCaml on programs that do not use suspended constraints. (Of course, suspended constraints in themselves cannot be compared, because they are not supported by existing type systems.)

In other words, our efficiency claims are not based on experiment, but on the use of state-of-the-art implementation techniques. We will rephrase the paper to clarify, thanks.

In fact, there is a trivial, brute force implementation of suspended constraints that would restart typechecking every time a new suspended constraint has been discharged!  Instead, our implementation of partial type schemes is very careful to resume instantiation from where it was suspended so as to avoid repeating work---which is actually not so trivial.

> - _would omnidirectional type inference enhance the support for
>   polymorphic variants?_

Probably not, as polymorphic variants use fairly different type-system mechanism -- they are not among the "fragile implicit features" we consider, as they are based on structural types with row variables.  (Similarly, omnidirectional type inference should not help with typechecking of polymorphic records or objects.)

