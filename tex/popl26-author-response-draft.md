We are grateful to our reviewers for their interest in our work and their copious, high-quality feedback. Thanks!

We are particularly grateful for the very detailed criticisms in review B. Some criticism we realize that we agree on, and we have proposals to fix them. Some comments suggest that we did not get some important points across as clearly as we should have, and they also inform our plan to clarify the paper.

Our response is too long, but it discusses the important points first that we hope may be of interest to all reviewers, and you can stop at any point. In the following:

1. We propose a workable plan to revise the paper to make it more focused and more self-contained, and to rewrite the introduction so that *future readers* don't need to ask your important questions.

2. We discuss the feature space of ML-family-and-neighbors type inference topics related to overloading, guided by the helpful reference list of reviewer B, to clarify what is the focus of *this* work.

3. We convince reviewer B (or, as a backup plan, both A and C :-) that providing declarative semantics for suspended constraints is in fact the opposite of "rather straightforward": a substantial scientific contribution.

4. Finally we discuss local comments of each review in order, so that our scientific exchange is as satisfactory as possible if you have the curiosity to read them.


## 1. Revision plan

A common theme across reviews is that the paper is difficult to read:
definitions are scattered, essential material is deferred to appendices, and
the line of contributions is obscured by staging and breadth. We acknowledge
these issues (and were in fact aware of them prior to your reviews). Below we
outline concrete changes we will make in a revision.

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

Prior work (helpfully highlighted by reviewer B) only deals with top-level
let-generalization. Local let bindings are treated monomorphically, and
therefore do not address this troubling interaction.

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

### Clearer treatment of features

At present the paper tries to cover too much. To reduce breadth and improve
narrative continuity, the main text will focus **exclusively** on OCaml
features:

- Tuple overloading will be removed from the main text.

- Section 2 will explicitly relate semi-explicit first-class polymorphism to
  OCaml's polymorphic methods, showing their reduction into object methods
  (which are orthogonal to our work) and polytypes.

  This addresses the gap noted by B and C.


### Consolidation of technical material

Reviewers B and C found the paper insufficiently self-contained, with
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
(in the appendix) into the paper, but we hope this addresses reviewer B and C's
comments regarding difficulty reading technical sections.

### Space

We fund the added self-contained material by (i) removing tuple overloading from
the main text (~1-2 pages), (ii) pruning conclusion/future work (~1 page),
(iii) consolidating split definitions into single figures (net-neutral), and
(iv) removing duplicated exposition by consolidating sections (net-neutral).

### Overview: Revised paper sections

Below is the revised paper sections:

- Introduction (1)
  - Contributions (1.1)

- Overview (2)
  - Static overloading of constructors and record labels (2.1)

  - Polymorphic methods (2.2)
    - Semi-explicit first-class polymorphism

  - Directional type inference (2.3)
    - $\pi$-directional type inference
    - Bidirectional type inference
    - Limitations of directional inference

  - Omnidirectional type inference (2.4)
    - Suspended constraints
      - Two mini examples of constraint generation for records and polytypes
    - Scaling to ML
      - Back-propagation and the requirement of local let-generalization

- Constraints (3)

  *Cheat sheet: syntax and semantics*
  - Syntax and semantics outlined.

  - Shapes and patterns (3.1)

  - Suspended constraints (3.2)
    Examples

- The OmniML calculus (4)

  *Cheat sheet: syntax*
  - Syntax outlined

  - Typing rules (4.1)

    *Cheat sheet: typing rules and unicity definition*
    - Examples

  - Constraint generation (4.2)
    Examples

  - Metatheory (4.3)

- Solving constraints (5)

  *Cheat sheet (constraints): new syntax and semantics. e.g. unification problems, regional let constraints, partial instantiations*

  - Unification (5.1)

    - Re-frame unification as an abstract rewriting relation that
    satisfies termination, preservation, progress. This parameterizes
    our solver by an arbitrary equational system.

    - Definition of solved form

  - Solving rules (5.2)

    *Cheat sheet: rewriting rules for constraint solving and associated definitions (e.g. C determines 'bs)*

    - Basic rules
    - Let constraints
    - Suspended match constraints
      - Back-propagation

  - Metatheory (5.3)

- Implementation (6)
  - Remains unchanged

- Related work (7)
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

- Conclusion (8)
  - Future work


## 2. Explaining the feature space



## 3. The difficulty of finding good declarative semantics

> 2. **Lack of comparison with alternatives.** The purported advantage
>    of the presented system over existing work, such as OutsideIn(X),
>    is its support for fine-grained partial types schemes; however,
>
>     - This seems to be more of an engineering problem without a very
>       deep theoretical underpinning – sure, the semantics for
>       suspended match constraints is not as obvious as one would
>       think, but it is still rather straightforward.

We firmly disagree. It was a lot of work to come up with satisfying declarative semantics for suspended constraints, we went through many iterations which ended up not working, and had to work through the detailed proofs to find a suitable definition. We are convinced that coming up with a declarative semantics is a significant contribution --- the main reason why our work was not published a few years before, as semantics were the missing piece.

To be more objective and less anecdotal about this: we see a connection between our difficulties finding a declarative semantics and what happened in the OutsideIn(X) work. In the conference version of the OutsideIn paper there is a declarative semantics, which is rather subtle and elegant. In the journal version of the work, the declarative semantics is gone, because the authors failed to generalize it to other surface-language features than GADTs. (Note: our paper as submitted claims that OutsideIn does not come with a declarative semantics, because we had only looked at the journal version at first. We re-read the conference version after submission, realized our mistake, and fixed it in our current draft.)

- conference version: https://www.microsoft.com/en-us/research/wp-content/uploads/2009/09/implication_constraints.pdf
- journal version: https://simon.peytonjones.org/assets/pdfs/outsideinx.pdf

Let us quote the journal version (section 6.5, pages 50-51).

> To sum up, our specification accepts some “bad” programs (ones that are ambiguous, or lack a principal type), and the OutsideIn(X) algorithm rejects some “good”ones.
> The latter is no great surprise. For example, the Hindley-Milner algorithm accepts only λ-abstractions whose binder has a monotype. We accept that a tractable algorithm cannot work magic, so instead we tighten the specification so that it matches what the algorithm can achieve.
> The obvious way to restore completeness is is to tighten up the specification, so that it rejects both (a) bad programs and (b) programs that the inference algorithm cannot type. The trouble is that the cure is worse than the disease: the specification becomes as complicated and hard to understand as the algorithm.
> For one such attempt the reader is encouraged to read our earlier version of the OutsideIn(X) algorithm, which had a fairly complicated specification, and one that worked only for the special case of GADTs in (Schrijvers et al., 2009), and neglected ambiguity entirely. For the general case of arbitrary constraint domains and local constraints, we are not optimistic about this approach.

We believe that our declarative semantics is at least of comparable difficulty to the declarative semantics that the OutsideIn(X) authors were looking for and failed to be able to formulate in a general enough way. Here is an explanation for our intuition: in the OutsideIn work, each GADT match is type-checked independently with no flow of information from inside to outside, so in particular there is no communication between GADT matches that are not syntactically nested. In contrast, our suspended constraints can exchange information, discharging one can disambiguate another and lead to its discharge; our declarative semantics has to support this "causality" between syntactically-separate constraints (without allowing self-justification cycles, which has plagued several of our attempts).

Simplifying their specification was also a major reason why the JFP removes local let-generalization (end of page 32):

> In summary, generalisation of local let bindings (without annotations) is a device that is almost never used, and its abolition yields a dramatic simplificationin both the specification and implementation of a typechecker.

Note that we are not covering the same language features and design issues
as the OutsideIn(X) work (we do not consider type families, for example), so
we are not claiming to subsume their work, but we claim that the difficulty
(the non-straightforwardness) is at least comparable.


## 4. Local comments

### Review A

### Review B

### Review C
