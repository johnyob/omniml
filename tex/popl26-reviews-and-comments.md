> POPL 2026 Paper #127 Reviews and Comments
> ===========================================================================
> Paper #127 Omnidirectional type inference for ML: principality any way

> Review #127A
> ===========================================================================
>
> Overall merit
> -------------
> B. OK paper, but I will not champion it
>
> Reviewer expertise
> ------------------
> Y. Knowledgeable
>
> Paper summary
> -------------
> In the Damas-Hindley-Milner type system, the type inferred for an
> expression does not depend on the order in which type inference for
> subexpressions is performed, and this matters to allow the existence
> of principal types.
>
> However, in some ML extensions this property is lost: a simple example
> are record labels which are overloaded, that is, present in different
> record types, so that in an expression r.x multiple types could be
> assigned to r.
>
> In such cases, the type inferred for an expression does depend on the
> order type inference for subexpressions is performed. Existing
> solutions essentially follow a given strategy. In this paper, the
> authors exploit another solution: very roughly, the type assigned to
> r is the unique type, if any, which makes the whole expression
> containing the subterm r.x well-typed. Otherwise an "ambiguity" error
> is raised. I find this key idea to forbid ambiguity natural and nice,
> and analogous to what happens in resolution of static
> overloading. However, to achieve this the technical treatment is quite
> complex, so I am not sure that
>
> Essentially, this is achieved by introducing suspended match
> constraints. Roughly, this means that the language of constraints
> includes a pattern matching construct of shape match $\tau$ with
> $\rho_1\rightarrow C_1, \ldots, \rho_n\rightarrow C_n$ which can be
> solved when the the type $\tau$ is not a variable.  In this case, it
> is checked whether its shape matches one of the (disjoint) patterns
> $\rho_1, \ldots, \rho_n$, otherwise the constraint fails.
>
> Comments for authors
> --------------------
> A general concern I have is that usually you first define the
> declarative type system and then constraint generation.
> Maybe this would be a better presentation, or at least you could
> discuss the choice?

We hesitated as well. The opinion of a majority of the co-authors is that the formulation of uniqueness in constraints is more general and more regular, so hopefully easier to understand, than the uniqueness conditions in the type systems which are slightly different for each feature. (The most general form for record overloading is too complex for a first introduction.)

Also, it may be the case that some type-inference authors will find constraints easier to relate to their own work, rather than the particular surface-syntax type system we present. (We do find it more natural to think of constraints first.)

> At least for me, it would be clearer to start from the declarative
> type system, which in a sense is a formalisation of the intuitive type
> one would assign to the problematic examples, and then insist more on
> the uniqueness conditions at lines 762-764 which are in my opinion the
> kernel idea. The analogous for the constraints will then follow more
> easily.
>
> Detailed comments and typos
>
> abstract line 2: maybe "which" should be erased?
>
> I would say "partial type schemes" rather than "partial types schemes".


@Alistair: Disagree here, the type schemes are partial?

>
> 239: We write V the set -> We write V for the set
>
> Footnote 2: as they which -> as they
>
> Fig.1 and its explanation: could you specify which constraint forms
> are novelties of your approach, and which are introduced in [Pottier
> and Remy 2005]? Certainly suspended match constraints are a novelty,
> but, e.g., constraint abstraction and application?

Constraint abstraction and application is not new, mostly a syntactic reformulation of previous works. (Technically there is a difference between solved constraint-abstractions and type schemes, but it is mostly a superficial/cosmetic difference.)

FIXME: Proposed fix, highlight the novelities? (e.g. in grey bg / yellow?)

> The last two productions (shapes and canonical principal shapes) are
> not explained here, you could anticipate some comments (or postpone
> the productions).
>
> 272-273: I would say the other way round (the abstraction constraint
> is applied to the type)
>
> 320: the metavariable $e$ is not known at this point
>
> 386: $x$ and $y$ -> $x$ and $z$
>
> 479: ground types -> ground type
>
> 451: the set of types associated to term variables is infinite, right?
> I see at line 498 that membership test is done by substitution, do you
> not need then to have a finite representation of the environment?


Not correct. 498 comments on a interpretation between application and substitution. The satisfiability of application (and the associated membership test) is not decided by substitution.

> 318: in rule Int-Shape, has the symbol # been defined before? Did I miss it?
>
>
>
> Review #127B
> ===========================================================================
>
> Overall merit
> -------------
> C. Weak paper, though I will not fight strongly against it
>
> Reviewer expertise
> ------------------
> X. Expert
>
> Paper summary
> -------------
> This paper proposes a new approach to ML-style type inference that
> incorporates support for some amount of type-based
> disambiguation. Unlike some previous approaches, this approach retains
> principality without introducing fragility in the resolution
> process. Notably, it makes sure to avoid the usual ordering normally
> induced by let-generalization. To do so, it proposes partial type
> schemes, which are type schemes that are generalized despite being not
> yet fully solved.
>
> Comments for authors
> --------------------
> The paper’s ideas and execution are sound and the system seems
> potentially useful. However, I have a few concerns regarding its
> acceptance:
>
>
> 1. **Missing discussions of related work.** The paper is missing many
>    pieces of important related work, including more recent work on the
>    same topic. In fact, the paper makes only three references to
>    papers that are less than 10 years old!
>
>     A few relevant older and newer references, off the top of my head,
>     and in no particular order (I am not related to any of them):


>     - *François Pottier. 2000. A versatile constraint-based type inference system. Nordic J. of Computing 7, 4 (Winter 2000), 312–347.* https://cambium.inria.fr/~fpottier/publis/fpottier-njc-2000.pdf
>
>         It seems that Pottier's conditional constraint system HM(SRC)
>         have strong similarities with match constraints and that
>         Pottier’s solution could also apply to many of the problems
>         described in the present paper. While that system is
>         𝜋-directional, this does not lead to fragility, since some
>         constraints are suspended, until more disambiguating
>         information is accrued later, and included in the type
>         scheme. It is not clear at all why that 25-year old solution
>         isn't good enough, and why we need the new system presented
>         here.
>
>         Pottier’s paper also cites a number of older related works in
>         a similar vein, which should also be discussed here.

François Pottier' conditional constraints succeed if they do not get discharged, so they are of use for fairly different language features. (Question: is there a semantics with a complete solver for it? The semantics in François' paper is declarative and probably too wide.)

>
>     - *Sheng Chen and Martin Erwig. 2016. Principal type inference for GADTs. In Proceedings of the 43rd Annual ACM SIGPLAN-SIGACT Symposium on Principles of Programming Languages (POPL '16). Association for Computing Machinery, New York, NY, USA, 416–428.* https://doi.org/10.1145/2837614.2837665
>         Their choice types seem like they could also be used, here, as
>         they solve some of the same problems.

Choice types introduce a type `D(A | B)` (`D` is the 'dimension'). For GADTs, Chen and Erwig eliminate the dimensions that choice types introduced (if a most generalizable type can be found for the choice types).

For overloading, we cannot eliminate choice types like this. If we naively use choice types, we would introduce a runtime overhead (by adding a tag for the dimension `D`).

We believe choice types can be combined with omnidirectional type inference for improved inference on overloading, where unification can eliminate dimensions.

>     - *Olivier Blanvillain, Jonathan Immanuel Brachthäuser, Maxime Kjaer, and Martin Odersky. 2022. Type-level programming with match types. Proc. ACM Program. Lang. 6, POPL, Article 37 (January 2022), 24 pages.* https://doi.org/10.1145/3498698
>
>         That paper does not provide a very formal semantics of match
>         types and does not use them *for* constraint solving, but
>         still, it should be mentioned, as it’s describing
>         a fundamentally analogous construct to match constraints.
>
>     - Although the submission discusses MLF, it does not compare the
>       relative expressiveness of the two systems, beyond mentioning
>       that MLF uses 𝜋-directionality. But it would be good to have
>       a good justification of why one would want to use your system
>       rather than MLF (possibly complemented with something like SML’s
>       monomorphic row types for variant/field overloading).

There seems to be two questions in one:

- MLF certainly improves over semi-explicit polyorphism, but it relies and extend the same basic \pi-directionality idea. The point or our work is not to compare several uses


>         On the one hand, the argument made in the introduction of this
>         submission (and also in 2.1) is not very convincing: why would
>         someone want to annotate a *use* of a variable and expect
>         other unrelated uses to non-locally benefit from that
>         annotation?

In the absence of let-polymorphism, type inference is plain unification and becomes obvious to the user that type information should be propagated --- users prefer to avoid repeating type annotations. We argue that this should not be broken by let-polymorphism.


>         On the other hand, the requirement of
>         semi-explicit polymorphism seems to make programming worse
>         than in a system like MLF, so it is not clear at all that you
>         are reaching for the right tradeoff, here.

Our system aims to target features present in OCaml, which does not include the higher-rank polymorphism present in MLF. We're aware that our system could be combined with MLF (in lines 1196-1197, we suggest that omnidirectionality could benefit MLF as well)

>     - *Jiří Beneš and Jonathan Immanuel Brachthäuser. 2025. The Simple Essence of Overloading: Making ad-hoc polymorphism more algebraic with flow-based variational type-checking. OOPSLA ‘25.*
>
>         Of course, you could not have known about this paper, since it
>         was just published. But it is quite relevant and you may want
>         to add it to your related work section, for completeness.
>
>     - Stefan Kaes. 1992. Type inference in the presence of
>       overloading, subtyping and recursive types. In Proceedings of
>       the 1992 ACM conference on LISP and functional programming
>       (LFP '92). Association for Computing Machinery, New York, NY,
>       USA, 193–204. https://doi.org/10.1145/141471.141540
>
> - Geoffrey S. Smith. Principal type schemes for functional programs with overloading and subtyping. Science of Computer Programming, Volume 23, Issues 2–3, 1994. [https://doi.org/10.1016/0167-6423(94)00020-4](https://doi.org/10.1016/0167-6423(94)00020-4).

This uses a form of qualified types in type schemes, so it corresponds to dynamic overloading.

>
> 2. **Lack of comparison with alternatives.** The purported advantage
>    of the presented system over existing work, such as OutsideIn(X),
>    is its support for fine-grained partial types schemes; however,
>
>     - This seems to be more of an engineering problem without a very
>       deep theoretical underpinning – sure, the semantics for
>       suspended match constraints is not as obvious as one would
>       think, but it is still rather straightforward.

We firmly disagree. It was a lot of work to come up with satisfying declarative semantics for suspended constraints, we went through many iterations which ended up not working, and had to work through the detailed proofs to find a suitable definition. We are convinced that coming up with a declarative semantics is a significant contribution -- the main reason why our work was not published a few years before, as semantics were the missing piece.

To be more objective and less anecdotal about this: we see a connection between our difficulties finding a declarative semantics and what happened in the OutsideIn(X) work. In the conference version of the OutsideIn paper there is a declarative semantics, which is rather subtle and elegant. In the journal version of the work, the declarative semantics is gone, because the authors failed to generalize it to other surface-language features than GADTs. (Note: our paper as submitted claims that OutsideIn does not come with a declarative semantics, because we only looked at the journal version at first. We fixed this mistake in our current version.)

To quote the JFP paper on OutsideIn (page 50-51):

> To sum up, our specification accepts some “bad” programs (ones that are ambiguous, or lack a principal type), and the OutsideIn(X) algorithm rejects some “good”ones.
> The latter is no great surprise. For example, the Hindley-Milner algorithm accepts only λ-abstractions whose binder has a monotype. We accept that a tractable algorithm cannot work magic, so instead we tighten the specification so that it matches what the algorithm can achieve.
> The obvious way to restore completeness is is to tighten up the specification, so that it rejects both (a) bad programs and (b) programs that the inference algorithm cannot type. The trouble is that the cure is worse than the disease: the specification becomes as complicated and hard to understand as the algorithm.
> For one such attempt the reader is encouraged to read our earlier version of the OutsideIn(X) algorithm, which had a fairly complicated specification, and one that worked only for the special case of GADTs in (Schrijvers et al., 2009), and neglected ambiguity entirely. For the general case of arbitrary constraint domains and local constraints, we are not optimistic about this approach

We believe that our declarative semantics is at least of comparable difficulty to the declarative semantics that the OutsideIn(X) authors were looking for and failed to be able to formulate in a general enough way. In fact we could go slightly further: in the OutsideIn work, each GADT match is type-checked independently with no flow of information from inside to outside, so in particular no communication between GADT matches that are not syntactically nested. In contrast, our suspended constraints can exchange information, discharging one can disambiguate another and lead to its discharge; our declarative semantics has to support this "causality" between syntactically-separate constraints.

The primary reason OutsideIn(X) (JFP paper) removes local let generalization is to simplify their declarative (and still incomplete) specification:

> In summary, generalisation of local let bindings (without annotations) is a device that is almost never used, and its abolition yields a dramatic simplificationin both the specification and implementation of a typechecker.

Note: we are not covering the same language features and design issues as the OutsideIn(X) work, so we are not claiming to strictly subsume their work, but we believe that the difficulty (the non-straightforwardness) is comparable.

>     - There is no quantitative and little qualitative evaluation to
>       indicate that the presented system indeed performs better than
>       the existing ones. The authors write, in the context of
>       OutsideIn(X), “we believe that we have solved the troubling
>       interactions between let-generalization and suspended
>       constraints in this work”, but this belief is never
>       substantiated. However, it is a at the core of deciding whether
>       the paper makes a significant contribution or not!

We apologize for the too-careful wording. Let us *claim* that we have solved the interaction between suspended constraints and let-generalization (via partial type schemes), both in theory and in practice: this is the very point of our submission indeed.

>         I would want to see, at the very least, rigorous comparisons
>         with all of OutsideIn(X), MLF, and HM(SRC).
>
> 3. **Unclear connection to OCaml.** Unless I missed it, the paper
>    stays silent about a crucial aspect of the problem it is trying to
>    tackle: the interaction between fields/variant overload resolution
>    and the typing of polymorphic record fields in OCaml. The
>    *semi-explicit polymorphism* setting of the paper, which uses
>    explicit user-provided boxing and unboxing, seems too simplified to
>    capture that essential difficulty. Indeed, as far as I'm aware,
>    there is no "polymorphism box” syntax in OCaml, and polymorphic
>    types are inferred based on context.

To the best of our knowledge, the pi-directional features of OCaml (there are several) were all designed, implemented or at least reviewed by people familiar with the semi-explicit polymorphism work (Didier Rémy and Jacques Garrigue), and relating to semi-explicit polymorphism is how OCaml maintainers told us that they reason about these type-system features. This is the basis of our decision to emphasize semi-explicit first-class polymorphism in our presentation.

We could explain the elaboration of OCaml's polymorphic methods into the base constructs of semi-explicit polymorphism, but it is unclear that this would be a good use of the already much-constrained space. (We did try to explain this shortly in lines 310-313.)

>     Now, one could imagine implicitly boxing every record field and
>     every argument and implicitly unboxing all parameters and field
>     selections. This could allow users to explicitly annotate record
>     fields and parameters with polymorphic types, while leaving their
>     provider and consumer expressions handle that polymorphism
>     implicitly. Would this not give us a form of higher-rank
>     polymorphism?

This is precisely the elaboration of polymorphic methods into semi-explicit first-class polymorphic primitives.

>     None of this is discussed in the present paper. Instead, we have
>     to try and read between the lines; we are left wondering how
>     everything fits in together and applies to the real language.
>
> 4. **Poor exposition and not self-contained.** The paper is not very
>    well written and its exposure needs lots of polishing.
>
>     The paper is not self-contained. It introduces each important
>     concept haphazardly through vague textual descriptions. Several
>     times, I screamed to myself “Where are all the syntax definitions?
>     What is that metavariable for?” I found that there is a helpful
>     appendix summarizing most of this information, but this info
>     *should be* in the main paper, too. Most typing rules are not even
>     given or discussed in the main paper.

TODO come up with a credible revision plan to address this valid concern.


>     Relatedly:
>
> 5. **Inadequate focus.** It seems the paper is trying to do too much,
>    with too little space available. The main issue I see is that the
>    problems of record field, variant constructor, and tuple projection
>    overloading is uninteresting and already solved by other
>    systems. As mentioned in the paper, even SML does it. Adding
>    monomorphic row variables to a language is not rocket science.
>
>     It seems to me that the paper should focus on a single aspect that
>     is *not* handled well by any other system – which *appears* to be
>     partial type schemes with suspended constraints (although a proper
>     analysis of previous work would still be needed to show
>     that fact). Then, the paper could still say, *in passing*, that
>     the same infrastructure can also be used to handle the more
>     mundane overloading tasks mentioned above (which even SML knew how
>     to deal with), without having to develop an extensive and
>     distracting theory about all these features together, as the
>     current submission does.
>
>
> For all these reasons, I am leaning rather negatively on this paper
> overall, but would like to give the authors a chance to address my
> complaints first.
>
> Points 2, 4, and 5 are the overriding concerns to me. There is
> probably a nice paper hiding in there, but extracting that paper from
> the current draft seems like it would require too much work to be done
> within a conditional acceptance timeframe. As it stands, the current
> paper is not self-contained enough and too hard to read.
>
> Specific questions to be addressed in the author response
> ---------------------------------------------------------
> What is the worst-case complexity of your approach? You might want to
> consider a simplified setting with no let-generalization to make this
> analysis more informative. With let-generalization, does your approach
> make the complexity of constraint solving significantly worse? What
> makes you think it is still practical in usual user-written programs?

With no let-generalization, solving constraints becomes plain unification, so there is no extra cost to suspended constraints. (Implementation-wise: it's possible to add a check on each variable-structure unification, and discharge any constraint suspended on this variable). Note: suspended constraints without generalization are folklore in other type-inference systems (eg. dependent type-checkers) and not something we claim is novel.

The interesting setting is in presence of let-generalization. ML inference is doubly-exponential, but let us assume that let-nesting depth in bounded. A key point is that we never backtrack nor duplicate the body of suspended constraints. When a polymorphic region makes progress (discharges a suspended constrant), our partial instantiation machinery implies some extra work for each instantiation of the partial polymorphic scheme. We cannot make precise complexity claims at this point, but we have tried our best to avoid unnecesary repeated work and use appropriate data structures.

> What are the error messages like, in your new framework? OCaml’s type
> error messages are notoriously bad. Could they now get even worse? Or
> do you have a specific plan to handle this problem?

Not yet, unfortunately.
We believe with sufficient engineering work, that error messages would not be worse.

> > 1172: We have also experimented with a more general overloading
> > mechanism in which several definitions may be bound to the same
> > identifier 𝑀.𝑥, but prefixed with a namespace
>
> Doesn’t this make you jump right back to backtracking-based solving of
> an NP-hard problem? You wrote earlier that:
>
> > We believe that this restriction is necessary for effective type
> > inference, since the complexity of general overloading without this
> > restriction is NP-hard, even in the absence of let-polymorphism,
>
>
>
>
> Review #127C
> ===========================================================================
>
> Overall merit
> -------------
> B. OK paper, but I will not champion it
>
> Reviewer expertise
> ------------------
> Y. Knowledgeable
>
> Paper summary
> -------------
>
> This paper presents a novel approach to type inference in ML-like
> languages, which consists in being more flexible in the order in which
> constraints are resolved. Instead of fixing a priori an ordering
> strategy (eg. first infer the polymorphic type of a bound expression
> and then typecheck the instances), the proposed approach leaves some
> constraints "suspended" until some more information is available in
> the context to solve them. The technical devices underlying the
> approach are suspended match constraints, and partial type schemes,
> which can be incrementally refined. The paper shows the advantage of
> this approach on several advanced mechanisms for which traditional ML
> implementations are rather fragile and/or require explicit
> annotations, namely static overloading of record labels and
> constructors, semi-explicit first-class polymorphism, and tuple
> projections.
>
> Comments for authors
> --------------------
>
> Omnidirectional type inference appears to be an effective improvement
> over both the state-of-the-art and the state-of-the-practice in ML
> languages. The paper is clear on the limitations of existing
> approaches and manages to introduce the intuitions behind
> omnidirectional inference rather clearly.
>
> However, the paper is very dense technically, and many important
> definitions are left to appendix due to space restriction. As such, it
> is really not an easy read (I was technically outscored at
> several places), and errs on the side of not being self-contained
> enough. Given the large space given to the future work section, it
> might be worthwhile considering pruning that section in favor of
> smoother explanations and more illustrations in different steps of the
> technical development.
>
> There is a claim about the description of an *efficient*
> implementation. However, despite a few explanations in section 6 about
> efficiency-driven decisions, the efficiency of the implementation is
> not validated at all; there are no benchmarks comparing with other
> approaches and/or showing the benefits of said decisions. I'd suggest
> to tone down that efficiency claim.

It is difficult to benchmark new type-systems, as there are no large, representative user programs to measure, and writing large synthetic terms is hardly representative. (One could also think of translating existing ML programs to our system, but few only cover a few representative features and practical programs are outside this subset.)

On the other hand, we do have expertise in implementing ML-family type-inference engines. Our implementation is state-of-the-art in its choice of data sturctures (union-find rather than substitutions for unification, efficient level-based generalization, etc.). Previous prototypes written by some of us in this style have been observed to have at least comparable performance to the OCaml type-checker on certain programs. We expect this one to compare favorably as well: it should be as efficient as OCaml on programs that do not use suspended constraints. (Of course, suspended constraints in themselves cannot be compared, because they are not supported by existing type systems.)

In other words, our efficiency claims are not based on experiment, but on the use of state-of-the-art implementation techniques. We will rephrase the paper to clarify, thanks.

> The use of semi-explicit first-class polymorphism as the first example
> in section 2 greatly suffers from a lack of explanations for people
> not familiar with that mechanism. At least a simple example showing
> the use of (un)boxing and the advantages of that technique would help
> to follow.
>
> Presentation-wise, I was puzzled by the absence of the definition of
> constraint generation ([[ ]]), given the somewhat central role it
> plays in the examples (eg. 3.10, 3.11) and some theorems
> (eg. 4.2). There are only some cases in section 2, none use the _
> pattern generally.
>
> Overall, I found section 3 hard to follow, and this feeling only
> worsened in the following two sections. At an intuitive level, I could
> follow the general approach, but was lost in the details and heavy
> reliance on appendices for definitions. I will therefore defer to more
> expert reviewers on that aspect.
>
>
> Details:
>
> - L99 "OCaml does not make any difference between ex3, ex32, or ex33"
>   this contradicts the fact that ex33 yields a warning, while the
>   other two are rejected.
>
> - L148 "the implementation of OCaml cheats and is incomplete by
>   default" any specific reference(s) to back up this claim?
>
> - L153 "omnidirectional inference is then natural..." At this point
>   the concept has not been introduced yet
>
> - L171 "everything is hard" what does this mean precisely?
>
> - L179 first occurrence of OmniML in the text, despite the fact that
>   it appeared in the code snippets; I'd suggest to say upfront, before
>   the first example, what OmniML is
>
> - L198 "from an instance to back the definition": from an instance
>   back to the definition
>
> - L200-202 why not using such "alternative typing rules" directly in
>   order to support said example?
>
> - L230 "Three instantiation" missing 's'
>
> - L288 "sufficies"
>
> - L386 "x and y" should be x and z
>
> - L408 shouldn't it be β → α  ?
>
> - L448 "such polymorphic fields" missing "as"
>
> - L472 "what it means for type information to be known": indeed, and
>   some early insight would help
>
> - L479 "a ground types"
>
> - example 3.10 how are the constraints obtained
>
> - it is weird to put definition 5.1 before the section that presents the syntax.
>
> - bibliography: all occurrences of "François" (Pottier) have been butchered
>
> Specific questions to be addressed in the author response
> ---------------------------------------------------------
> - would omnidirectional type inference enhance the support for
>   polymorphic variants?

Probably not, as polymorphic variants use fairly different type-system mechanism -- they are not among the "fragile implicit features" we consider, as they are based on structural types with row variables.

