We thank the reviewers for their careful reading and thoughtful, constructive
feedback! We have revised the paper accordingly. 

We begin by summarizing the main changes, and then provide detailed responses
to the individual comments of each review.

# Improved presentation of polytypes 

The presentation of polytypes was identified as a challenging aspect of the
paper by all reviews. We have therefore significantly reworked this part of
the exposition. In particular, we now:

- Introduce polytypes *without* annotation variables first, in the context of
  discussing OCaml's polymorphic methods. This separates the discussion of
  polytypes from existing directional approaches (such as pi-directional
  inference), resulting in two clearer and more focused sections.

- Expand and clarify the relationship between polytypes and explicit / implicit
  first-class polymorphism. We now explicitly state that inference for
  polytypes is *easier* than for implicit first-class polymorphism.

- Add an explicit point explaining that we do not currently know how to support
  omnidirectional inference for implicit first-class polymorphism *without*
  default rules, which are beyond the scope of this work. 

# Improved presentation of pi-directionality

With a clearer separation from the introduction of polytypes, we were able to 
simplify the presentation of pi-directional inference: 

- We removed the technically heavy judgement `(𝜎1 : 𝜎 : 𝜎2)` by dropping
  support for existentially quantified type variables in annotations. As a
  result, we replace it with a simpler judgement `𝜎1 =>ann 𝜎2` which inserts
  fresh annotation variables on boxed polytypes `[ 𝜎 ]`.

- We added all the rules for the simple bidirectional system used in comparisons, 
  eliminating confusion and ambiguity that review 2 correctly highlighted. 

# New section: Discussion 

We added a new 'Discussion' section as a new home for our discussion
of the semi-unification-based implementation, and other practical
considerations highlighted by reviews 1 and 3:

- A discussion of error reporting: we did not work on this yet, but
  we believe that our work would be compatible and in fact play nicely
  with a "getting into the flow" approach.

- A discussion of integrability into OCaml, separating the OCaml type
  system abstractly and its current implementation in the OCaml
  compiler.

- A discussion of semi-unificatio as an alternative approach for
  implementing incremental instantiation.

# Incremental instantiation (Section 5.2-5.3)

All reviewers noted that the presentation of incremental instantiation was
difficult to follow. While some of this complexity is inherent, we have
improved the exposition:

- We added a new, dedicated in Section 5.3 that continues the running example
  from Section 5.2 and demonstrates incremental instantiation in action, which
  we hope should make the mechanism more concrete and easier to understand.

# Comparisons to bidirectional typechecking

We have strengthened the related work discussion by adding a dedicated
subsection on directionality: 

- This section includes the survey of existing approaches (let arguments go first
  and contextual typing) in the setting of bidirectional typing. 

- We incorporate the insightful connections to contextual typing from review 2.

- We clarify our position that omnidirectionality subsumes contextual typing in
  terms of expressiveness. Concretely, we have implemented a prototype (OmniF)
  that supports implicit first-class polymorphism in the style of local type
  inference. Our experience with this system suggests that it subsumes local
  contextual typing in practice.

- At the same time, we now explicitly restate an important limitation:
  omnidirectionality alone does not subsume all bidirectional systems. In
  particular, systems supporting implicit first-class polymorphism appear to
  require default rules.

# Local comments

We address review-specific comments in detail below.

### Review 1

> 
> - page 12, fig 2, rule Annot
> 
> I was surprised to see the result type as `𝜏[𝛼 := 𝜏]`, rather than `∃𝛼.𝜏`. Is
> this specific to how OCaml handles existential refinement?
> 

This is the generally accepted rule for existential variables in type
annotations for ML-like systems. e.g. PolyML uses a similar rule using the 
`(𝜎1 : 𝜎 : 𝜎2)` judgement to instantiate existential variables in `𝜎`. 

> 
> - page 15, line 25-26, Definition 3.1
> 
> It seems you only reason about canonical principal shapes. Would it simplify the
> presentation to introduce canonical shapes directly?

Canonical principal shapes have a rather complex structure. Whereas the
properties of being canonical and principal are rather intuitive, hence why we
introduce shapes, principal shapes and canonical principal shapes.

> 
> - page 15, line 37-39, `𝜈𝛾.[∀𝛼.([∀𝛽.(𝛽 →𝛾)∗ 𝛽])→𝛼 →𝛼].`
> 
> It was not clear to me until this example that the holes (`𝛾`) cannot refer to
> bound variables (e.g. `𝛼` and `𝛽`). I think this is a crucial detail worth
> stating explicitly.

This just a property of the substitution being capture avoiding wrt to
universal quantifiers.

> 
> - page 16, Fig 3, `ℰ [𝑒 ⊳ 𝜍 | 𝑒s] `
> 
> I'm confused about why you require the list of `𝑒s`, as it does not seem to be
> used in the rules.

This is only because all our fragile elimination rules (Rcd-Proj-I, Use-I,
Proj-I) only have one subterm. 

But this isn't the case for all features. If one were to consider
omnidirectionality for local type inference in the style of Pierce and Turner,
we could have:
  
   𝜏 ::= ... | ∀𝛼s. 𝜏s -> 𝜏

with the rule: 


    ℰ [e ⊳ νɣs. ∀𝛼s. 𝜏s -> 𝜏 | 𝑒s]
    Γ |- ℰ[(e : ∃ɣs. ∀𝛼s. 𝜏s -> 𝜏)@es] : 𝜏'
   ----------------------------------------- [App-I]
    Γ |- ℰ[e es] : 𝜏'

Here, e es is the implicit form for application and (e : ∃ɣs. ∀𝛼s. 𝜏s -> 𝜏)@es
is the explicit form, with the rule: 


    Γ |- e : (∀𝛼s. 𝜏s -> 𝜏)[ɣs:=𝜏s']
    Γ |- es : 𝜏s[ɣs:=𝜏s', 𝛼s:=𝜏s'']
   -------------------------------------------------------- [App-X]
    Γ |- (e : ∃ɣs. ∀𝛼s. 𝜏s -> 𝜏)@es : 𝜏[ɣs:=𝜏s', 𝛼s:=𝜏s'']

> - page 17, line 21, the omnidirectional recipe
> 
> The distinction between introduction and elimination forms here feels
> reminiscent of bidirectional typing.

Added a remark on the similarity between our recipe and the Pfenning recipe.

> 
> - page 60, line 23-24, "This simplifies the proof, but introduces a circular
> dependency between Theorem B.5 and Lemma B.6. "
> 
> Theorem B.5 does not seem to use Lemma B.6 (or Corollary B.7).

Correct, this appears to be an artefact of an earlier version of the proof. 

> 
> - page 62, line 39-40, "𝜙 ⊢𝒞2 [match 𝜏 := 𝜍 with¯ 𝜒,𝐶2] By i.h."
> 
> Should it be `𝜙 ⊢𝒞2 [𝐶2] By i.h.`?

No, 𝒞2 is a *two-hole* where 𝒞2 [match 𝜏 with¯ 𝜒,𝐶1] = 𝒞[𝐶1]. 
The inductive hypothesis is: 
    
    forall 𝒞' with one less suspended match constraint than 𝒞, 
      𝒞' [𝐶1] === 𝒞' [𝐶1]

On line 36, we have

    𝜙 ⊢𝒞2 [match 𝜏 := 𝜍 with¯ 𝜒,𝐶1] 

and by noting that 

    𝒞2 [match 𝜏 := 𝜍 with¯ 𝜒,-]

has one less suspended match constraint than 

    𝒞[-], 

then 

    𝜙 ⊢𝒞2 [match 𝜏 := 𝜍 with¯ 𝜒,𝐶2] 

Note: There was a typo on line 36-37, where we claimed that 
𝒞2 [match 𝜏 := 𝜍 with¯ 𝜒,𝐶1] = 𝒞[𝐶1]. 


## Review 2

> ### Principal Shapes
> 
> On page 15 line 38, why the principal shape is not `ν γ . [∀a . γ -> a -> a]`?

Correct! However, we ended up fixing the type to get the desired shape. 

> ### Terminology of "polytypes"
> 
> It's a bit confusing to me that the paper uses the term "polytypes" to
> refer to polymorphic types with boxes. Polytypes to me are just
> abbreviations for polymorphic types. I understand that this is also
> the terminology used in Garrigue and Rémy 1999. I would appreciate if
> the paper can explicitly use something like ``boxed polytypes''. Feel
> free to ignore this comment if you'd like to keep "polytypes".

We carefully considered this suggestion. While it is true that the term
``polytype'' dates back to Milner's (1978) _A Theory of Type Polymorphism in
Programming_, in modern ML settings it has largely been supplanted by ``type
scheme''. In this paper, we ended up following the terminology of Garrigue and
Rémy (1999).

We experimented with the term ``boxed polytypes'', but found it to be overly
explicit and somewhat verbose, without providing additional clarity to justify
the added ``boxed'' qualifier.

> 1. Page 8 line 9: "so some well-typed programs are rejected as ill-typed (e.g. ex62, ex63)."
> 
> It's unclear to me what this sentence means. Because contextual typing
> does not have polymorphism. In their POPL26 paper on local contextual
> type inference which supports polymorphism, I think ex62 fails but
> ex63 succeeds, because their approach is order sensitive.

Added a footnote stating that to typecheck ex62 and ex63 using contextual
typing, one must inline `app` and `rev_app`. The comment correctly points out
that the ICFP24 contextual typing work doesn't support polymorphism.

> ## Non-technical Comments
> 
> In section 2, I think the colours you use for success and warnings
> can be challenging to distinguish for colour-blind readers. I would
> suggest using colour-blind safe colours.

We added explicit symbols for the success, warning and error cases,
improving clarity and ensuring accessibility for color-blind readers 
as well as in black-and-white print.

## Review 3

> p.17, LL13-20: I had this very question immedietely after looking at
> the definition of the unicity condition and had a hard time reading the
> paragraphs before this one.  I wonder if it's a good idea to
> distinguish two typing relations, one defined by the rules in Fig. 2
> and Hole, and the other for the full language.  Then, the unicity
> condition (for expressions without fragile constructs) can be
> explained and defined before presenting Fig. 3.

When revisiting this part of the paper, we concluded htat the primary
difficulty is not with the well-foundedness of the definition (which follows
relatively directly), but with building intuition for what the unicity
condition enforces and why it is needed. Accordingly, rather than introducing a
second typing relation (which would require this explaination upfront), we have
left the presentation as is.

