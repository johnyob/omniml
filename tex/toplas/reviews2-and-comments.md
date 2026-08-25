# Referee: 1

## Summary

> This revised version of the paper addresses most of my concerns and
> comments. In particular, it clearly explains the relationship between
> OmniML, bidirectional typing, and contextual typing, as well as the
> relationship between semi-explicit first-class polymorphism and
> implicit first-class polymorphism.
> 
> I recommend the paper for acceptance.
> 
> I still have one minor comment which I will list below. I believe the
> authors can easily address it (or ignore if I am wrong) in the final
> version of the paper.

## Comments

### Semi-explicit first-class polymorphism with bidirectional typing

> Thank you for providing the full rules of this system in Figure 3.
> I see that Chk-Fun requires checking mode, which resolves my concern
> of the example `\lambda x . <x>` in my review.
> 
> However, I am still not convinced that this system is ``correct''. My
> new example is `id (\lambda x . <x>)`. With rule Chk-Inst, I guess we
> can instantiate `id` with any type `[σ] -> τ` where `τ` is an
> instantiation of `σ`. I think this still amounts to guessing
> polymorphism and is not allowed in either π-directional type inference
> or OmniML.
> 
> Also, on page 9 line 34, I think both ex62 and ex63 are accepted with
> the bidirectional system in Figure 3, but I can believe that in OCaml
> only ex62 is accepted (though I am not sure if I understand why OCaml
> is a bidirectional approach).

# Referee: 2

> The revised version addresses the reviewers' major concerns.
> I'll be happy to accept acceptance.
> 
> Actually, a few minor points (raised in my first review) remain.  I
> don't have to check if the final version addresses them.
> 
> > p.4, L18 and L26: There are two references to footnote 2.  Is this
> > intended?
> 
> There are still two references to footnote 3 on L29 and L38.  I'm not sure how this happens.
> 
> > p.27, Def. 5.1: Later you use \hat C and \hat{\mathcal{C}} but they
> don't appear to be defined.
> 
> I don't remember why I mentioned Def. 5.1 here, but my question was:
> What does \hat{\mathcal{C}} used in Theorem 5.5 stand for?  (The role of
> \hat is not clear.)
> 
> > p.29, L39:  What does \bar \epsilon stand for?
> 
> The role of \bar is not clear.  Is "\bar \epsilon" just another
> metavariable for multi-equations, like \epsilon_1?  Or, does \bar
> stand for a sequence?  I might have missed notes on notational
conventions.


# Referee: 3

## Summary

I have reread the paper, and it has improved. The discussion of Pi-directional
type inference is much clearer. The discussion of implementations has also
changed, from two implementations to a single one in Section 6 (with the second
one retained in Section 7).

The revision has added explanations, examples, and discussion throughout,
including defaulting for polymorphic methods, Example 5.4, and Section 7. I like
the new discussion in Section 7, especially the perspectives on integration into
OCaml.


## To author response

> > > - page 16, Fig 3, `ℰ [𝑒 ⊳ 𝜍 | 𝑒s] `
> > >
> > > I'm confused about why you require the list of `𝑒s`, as it does not seem to be
> > > used in the rules.
> >
> > This is only because all our fragile elimination rules (Rcd-Proj-I, Use-I,
> > Proj-I) only have one subterm.
> >
> > But this isn't the case for all features.
> 
> This is a fair answer, but I think it is worth a note in the paper. Readers will
> naturally wonder why the notation carries a list of expressions that is never
> used anywhere in the presentation, and a sentence explaining that the list is
> useful for elimination forms with multiple subterms would help.


## Other comments

> - Page 6, footnote 4 appears on the wrong page.
> 
> - Page 6, it may be worth mentioning that rule Annot-Poly is not the only rule
> for the ==>ann judgment, but perhaps the only interesting one.
> 
> - diff page 29. There seemed to be some discussion on the stability of typing by
>   common program transformations. I could not find it anywhere in the paper. Do
>   those results still hold? If so, It'd be useful to add them to the discussion.
