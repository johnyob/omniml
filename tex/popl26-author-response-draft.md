We are grateful to our reviewers for their interest in our work and their copious, high-quality feedback. Thanks!

We are particularly grateful for the very detailed criticisms in review B. Some criticism we realize that we agree on, and we have proposals to fix them. Some comments suggest that we did not get some important points across as clearly as we should have, and they also inform our plan to clarify the paper.

Our response is too long, but it discusses the important points first that we hope may be of interest to all reviewers, and you can stop at any point. In the following:

1. We propose a workable plan to revise the paper to make it more focused and more self-contained, and to rewrite the introduction so that *future readers* don't need to ask your important questions.

2. We discuss the feature space of ML-family-and-neighbors type inference topics related to overloading, guided by the helpful reference list of reviewer B, to clarify what is the focus of *this* work.

3. We convince reviewer B (or, as a backup plan, both A and C :-) that providing declarative semantics for suspended constraints is in fact the opposite of "rather straightforward": a substantial scientific contribution.

4. Finally we discuss local comments of each review in order, so that our scientific exchange is as satisfactory as possible if you have the curiosity to read them.


## 1. Revision plan

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

We believe that our declarative semantics is of comparable difficulty to the declarative semantics that the OutsideIn(X) authors were looking for and failed to be able to formulate in a general enough way. Here is an explanation for our intuition: in the OutsideIn work, each GADT match is type-checked independently with no flow of information from inside to outside, so in particular there is no communication between GADT matches that are not syntactically nested. In contrast, our suspended constraints can exchange information, discharging one can disambiguate another and lead to its discharge; our declarative semantics has to support this "causality" between syntactically-separate constraints (without allowing self-justification cycles, which has plagued several of our attempts).

Simplifying their specification was also a major reason why the JFP removes local let-generalization (end of page 32):

> In summary, generalisation of local let bindings (without annotations) is a device that is almost never used, and its abolition yields a dramatic simplificationin both the specification and implementation of a typechecker.

Note that we are not covering the same language features and design issues as the OutsideIn(X) work (we do not consider type families, for example), so we are not claiming to subsume their work, but we claim that the difficulty (the non-straightforwardness) is comparable.


## 4. Local comments

### Review A

### Review B

### Review C
