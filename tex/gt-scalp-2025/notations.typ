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
#let magic(sq, ts) = $#sq _ #ts$

#let der = $thick tack.r thick$
#let derr(label) = $thick attach(tack.r, tr: #label) thick$
#let derinf = derr("inf")
#let sat = $thick tack.r.double thick$

#let constraint_gen(t, A) = $attach(br: #A, bracket.l.double #t bracket.r.double)$

#import "@preview/curryst:0.5.1": rule, prooftree
#let infer(..args) = prooftree(min-premise-spacing: 1.5em, rule(..args))
