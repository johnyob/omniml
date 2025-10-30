#import "@preview/touying:0.6.1": *
#import themes.metropolis: *

#show: metropolis-theme.with(
  aspect-ratio: "16-9",
  footer: self => self.info.institution,
  config-info(
    title: [Inférence de types dans tous les sens],
    author: [
      #let face(name) = [
        #image("pictures/" + name + ".jpg", height:10em)
        #v(0.6em)
      ]
      #grid(columns: (1fr,) * 3,
        face("Alistair_OBrien"),
        face("Didier_Remy"),
        face("Gabriel_Scherer"),
        [Alistair O'Brien],
        [Didier Remy],
        [Gabriel Scherer (speaker)],
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

#import "./notations.typ": *


= Contexte: désambiguation \ #h1 par les types en OCaml

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

== Constructions désambiguées

Désambiguation par les types:

- #pause ```ocaml Printf.printf "%d: %s"```: `string` ou `format`? (depuis toujours)
- #pause ```ocaml Foo, x.foo```: constructeurs et champs (depuis 4.01 (2013))
- #pause ```ocaml let (~x, ..) = point in x+1```: $n$-uplets étiquetés (depuis 5.4)
- #pause ```ocaml ([| 0. |] : _ Iarray.t), ([| 0. |] : floatarray)```: tableaux litéraux (depuis 5.4)

== Erreurs

#v1

Erreurs ou avertissements quand le type n'est pas *(bien) connu*:

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

== Bien connu ?

#v1

Deux mécanismes complémentaires

1. Propagation syntaxique des annotations (inférence bidirectionnelle)

   ```ocaml
   let rec eval : formula -> boolean = function
     | True -> True | [...]
   ```

#v1

2. Propagation des définitions polymorphes à leur usage (inférence Hindley-Milner):

   ```ocaml
   let point = (~x:41, ~y:31) in
   let (~x, ..) = point in [...];;
   ```

#v1
  
pas 3. L'unification ne suffit pas.

```ocaml
  if b then (True : boolean) else False (* warns *)
  if b then True else (False : boolean) (* fails *)
```

#v1

== Problème: on veut toujours plus

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


= Pratique: \ #h1 inférence omni-directionnelle

== Omni-directionelle ?

#v1

Si manque d'information pour désambiguer, on _attend_.

#v1

```ocaml
  if b then True else (False : boolean)
```

#v1

Nous proposes des contraintes *suspendues*
\ qui sont *déchargées* quand l'information devient disponible.

$ matchwith(alpha, sh, C) $

#v1#pause

Si une contrainte est toujours suspendue à la fin,
elle *échoue*.

#v1

== Génération de contraintes suspendues

#v1

#let condcolor = red
#let testcolor = condcolor
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

== Difficulté: généralisation des `let`

#v1

Règle simplifiée pour la généralisation des `let`.

$
    #infer($Gamma der letin(x, t, u) : B$,
           $Gamma der t : A[overline(alpha)]$,
           $quad overline(alpha) in.not Gamma$,
           $quad Gamma, bind(x, forall overline(alpha). A[overline(alpha)]) der u : B$)
$

#alternatives[Les variables restées indéterminées deviennent polymorphe.][Mais si $t$ contient des contraintes suspendues ?]

#v1#pause#pause

#grid(columns: (1fr, 0.4fr, 1fr),
[
Cas *facile* (avec Olivier Martinot):

$(matchwith(beta, sh, C))$ \ ne mentionne aucun des $overline(alpha)$

on garde la contrainte suspendue
], [],
[ #pause
Cas *difficile* (ce travail):

$(matchwith(beta, sh, C))$ \
mentionne certains $overline(alpha)$

comment généraliser ?
]
)
#v1

== Solution

#v1

Cas *difficile*: $(matchwith(beta, sh, C))$
mentionne certains $overline(alpha)$

#v1

#grid(columns: (1fr, 1fr),
[ 
```ocaml
let myfun li =
  let add y = (Foo y) :: li in
  [...]
```
],[
Situation: ```ocaml Foo y``` est suspendue sur `li`; \
`y` est généralisable ou pas
])

#v1#pause

Idée: on continue à explorer `[...]` pour inférer `li`. Que faire quand on voit `add`?

#v1#pause

Peut-on juste suspendre `add`?

#pause Nope: #h1 ```ocaml ((add [...]) : foo list) ``` #h1

#v1#pause

Instantiation et généralisation *incrémentales*:
- schéma polymorphe partiel: $forall alpha beta . quad alpha -> beta "list"$
- quand `Foo` est déchargée, on met à jour toutes les instances de $alpha$

#v1#pause

#h1 Ça marche même quand $beta in overline(alpha)$: *back-propagation*.

#v1

== C'est facile!

#v1

Dans chaque région de généralisation:
1. on infère autant qu'on peut
   #h1 $==>$ schéma polymorphe partiel
2. on continue après la définition
   #h1 $==>$ instances partielles
3. si on décharge un truc, on met à jour les instances
   #h1 $==>$ instantiation incrémentale
4. s'il reste une contrainte suspendue à la fin,
   #h1 échec

#v1

Difficile à implémenter.
#pause
Je n'ai pas réussi, Alistair l'a fait !

#v1

Un prototype efficace en pratique:
#link("https://github.com/johnyob/mlsus/")

#v1

= Théorie: \ #h1 conditions d'unicité

== Règles de typage déclaratives

#v1

$
  Gamma der t : A
  quad<==quad
  Gamma derinf t : A
$

#v1

#grid(columns: 3, gutter: 1em,
[$Gamma der t : A$], [$quad$], [le système de typage de référence],
[$Gamma derinf t : A$], [], [un sous-ensemble _inférrable_ mais _déclaratif_],
[$(exists A. thin Gamma derinf t : A)$], [], [décidable]
)

#v1

_declaratif_ est mieux qu'_algorithmique_ pour l'utilisateurice.

#v1

Difficile à trouver !

#v1

== Constructions fragiles

#v1

Constructions fragiles:
forme *explicite* (annotée) $(app(K^d, overline(t)))$
ou *implicite* $(app(K^?, overline(t)))$.

#v1

Explicite:
#h1 $//
  #infer($Gamma derinf app(K^d, overline(t)) : tycon(d, overline(A))$,
    $Gamma derinf K : overline(B) -> app(overline(A), d)$,
    $Gamma derinf overline(t) : overline(B)$
  )
$ #h1
#pause

#v1

Implicite ?
#h1 $//
  #infer($Gamma der app(K^?, overline(t)) : tycon(d, overline(A))$,
    $Gamma der K : overline(B) -> app(overline(A), d)$,
    $Gamma der overline(t) : overline(B)$
  )
$
#h1

Règle naturelle, déclarative,
mais elle *devine* le constructeur $d$. \
(Backtracking + non principal.)

#v1

Wanted: une règle implicite inférable.

#v1

== Solution: règles contextuelles + unicité

#v1

Implicite, contextuelle
#h1 $//
  #infer($Gamma derinf ctx[app(K^?, overline(t))] : C$,
    $unicityH(ctx, tycon(d, "_"), overline(t))$,
    $Gamma derinf ctx[app(K^d, overline(t))] : C$
  )
$
#h1

#v1

$unicityH(ctx, tycon(d, "_"), overline(t))$: condition d'*unicité* \ Le contexte $ctx[square]$ et les sous-termes $overline(t)$ _déterminent uniquement_ la forme de $hole$, qui est $(tycon(d, "_"))$.

#v1

"Bien connu" := "déterminé uniquement".

#v1

== Comment définir l'unicité ?

#v1

#grid(columns: (2fr, 1fr),
[
Voilà la construction de _trou à sous-termes_ $magic(square, overline(t))$. \ Les $overline(B)$ sont indéterminés.
], [#h1
$ #infer($Gamma derinf magic(square, overline(t)) : A$,
         $Gamma derinf overline(t) : overline(B)$) $
])

#v1#pause

#grid(columns: (2fr, 1fr),
[On peut _effacer_ les constructions fragiles],[#h1
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
[Le bouquet final !],[#h1#pause
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

Nous proposons l'inférence de type omni-directionelle:

- contraintes suspendues
- généralisation et instantiation incrémentales
- pas facile à implémenter
- pas facile à spécifier: conditions d'unicité

#v1

Prototype: #link("https://github.com/johnyob/mlsus/")

Brouillon: #link("https://www.ajo41.dev/papers/suspended-final.pdf")

#v1

#[
#set text(size: 30pt)
#h1 Merci! #h1 Des questions? #h1
]

#v1

// = References

// #bibliography("../suspended.bib", title: none, style: "./gasche-author-date.csl")
