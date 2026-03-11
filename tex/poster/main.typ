#import "@preview/poster-syndrome:0.1.0": *
#import "@preview/cetz:0.4.2": canvas, draw

////////////////////////////////////////////////////////////////////////////////
// Colors
////////////////////////////////////////////////////////////////////////////////


#let blue(content) = text(fill: rgb("#7CC6FE"))[#content]
#let green(content) = text(fill: rgb("#51CF66"))[#content]
#let red(content) = text(fill: rgb("#FF6B6B"))[#content]
#let purple(content) = text(fill: rgb("#BB86FC"))[#content]  // for synthesis mode
#let amber(content) = text(fill: rgb("#FFB86C"))[#content]   // for checking mode

// Colored arrows for bidirectional typing
#let syn = text(fill: rgb("#BB86FC"))[$arrow.r.double$]  // synthesis
#let chk = text(fill: rgb("#FFB86C"))[$arrow.l.double$]  // checking


////////////////////////////////////////////////////////////////////////////////
// Poster setup
////////////////////////////////////////////////////////////////////////////////


#let custom-theme = theme-helper(
  palette: (
    base: rgb("#0F1115"), // overall dark base
    fg: rgb("#E8ECF1"), // main foreground text
    bg: rgb("#0F1115"), // page background
    highlight: rgb("#7CC6FE"), // cool accent blue
    contrast: rgb("#1A2230"), // panel / box background
  ),
  fonts: (
    base: "Libertinus Sans",
    raw: "Fira Code",
    math: "New Computer Modern Math",
  ),
  overrides: (
    par: (
      // default: (justify: false),
      outlook: (justify: false),
    ),
    text: (
      default: (fill: rgb("#E8ECF1"), size: 20pt),
      title: (
        font: "Libertinus Sans",
        size: 48pt,
        weight: 200,
        tracking: 2pt,
        fill: rgb("#F5F7FB"),
      ),
    ),
  ),
)

#let container = (
  x: 0,
  y: 0,
  width: 594,
  height: 420,
)

#let frames = (
  // Top header - title takes 2/3, authors/affiliation takes 1/3
  title: (x: 15, y: 15, width: 375, height: 35),
  authors: (x: 400, y: 18, width: 174, height: 35),
  // Three columns below header
  // Column 1: Introduction (left)
  introduction: (x: 15, y: 52, width: 185, height: 40),
  overloading: (x: 15, y: 175, width: 185, height: 325),
  // Column 2: Description + Methods (middle)
  bidir: (x: 210, y: 52, width: 185, height: 183),
  constraints: (x: 210, y: 242, width: 185, height: 155),
  // Column 3: Illustration + Outlook (right)
  omni: (x: 405, y: 52, width: 174, height: 340),
  contributions: (x: 405, y: 380, width: 174, height: 18),
  // Unused (needed by poster-syndrome)
  cover-image: (x: 0, y: 0, width: 0, height: 0),
  subtitle: (x: 0, y: 0, width: 0, height: 0),
  details: (x: 0, y: 0, width: 0, height: 0),
)

// initialise with defaults
#let (poster, frame) = poster-syndrome-setup(
  theme: custom-theme,
  frames: frames,
  container: container,
)

#let page-background = {
  let padding = 0.4cm

  let page-base = rgb("#0F1115")
  let constraints-fill = rgb("#151D29")
  let soft-line = rgb("#2B3A52")

  // base background
  place(
    rect(
      width: 100%,
      height: 100%,
      fill: page-base,
    ),
  )

  place(
    left + top,
    dx: frames.constraints.x * 1mm - padding,
    dy: frames.constraints.y * 1mm - padding,
    rect(
      width: frames.constraints.width * 1mm + 2 * padding,
      height: frames.constraints.height * 1mm + 2 * padding,
      fill: constraints-fill,
      stroke: soft-line,
      radius: 3mm,
    ),
  )
}


#let page-foreground(bleed: 8.5pt, trim: 29.5pt, frames: none) = {
  // outer
  crop-marks(
    distance: trim - bleed,
    offset: 0pt,
    length: trim - bleed - 0pt,
    stroke: .5pt + black,
  )
  // inner with outline
  crop-marks(distance: trim, length: trim + 2pt, stroke: 2pt + white)
  crop-marks(
    distance: trim,
    length: trim + 2pt,
    stroke: 0.5pt + black,
  )
}

#show: poster.with(
  title: text[Omnidirectional type inference for ML],
  subtitle: text[],
  authors: text[],
  affiliation: text[],
  qr-code: none,
  date: "",
  cover-image: none,
  credit: "",
  foreground: page-foreground(frames: frames), // show boxes with frames: _default-frames
  background: page-background,
)

////////////////////////////////////////////////////////////////////////////////
// Notations
////////////////////////////////////////////////////////////////////////////////

#let keyword(kwd) = text(weight: "bold")[#kwd]
#let sh = $sigma.alt$
#let efun(x, e) = $keyword("fun") #x arrow #e$
#let exfun(x, t, e) = $keyword("fun") (#x : #t) arrow #e$
#let etfun(tv, e) = {
  $keyword("fun") thin (#keyword("type") #tv) arrow #e$
}
#let elet(x, e1, e2) = {
  $keyword("let") thin #x = #e1 thin keyword("in") #e2$
}
#let eapp(e1, e2) = $#e1 med #e2$
#let eannot(e, t) = $(#e : #t)$
#let eproj(e, l) = $#e.#l$
#let hole = $square$
#let ty = $tau$
#let ts = $sigma$
#let tv = $alpha$
#let tva = $alpha$
#let tvb = $beta$
#let tvc = $gamma$
#let tarr = $arrow$
#let tapp(T, ts) = $#T med #ts$
#let tfor(alpha, A) = $forall #alpha. thin #A$
#let ctx = $Gamma$
#let csolve = $arrow.r.long$
#let cinfer(e, t) = $bracket.l.stroked #e : #t bracket.r.stroked$
#let cexists(..args) = {
  let args-array = args.pos()
  let c = args-array.last()
  let tvs = args-array.slice(0, -1)
  let tvs-display = tvs.join($,$)
  $exists #tvs-display. thin #c$
}
#let ceq = $=$
#let cand = $and$
#let cmatch(t, branches) = {
  $keyword("match") thin #t thin keyword("with") #thin #branches$
}
#let trans(R) = $scripts(#R)^*$
#let eqdef = $eq.delta$

#let cctx = $cal(C)$
#let ectx = $cal(E)$

#let eg = text[e.g. #h(1mm)]

////////////////////////////////////////////////////////////////////////////////
// Programs
////////////////////////////////////////////////////////////////////////////////

#let code-with-status(status: none, body) = {
  let symbol = if status == "ok" {
    green[✓]
  } else if status == "error" {
    red[✗]
  } else {
    h(0.8em) // spacing when no symbol
  }

  grid(
    columns: (1fr, auto),
    column-gutter: 8pt,
    align: (left, left + horizon),
    body, symbol,
  )
}

#let ocaml-code-block(lang: "OCaml", lang-color: rgb("#EE6A1A"), body) = {
  block(width: 100%)[
    #rect(
      fill: rgb("#151D29"),
      stroke: rgb("#2B3A52"),
      radius: 2mm,
      inset: 12pt,
      width: 100%,
    )[
      #place(
        top + right,
        dx: 15pt,
        dy: -22pt,
      )[
        #rect(
          fill: lang-color,
          radius: 0pt,
          inset: (x: 10pt, y: 5pt),
        )[
          #text(size: 11pt, weight: "bold", fill: white)[#lang]
        ]
      ]
      #set text(size: 19pt)
      #body
    ]
  ]
}


// Helper function for comparison tables
// Pass true for green checkmark, false for red X
#let comparison-table(
  header-fill: rgb("#151D29"),
  table-fill: none,
  efficient,
  expressive,
  no-fixed-order,
) = {
  let checkmark(is-good) = if is-good {
    green[✓]
  } else {
    red[✗]
  }

  let header(label, is-good) = if is-good {
    green[*#label*]
  } else {
    red[*#label*]
  }

  table(
    columns: (1fr, 1fr, 1fr),
    stroke: rgb("#2B3A52"),
    fill: (col, row) => if row == 0 { header-fill } else { table-fill },
    align: center + horizon,
    inset: 8pt,

    header("Efficient", efficient),
    header("Expressive", expressive),
    header("Dynamic order", no-fixed-order),
    checkmark(efficient), checkmark(expressive), checkmark(no-fixed-order),
  )
}

#frame(tag: "authors")[
  #set text(size: 20pt)
  #grid(
    columns: (1fr, auto),
    column-gutter: 12pt,
    align: (left, center),
    [
      Alistair O'Brien#super[1], Didier Rémy#super[2], Gabriel Scherer#super[2]

      #text(
        size: 14pt,
        fill: custom-theme.palette.highlight.darken(40%),
        weight: "regular",
        features: (smcp: 1, c2sc: 1, onum: 1),
      )[
        #super[1]University of Cambridge, #super[2]Inria Paris
      ]
    ],
    place(dx: -25mm, dy: -5mm)[#image("qr-code.svg", width: 30mm)],
  )
]


#frame(tag: "introduction")[
  #set par(justify: true)
  #set text(size: 20pt)

  == Principality

  At the heart of ML's type system lies the ability to #blue[*guess*] types:

  $
    (ctx, x : ty_1 tack e : ty_2) / (ctx tack efun(x, e) : ty_1 tarr ty_2)
  $

  The parameter type $ty_1$ is #blue[*guessed*], then constrained by
  the body~$e$.

  #blue[*Principality:*] Every well-typed expression $e$ admits a
  #blue[*principal type*] $ts$---a most general type from which all other valid
  types are instances. Example: $efun(x, x)$ has principal type
  $tfor(tv, tv tarr tv)$.

  Principality $arrow.r.double$ local typing decisions are always
  #green[*optimal*] without backtracking (#green[*very efficient inference!*]).
]

#frame(tag: "overloading")[
  #set par(justify: true)
  #set text(size: 20pt)

  == Static Overloading

  OCaml has overloaded record fields:
  #ocaml-code-block[
    ```ocaml
    type point = { x : int; y : int }
    type 'a gpoint = { x : 'a ; y : 'a }
    ```
    #code-with-status(status: "error")[```ocaml
    let getx p = p.x
    ```]
  ]

  Fields `x` and `y` are #emph[statically overloaded].

  Which type should we infer for `getx`?

  #[
    #set align(center)
    ```ocaml
    val getx : point -> int
    ```
    #emph[or]
    ```ocaml
    val getx : 'a gpoint -> 'a
    ```
  ]

  #red[*Problem*:] Neither is more general than the other---#red[*principality is violated!*]

  To resolve overloading, the type of `p` must be #emph[known]:
  #ocaml-code-block[
    #code-with-status(status: "ok")[```ocaml
    let getx p = (p : point).x
    ```]
  ]

  #red[*Problem*:] Annotations at every projection are cumbersome!

  Inference algorithms must propagate #blue[*known type information*] to
  disambiguate projections.
]

#let infer-rule-name(name) = [
  #set text(size: 15pt)
  #show smallcaps: set text(font: "Latin Modern Roman Caps")
  #smallcaps(name)
]


#frame(tag: "bidir")[
  #set par(justify: true)
  #set text(size: 20pt)

  == Bidirectional typechecking

  Bidirectional typing uses two modes to propagate types:
  - #purple[*Synthesis mode*] ($#syn$): infer type from expression
  - #amber[*Checking mode*] ($#chk$): check against expected type

  #v(7mm)

  $
    (#place(dx: 0mm, dy: -11mm)[#infer-rule-name[App]] ctx tack e_1 #syn ty_1 tarr ty_2 quad ctx tack e_2 #chk ty_1)
    /
    (ctx tack eapp(e_1, e_2) #syn ty_2)
    #h(2cm)
    (#place(dx: -8mm, dy: -11mm)[#infer-rule-name[Annot]]
    ctx tack e #chk ty)
    /
    (ctx tack eannot(e, ty) #syn ty)
  $

  #v(5mm)
  $
    (#place(dx: 0mm, dy: -11mm)[#infer-rule-name[Proj]]
    ctx tack e #syn tapp(T, overline(ty))
    quad ell : tapp(T, overline(tau)) tarr ty')
    /
    (ctx tack eproj(e, ell) #syn ty')
  $

  #blue[*$ty$ is known*] $eqdef$ $#syn ty$ in premise or $#chk ty$ in
  conclusion.



  #ocaml-code-block[
    #code-with-status(status: "error")[```ocaml
    (fun p -> p.x) ({ x = 42; y = 1337 } : point)
    ```]
  ]

  #red[*Problem*:] Fixed order in #infer-rule-name[App] $=>$ annotation
  `(_ : point)` arrives #red[*too late*] to help synthesize `p.x`.


  #comparison-table(true, true, false)
]

#frame(tag: "constraints")[
  #set par(justify: true)
  #set text(size: 20pt)

  == Constraint-based inference

  #columns(2, gutter: 12pt)[
    $
      C ::= ty ceq ty' bar C cand C' bar dots
    $

    //      $
    //        (phi(ty) = phi(ty')) / (phi tack ty ceq ty')
    //        quad
    //        (phi tack C quad phi tack C') / (phi tack C cand C')
    //      $
    //
    Constraint generation: generate $cinfer(e, ty)$, a constraint that is
    satisfiable $<==>$ $e$ has the type $ty$.

    Constraint solving: efficient algorithms (#eg Tarjan's unification,
    Rémy's rank-based generalization); #green[*modular*] solving.

    #red[*Problem:*] Cannot express whether the solution $delta |-> ty'$
    is known or guessed.

    #align(center)[
      #canvas({
        import draw

        let box-w = 7.5
        let box-h = 1.8
        let gap = 1.25
        let radius = 0.15
        let label-size = 0.7

        // Helper to draw a labeled box with badge
        let labeled-box(
          y,
          bg-color,
          badge-color,
          badge-label,
          content,
        ) = {
          // Main rounded box
          draw.rect(
            (0, y),
            (box-w, y + box-h),
            fill: bg-color,
            stroke: none,
            radius: radius,
          )
          // Badge in top-right corner
          draw.rect(
            (box-w - label-size, y + box-h - label-size),
            (box-w, y + box-h),
            fill: badge-color,
            stroke: none,
          )
          draw.content(
            (box-w - label-size / 2, y + box-h - label-size / 2),
            text(
              size: 9pt,
              fill: white,
              weight: "bold",
            )[#badge-label],
          )
          // Main content centered
          draw.content((box-w / 2, y + box-h / 2), text(
            fill: black,
          )[#content])
        }

        // Helper to draw a vertical arrow with label
        let labeled-arrow(y, label) = {
          draw.line(
            (box-w / 2, y - 0.1),
            (box-w / 2, y - gap + 0.1),
            stroke: rgb("#888888") + 2.5pt,
            mark: (end: "straight"),
          )
          draw.content(
            (box-w / 2 + 1.2, y - gap / 2),
            anchor: "west",
            text(
              size: 10pt,
              fill: rgb("#888888"),
            )[#label],
          )
        }

        let y1 = 10
        let y2 = y1 - box-h - gap
        let y3 = y2 - box-h - gap
        let y4 = y3 - box-h - gap

        // Box 1: e
        labeled-box(
          y1,
          rgb("#E6F2FF"),
          rgb("#4682B4"),
          $e$,
          $efun(x, e)$,
        )
        labeled-arrow(y1, [Generate $#cinfer([-], ty)$])

        // Box 2: C
        labeled-box(
          y2,
          rgb("#FFF0E6"),
          rgb("#B45F32"),
          $C$,
          $cexists(tva, tvb, ty ceq tva tarr tvb cand dots)$,
        )
        labeled-arrow(y2, [Solve $C trans(csolve) hat(C)$])

        // Box 3: Ĉ
        labeled-box(
          y3,
          rgb("#E6FFEB"),
          rgb("#3C9646"),
          $hat(C)$,
          $cexists(tvc, ty ceq tvc tarr tvc)$,
        )
        labeled-arrow(y3, [Elaborate to System F])

        // Box 4: e:τ
        labeled-box(
          y4,
          rgb("#F5E6FF"),
          rgb("#8246A0"),
          $e_(: tau)$,
          $Lambda tvc. thin lambda (x : tvc). thin x$,
        )
      })
    ]

    #colbreak()


  ]
  #v(8pt)

  #comparison-table(table-fill: rgb("#0F1115"), true, false, true)
]

#frame(tag: "omni")[
  #set text(size: 20pt)
  #set par(justify: true)

  == Solution: Omnidirectional inference

  Key idea: add #blue[*suspended match constraints*].

  $
    cmatch(tv, (rho_1 => C_1 bar dots bar rho_n => C_n))
  $

  #blue[*Waits until $tv$ is unified*] with some non-variable $ty_0$, then:
  - match $ty_0$ against a unique #emph[pattern] $rho_i$ (#eg $tapp(c, \_) thin$
    for record types, binding the constructor name $c$),
  - solve $C_i$ in extended environment.

  Otherwise #blue[*if $tv$ is never unified, fail*].

  Implemented using #emph[wait lists] and a scheduler (#green[*efficient*]).


  #comparison-table(true, true, true)

  === Challenge: Let-polymorphism

  $
    (ctx tack e_1 : ty_1
    quad overline(tv) = "ftv"(ty_1) backslash "ftv"(ctx)
    quad ctx, x : tfor(overline(tv), ty_1) tack e_2 : ty_2)
    /
    (ctx tack elet(x, e_1, e_2) : ty_2)
  $

  Free type variables $overline(tv)$ in $ty_1$ are #blue[*generalized*],
  giving $x$ a #emph[polymorphic] type.

  What if $e_1$ has suspended constraints?

  #ocaml-code-block(lang: "OmniML")[
    #set text(size: 18.4pt)
    ```ocaml let getx p = p.x in```
    #v(-3mm)
    #code-with-status(
      status: "ok",
    )[```ocaml(getx q : float), getx ({x=42; y=0} : int gpoint)```]
  ]

  At generalization time, the constraint for `p.x` is suspended.
  `getx`'s argument and result variables $tv, tvb$ are #blue[*guarded*].

  === Solution: Incremental Instantiation
  - Generalize known parts; treat guarded variables as polymorphic
  - Partial type scheme: $tfor(tva\, tvb, tva tarr tvb bar cmatch(tva, dots))$
  - When suspended constraints resolve, refine all instances

]

#frame(tag: "contributions")[
  #set text(size: 20pt)
  #set par(justify: true)

  #text(fill: custom-theme.palette.highlight.darken(40%))[*Contributions:*]
  General framework for other features
  (#eg first-class polymorphism) • Sound, complete & principal inference
  algorithm • Prototype implementation]
