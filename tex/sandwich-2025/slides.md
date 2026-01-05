# Omnidirectional Type Inference for ML
## *Principality any way*

{pause}

<div class="faces">
  <figure class="face">
    <img
      src="https://www.cst.cam.ac.uk/sites/default/files/images/profile/image_2.png"
      class="avatar"
    />
    <figcaption>
      <div class="author">Alistair O’Brien</div>
      <div class="affiliation">University of Cambridge</div>
    </figcaption>
  </figure>

  <figure class="face">
    <img
      src="https://alliance.seas.upenn.edu/~bcpierce/cgi-bin/photos/2002Jan10/img0018-screen.jpg"
      class="avatar"
    />
    <figcaption>
      <div class="author">Didier Remy</div>
      <div class="affiliation">Inria, Paris</div>
    </figcaption>
  </figure>

  <figure class="face">
    <img
      src="https://gallium.inria.fr/~scherer/Gabriel_Scherer.jpg"
      class="avatar"
    />
    <figcaption>
      <div class="author">Gabriel Scherer</div>
      <div class="affiliation">Inria &amp; IRIF, Paris</div>
    </figcaption>
  </figure>
</div>

### _Cambium Seminar, 2025_

{pause up}
# Omnidirectionality Provides

{pause}

{#omni-provides-ans}
## *Principal* type inference for *fragile* features

{pause up=omni-provides-ans}

- **Type inference**: Given a (mostly) unannotated program $e$, compute a type $\tau$ such that $e : \tau$

  {pause}
  ```ocaml
  let id = fun x -> x
  ```

  {pause}

  {.no-background}
  ```ocaml
  val id : 'a -> 'a
  ```


{pause}

- **Principal**: Infer the *most general* type $\tau_p$ of $e$, from which all other valid types are instances. 
    
  {pause}

  *i.e.* OCaml does not infer 

  {.no-background}
  ```ocaml
  val id : int -> int
  val id : bool -> bool
  ```

  {pause}

  But instead infers $\forall \alpha. \; \alpha \to \alpha$

  {pause}
  ```math  
  \begin{array}{rcl}
    \forall \alpha. \; \alpha \to \alpha &\leq& \texttt{int} \to \texttt{int} \\ 
    \forall \alpha. \; \alpha \to \alpha &\leq& \texttt{bool} \to \texttt{bool}
  \end{array}


{pause}

- **Fragile**: Features that naively break principality 

  {pause}

  *i.e.* Static overloading, Higher-rank polymorphism, GADTs, etc


{pause up}
# Fragility: Static Overloading

<div class="vspace-md"></div>

{#point-def}
```ocaml
type point = { x : int; y : int }
```

{pause exec}
```slip-script
let el = document.querySelector("#point-def")
slip.setClass(el, "does-compile", true)
```


{pause}

<div class="vspace-md"></div>

{#gray-point-def}
```ocaml
type 'a gpoint = { x : 'a; y : 'a }
```

{pause exec}
```slip-script
let el = document.querySelector("#gray-point-def")
slip.setClass(el, "does-compile", true)
```


{pause}

<div class="vspace-md"></div>

{#getx-ambiguous-name}
```ocaml
let getx p = p.x
```

{pause exec}
```slip-script
let el = document.querySelector("#getx-ambiguous-name")
slip.setClass(el, "does-not-compile", true)
```

<pre>

  | let getx p = p.x
                 ^^^
  Error 41 [ambiguous-name]: 
  x belongs to several types: gpoint point
</pre>

{pause}

<div class="vspace-md"></div>

Rejected because there is no *principal* type for `getx`

{.no-background}
```ocaml
val getx : point -> int
val getx : 'a gpoint -> 'a
```

{pause up}
# Fragility: In OCaml

{pause}
- *Format string overloading*
  ```ocaml
  Printf.printf "%d: %s" 
  (*            ^^^^^^^^ Infer a `string` or a `format`?                *)
  ```
  Since: before I was born

{pause}
- *GADTs*
  ```ocaml
  type ('a, 'b) eq = Refl : ('a, 'a) eq
  let conv (type a b) (w : (a, b) eq) (x : a) = 
    match w with Refl -> x
  (*                     ^ Infer `a` or `b`?                            *)
  ```
  Since: 4.00 (2012)


{pause}
- *Static overloading of record fields and datatype constructors*
  ```ocaml
  let getx p = p.x
  (*       ^ Infer `point` or `gpoint`?                                *)
  ```
  Since: 4.01 (2013)


{pause center}
- *Labelled tuples* 
  ```ocaml 
  let (~x, ..) = point in x + 1
  (*  ^^^^^^^^ Infer `x:int * int` or `x:int * int * string` or ...?   *)
  ```

  Since: 5.4 (2025)

{pause bottom}
- *Polymorphic parameters*
  ```ocaml
  let self f = f f
  (*           ^ Infer `'a. 'a -> 'a` or `'a. 'a -> 'a -> 'a` or ...?   *)
  ```

  Since: 5.5 (2025)

{pause up}
# Robustness

<div class="vspace-lg"></div>

{pause}

Each fragile term (*e.g.* `p.x`) has a *robust* annotated counterpart (*e.g.* `p.(x of point)`)

{pause}

<div class="vspace-lg"></div>

{.does-compile}
```ocaml
let getx p = p.(x of point)
```

{pause} 

<div class="vspace-lg"></div>

*Robust* terms naively have principal type inference!

{pause}

<div class="vspace-lg"></div>

{#getx-annotated .maybe-compile}
```ocaml
let getx (p : point) = p.x
```


{pause exec}
```slip-script
let el = document.querySelector("#getx-annotated")
slip.setClass(el, "maybe-compile", false)
slip.setClass(el, "does-compile", true)
```


<div>
<svg style="margin-top: -70px; margin-left: 260px" class="arrow-svg" width="140" height="40" viewBox="0 0 140 40" aria-hidden="true"> 
    <path d="M20,20 Q80,50 138,20" fill="none" stroke="white" stroke-width="2" stroke-linecap="round"/> 
    <polygon points="140,20 131,18 134,27" fill="white"/>
</svg> 
<div class="arrow-label"><code>p</code> has the <em>known</em> type <code>point</code></div>
</div>

{pause}
<div class="vspace-md"></div>

{.does-compile}
```ocaml
~> let getx (p : point) = p.(x of point)
```

{pause up}
# Known Types

Fragile inference = type propagation of *known* types + inference of robust elaboration

{pause}
- *Static overloading*:

  ```ocaml
  let getx (p : point) = p.x
  ~> let getx p = p.(x of point)
  ```
 

{pause}
- *Higher-rank polymorphism*:
 
  ```ocaml
  let self (f : 'a. 'a -> 'a) = f f
  ~> let self f = (f : 'a. 'a -> 'a) f
  ```

{pause}
- *GADTs*:
  ```ocaml
  let conv (type a) (w : (a, b) eq) (x : a) : b =
    match w with Refl -> x
  ~>
  let conv (type a) w x =
    match (w : (a, b) eq) returns b with Refl -> (x : a)
  ```

  
{pause up}
# Propagation
{pause}

OCaml uses two directional mechanisms:
{pause}
1. Bidirectional propagation of *known* type information.
    
   {pause}
   {.does-not-compile}
   ```ocaml
   (fun p -> p.x) @@ ({ x = 42; y = 1337 } : point)
   ```

   {pause}

   {.does-compile}
   ```ocaml
   ({ x = 42; y = 1337 } : point) |> (fun p -> p.x) 
   ```
    <svg style="margin-left: 410px" class="arrow-svg" width="200" height="40" viewBox="0 0 140 40" aria-hidden="true"> 
        <path d="M0,20 Q80,50 188,20" fill="none" stroke="white" stroke-width="2" stroke-linecap="round"/> 
        <polygon points="190,20 181,18 184,27" fill="white"/>
    </svg> 

   {pause}
   *Order*: *static*

{pause}
2. Propagation from `let`-definitions to their uses ($\pi$-directional)

   {pause}
   {.does-compile}
   ```ocaml
   let point (* : x:int * int *) = (~x:42, 1337) 
   let (~x, ..) = point
   ```

   {pause}
   *Order*: *static*

<div class="vspace-lg"></div>

{#problem-point pause center}
Unification is not sufficient

{.does-not-compile}
```ocaml
type bval = True | False
type bexpr = True | False | And of bexpr * bexpr | ...

let bnot b = if b then False else (True : bval)
```

<pre>
  | let bnot b = if b then False else (True : bval)
                           ^^^^^
  Error 18 [not-principal]: 
  This type-based constructor disambiguation is not principal.
</pre>
  
{pause up=problem-point}


<div class="vspace-lg"></div>

{.remark title=Problem}
Users always want unification to work


{pause}
<div class="vspace-lg"></div>

*Key idea*: Use a *dynamic* order 

{pause}

{style="display: flex; position:relative"}
> {#part1 include src="practice.md" slip}
> 
> {up=problem-point}
> 
> {#part2 include src="theory.md" slip enter}
>
> {pause}

{pause up}
# Conclusion

{pause}
<div class="vspace-lg"></div>

Omnidirectionality provides principal inference any way, {pause} sanity not guaranteed.

{pause}

<div class="vspace-lg"></div>

Preprint: [https://arxiv.org/abs/2004.00396](https://arxiv.org/abs/2004.00396)

- Principal and declarative specification for many other features 

  (*e.g.* overloading, polytypes, and more)
- Sound and complete constraint-based inference algorithm 

  (using suspended constraints)
- A formalized efficient constraint solver 

  (using partial generalization)
- An efficient in-practice implementation

{pause}
<div class="vspace-lg"></div>

Thanks for listening!

Questions? 

