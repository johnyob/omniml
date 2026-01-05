# Omnidirectionality: Practice

{pause}

<div class="vspace-lg"></div>

*Key idea*: If we don't have the required known type information yet, **wait**.

{pause}


<div class="vspace-lg"></div>

```math
\textsf{match } \alpha \textsf{ with } \overline{\rho \Rightarrow C}
```

{pause}

<div class="vspace-lg"></div>

1. Wait until unification variable $\alpha$ is unified to some non-variable type $\tau_0$

{pause} 
2. $\tau_0$ uniquely matches a 'shape pattern' $\rho_i$ {pause} (*e.g.* $\alpha \to \beta$ or $\_ \; T$) 

{pause}
3. Solve $C_i$ under $\rho_i$'s extended context


{pause} 

<div class="vspace-lg"></div>

If $\alpha$ is never unified, constraint is **false**.


{pause up}
# Example: `bnot`
<div class="vspace-md"></div>


{pause}

{.two-grid-left}
> ```math
> ```
>
> ```ocaml
> let bnot b = if b then False else (True : bval)
> ```
> 
> {pause}
> ```math
> ```
>
> ```math
> {\color{#ff7b72}{(\textsf{if }}} {\color{royalblue}b^\beta} {\color{#ff7b72}{\textsf{ then }}} {\color{fuchsia}\textsf{False}^\gamma} {\color{#ff7b72}{\textsf{ else }}} {\color{greenyellow}(\textsf{True} : \textsf{bval})^\delta}{\color{#ff7b72})^\alpha}
> ```
> 
> {pause}
> ```math
> \overset{[\!\![\; - \mathop{:} \; = \;]\!\!]}{\longmapsto}
> ```
> 
> ```math
> {\color{royalblue}{\beta = \textsf{bool}}} \wedge {\color{fuchsia}{\textsf{match } \gamma \textsf{ with } (\_ \; T \Rightarrow \textsf{False of } T \leq \gamma)}} \wedge {\color{greenyellow}{\delta = \textsf{bval}}}
> \wedge {\color{#ff7b72}{\gamma = \delta = \alpha}}
> ```
> 
> {pause}
> ```math
> \longrightarrow
> ```
>
> ```math
> {\color{royalblue}{\beta = \textsf{bool}}} \wedge {\color{fuchsia}{\textsf{match } \gamma \textsf{ with } (\_ \; T \Rightarrow \textsf{False of } T \leq \gamma)}} 
> \wedge {\color{#ff7b72}{\gamma = \delta = \alpha}} = {\color{greenyellow}\textsf{bval}}
> ```
>
> {pause}
> ```math
> \longrightarrow
> ```
>
> ```math
> {\color{royalblue}{\beta = \textsf{bool}}} \wedge {\color{fuchsia}(\textsf{False of bval} \leq \gamma)}
> \wedge {\color{#ff7b72}{\gamma = \delta = \alpha}} = {\color{greenyellow}\textsf{bval}}
> ```
> 
> {pause}
> ```math
> \longrightarrow
> ```
>
> ```math
> {\color{royalblue}{\beta = \textsf{bool}}} \wedge {\color{fuchsia}\gamma = \textsf{bval}}
> \wedge {\color{#ff7b72}{\gamma = \delta = \alpha}} = {\color{greenyellow}\textsf{bval}}
> ```
> {pause}
> ```math
> \longrightarrow
> ```
>
> ```math
> {\color{royalblue}{\beta = \textsf{bool}}}
> \wedge {\color{#ff7b72}{\gamma = \delta = \alpha}} = {\color{greenyellow}\textsf{bval}}
> ```



<div class="vspace-lg"></div>

{pause up}
# A Recap: `let`

<div class="vspace-lg"></div>

{pause}
Let bindings *generalize* types into polymorphic ones


<div class="vspace-md"></div>

{#recap-let-id pause}
```ocaml
let id = fun x -> x
```

{pause exec}
```slip-script
let el = document.querySelector("#recap-let-id")
slip.setClass(el, "does-compile", true)
```

{.no-background}
```ocaml
val id : 'a -> 'a
```

{pause}


<div class="vspace-lg"></div>

Function bindings are monomorphic

{pause}

<div class="vspace-md"></div>

{#recap-fun-mono}
```ocaml
(fun id -> id 1, id true) (fun x -> x)
```

{pause exec}
```slip-script
let el = document.querySelector("#recap-fun-mono")
slip.setClass(el, "does-not-compile", true)
```

<pre>
| (fun id -> id 1, id true) (fun x -> x)
                      ^^^^

Error: This expression has type bool 
       but an expression was expected of type int
</pre>


{pause up}
# The Troubles of `let`


<div class="vspace-lg"></div>

```math
\frac
    {\Gamma \vdash e_1 : \tau_1 \quad \bar\alpha = \textsf{fv}(\tau_1) \setminus \textsf{fv}(\Gamma) \quad \Gamma, x : \forall \bar\alpha. \tau_1 \vdash e_2 : \tau_2}
    {\Gamma \vdash \textsf{let } x = e_1 \textsf{ in } e_2 : \tau_2}
```

<div class="vspace-md"></div>

{pause}
1. Infer $e_1$'s type $\tau_1$

{pause}
2. Generalize $\tau_1$ to produce $\forall \bar\alpha. \tau_1$

{pause}
3. Infer $e_2$'s type under extended context


{#let-problem pause}

<div class="vspace-lg"></div>

*Problem*: Static order of inference for `let`

What if $e_1$ contains suspended constraints?

<div class="vspace-lg"></div>

{pause up=let-problem}

```ocaml
type point = { x : int; y : int }
type 'a gpoint = { x : 'a; y : 'a }
```

{pause}

{#backprop-ex .does-compile}
```ocaml
let problematic_ex p' = 
  let getx p = p.x in 
  getx ({ x = 42; y = 1337 } : _ gpoint), (getx p' : float)
```

<div class="vspace-md"></div>

{pause}
At the point of generalization:
- `p.x` is suspended

{pause}

- Both `p` and the return type of `getx` may or may not be generalized

<div class="vspace-md"></div>

{pause up=backprop-ex}
*Ideas*: 

{pause}
- Make `let`'s monomorphic? {pause} ❌

  Nope, `getx` is instantiated for `int gpoint` and `float gpoint`

{pause}
- Suspend instantiating `getx`? {pause} ❌

  Nope, *back-propagation*

{pause}
- Partial generalization? {pause} ✅

  1. Start with a partial type scheme of $\forall \alpha, \beta.\; \alpha \to \beta$

  {pause}
  2. Once we unsuspend `p.x`, update all instances of $\alpha$ to be $\gamma \textsf{ gpoint}$ 
     and all instances of $\beta$ to be $\gamma$

{pause up}
# Partial Generalization

{pause}
<div class="vspace-lg"></div>

For each `let` binding: 
{pause}
1. Infer as much about $e_1$ as you can, produce partial type scheme
{pause}
2. Continue inferring $e_2$
{pause}
3. Whenever any suspended constraints in $e_1$ make progress, update all instantiations

{pause}
<div class="vspace-lg"></div>
Hard to implement correctly
