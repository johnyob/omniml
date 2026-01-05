# Omnidirectionality: Theory

{pause}
<div class="vspace-lg"></div>

*Problem*: Defining $\Gamma \vdash e : \tau$ 

{pause}
<div class="vspace-lg"></div>

*Declarative* is better than *algorithmic* for users

{pause}
<div class="vspace-lg"></div>

Hard to achieve!


{pause up}
# Fragility: Static Overloading


{pause}

<div class="vspace-lg"></div>

Fragile (*implicit*) terms $e.\ell$ and robust (*annotated*) terms $e.(\ell \textsf{ of } T)$

{pause}

<div class="vspace-lg"></div>

- *Robust* 
  ```math
  \frac
    {\Gamma \vdash e : \bar\tau_1 \; T \quad \ell \textsf{ of } T \leq \bar\tau_1 \; T \to \tau_2}
    {\Gamma \vdash e.(\ell \textsf{ of } T) : \tau_2}
  ```

{pause}
- *Fragile*
  ```math
  \frac
    {\Gamma \vdash e.(\ell \textsf{ of } T) : \tau}
    {\Gamma \vdash e.\ell : \tau}
  ```

  {pause}
  This rule is natural & declarative 

  {pause}
  Breaks principality, **guessing** the record type $T$ 

  {pause}
  (*e.g.* guessing $T \in \{\texttt{point}, \texttt{gpoint}\}$ for $\textsf{let getx} = \lambda p. p.\textsf{x}$)


{pause up}
# Contextual Rules

{pause}
<div class="vspace-lg"></div>

*Idea*: $\tau$ is known $\triangleq$ $\tau$ is uniquely determined from expression context $\mathscr{E}$

{pause}
<div class="vspace-md"></div>

```
let getx (p : point) = (* hole *)
```

{pause}

<div class="vspace-md"></div>

`p`'s type is *known* to be `point`


{#ctxt-rules-second-example pause}

<div class="vspace-lg"></div>


```
(fun p -> (* hole *)) @@ ({ x = 42; y = 1337 } : point)
```
{pause}

<div class="vspace-md"></div>

`p`'s type is also *known* to be `point`!


{pause up=ctxt-rules-second-example}
<div class="vspace-lg"></div>

<div class="vspace-lg"></div>

<style>
.ctxt-rules-anim mjx-container .reveal-2,
.ctxt-rules-anim mjx-container .reveal-3 { visibility: hidden; }

.ctxt-rules-anim:has(.step-2) mjx-container .reveal-2 { visibility: visible; }
.ctxt-rules-anim:has(.step-3) mjx-container .reveal-3 { visibility: visible; }
</style>

{.ctxt-rules-anim}
> {#ctxt-rules-anim-mathblock}
> ```math
> \frac
>   { \Gamma \vdash \class{reveal-2}{\mathscr{E}[}e.(\ell \textsf{ of } T)\class{reveal-2}{]} : \tau \qquad 
>     \class{reveal-3}{\mathscr{E}[e \triangleright T]} }
>   { \Gamma \vdash \class{reveal-2}{\mathscr{E}[}e.\ell\class{reveal-2}{]} : \tau}
> ```
> 
> {pause exec}
> ```slip-script
> let el = document.querySelector("#ctxt-rules-anim-mathblock")
> slip.setClass(el, "step-2", true)
> ```
> 
> {pause exec}
> ```slip-script
> let el = document.querySelector("#ctxt-rules-anim-mathblock")
> slip.setClass(el, "step-3", true)
> ```

{pause}
<div class="vspace-lg"></div>

We call $\mathscr{E}[e \triangleright T]$ a *unicity* condition

"The expression context $\mathscr{E}$ uniquely determines the shape of $e$'s type to be the record type $T$"



{pause up}
# Unicity

<style>
.unicity-anim mjx-container .reveal-2,
.unicity-anim mjx-container .reveal-3 { visibility: hidden; }

.unicity-anim:has(.step-2) mjx-container .reveal-2 { visibility: visible; }
.unicity-anim:has(.step-3) mjx-container .reveal-3 { visibility: visible; }
</style>


<div class="vspace-lg"></div>

{.unicity-anim}
> {#unicity-anim-mathblock}
> ```math
> \begin{array}{rcl}
> \class{reveal-2}{\mathscr{E}[}e \triangleright T\class{reveal-2}{]} &\quad\triangleq\quad& \forall \; \Gamma, \tau_{\text{ground}}, \tau. \\ 
> && \qquad \Gamma \vdash \class{reveal-3}{\color{fuchsia}\lfloor}\class{reveal-2}{\mathscr{E}[\{\square \textsf{ with }} (e : \tau_{\text{ground}})\class{reveal-2}{\}]} \class{reveal-3}{\color{fuchsia}\rfloor} : \tau \implies \tau_{\text{ground}} = \_ \; T
> \end{array}
> ```


<div class="vspace-lg"></div>

{pause exec}
```slip-script
let el = document.querySelector("#unicity-anim-mathblock")
slip.setClass(el, "step-2", true)
```

- *Typed holes*

  A hole with subterms $\{ \square \textsf{ with } \bar e \}$

  {pause}
  ```math
  \frac
    {\Gamma \vdash \bar e : \bar \tau}
    {\Gamma \vdash \{ \square \textsf{ with } \bar e \} : \tau'}
  ```

{pause exec}
```slip-script
let el = document.querySelector("#unicity-anim-mathblock")
slip.setClass(el, "step-3", true)
```

- *Erasure*

  ${\color{fuchsia}\lfloor} e {\color{fuchsia}\rfloor}$ erases all fragile terms, replacing them with typed holes

  {pause}
  ```math
  \begin{array}{rcl}
  \lfloor e.(\ell \textsf{ of } T) \rfloor &=& \lfloor e \rfloor.(\ell \textsf{ of }  T)\\
  \lfloor e.\ell \rfloor &=& \{ \square \textsf{ with } \lfloor e \rfloor \}
  \end{array}
  ```


  
