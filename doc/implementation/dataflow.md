# Dataflow

From the program design standpoint, Brittany performes a
`Config -> Text -> Text` transformation; it is not interactive in any way and
it processes the whole input at once (no streaming going on). This makes for
a very simple design with nice separation of IO and non-IO.

Brittany makes heavy usage of mtl-on-steroids-style transformers, mostly
limited to Reader, Writer and State. For this kind of task it makes a lot of
sense; we do a pure transformation involving multiple steps
that each requires certain local state during traversals of recursive data
structures. By using MultiRWS we can even entirely avoid using lens without
inducing too much boilerplate.

Firstly, the topmost layer, the IO bits:

<img src="../../doc-svg-gen/generated/periphery.svg">

The corresponding code is in these modules:

- `Language.Haskell.Brittany.Main`
- `Language.Haskell.Brittany.Internal`

The latter [contains the code to run our Reader/Writer/State stack](../../source/library/Language/Haskell/Brittany/Internal.hs) (well, no state yet).

Note that `MultiRWS` here behaves like a nicer version of a stack like
`ReaderT x (ReaderT y (WriterT w1 (WriterT2 w2 (Writer w3)..)`.
The next graph zooms in on that transformation:

<img src="../../doc-svg-gen/generated/ppm.svg">

Two places (The `BriDoc` generation and the backend) have additional local
state (added to the monadic context).
The following is a very simplified description of the BriDoc generation:

<img src="../../doc-svg-gen/generated/bridocgen.svg">


For the `BriDoc` generation, the relevant modules are
- `Language.Haskell.Brittany.Internal.Layouters.*`
- `Language.Haskell.Brittany.Internal.LayouterBasics`

For the `BriDoc` tree transformations, the relevant modules are
- `Language.Haskell.Brittany.Internal.Transformations.*`

Finally, for the backend, the relevant modules are
- `Language.Haskell.Brittany.Internal.Backend`
- `Language.Haskell.Brittany.Internal.BackendUtils`

