This is an umbrella library for dealing with (omega) automata in rust. It contains the following subpackages:
- `automata` introduces the basic types for dealing with transition systems and words. It also contains combinators for manipulating transition systems
- `hoars` implements a parser for the [HOA](https://adl.github.io/hoaf/) format for representing omega automata
- `automata-learning` deals with the inference of automata, defines both passive and active automaton learning schemes and implements some algorithms
- `bin` contains a command line interface to some of the functionality provided in the other packages

## Rust Version
For convenience, the library `automata` uses rust version 1.75 as it enables [RPITIT](https://github.com/rust-lang/rust/pull/115822). This feature greatly simplifies returning iterators (or in general objects implementing a certain trait) from a trait without explicitly giving the returned type. Until this feature is available on stable (which will be on the 28th of December 2023), the library requires using either nightly or a beta version of rust. This is easy to install through `rustup toolchain install beta` and switching to this toolchain can be done by executing `rustup default beta`. Of course, this will no longer be necessary once the changes have landed in master.

# Transition systems and automata
In essence, an automaton consists of a transition system (TS) together with an acceptance component. A TS is simply a finite collection of states (the set of all states is denoted $Q$) which are connected with directed edges. It can have colors on its states (then each state $q \in Q$ is assigned precisely one color) as well as colors on its edges (meaning every edge between two states has a color).

The implementation of TS is generic over the alphabet, which can either be simple (i.e. it is just a collection of individual symbols/chars) or propositional (meaning the alphabet consists of a collection of atomic propositions). Similar to other libraries dealing with (omega) automata, we distinguish between edges and transitions in a TS. Specifically, an edge is determined by its origin/target state, the color it emits and a guard, which is of the expression type that the alphabet determines. A transition on the other hand is a concretization of an edge, where the guard is a single symbol (also determined by the alphabet). For simple alphabets, expressions and symbols coincide, whereas for propositional alphabets, expressions are formulas (represented as BDDs) over the atomic propositions and symbols are satisfying valuations of expressions.

The most important trait is `TransitionSystem`, which provides access to the indices of all states and is capable of returning iterators over the outgoing edges of a state. It also provides a lot of combinators, which allow manipulation of the TS. For example `map_state_color` consumes the TS and relabels the colors on the states through applying a given function. Most combinators consume `self`, returning a new TS, which is mainly to avoid unneccessary cloning of the underlying data structures. If the original TS should continue to exist, call `as_ref` or simply use a reference to the TS.
As each combinator returns an object which again implements `TransitionSystem`, these can easily be chained together without too much overhead. While this is convenient, the applied manipulations are computed on-demand, which may lead to considerable overhead. To circumvent this, it can be beneficial to `collect` the resulting TS into a structure, which then explicitly does all the necessary computations and avoids recomputation at a later point. There are also variants `collect_with_initial`/`collect_ts`, which either take the designated ininital state into account or collect into a specific representation of the TS.

The crate defines some basic building blocks of TS which can easily be manipulated (see `Sproutable`), these are
- `NTS`/`DTS` (the latter is just a thin wrapper around the former). These store edges in a vector, a state contains a pointer to the first edge in this collection and each edge contains pointers to the previous/next one.
- `BTS` which stores transitions in an efficient HashMap

Further traits that are of importance are
- `Pointed` which picks one designated initial state, this is important for deterministic automata
- `Deterministic`, a marker trait that disambiguates between nondeterministic and deterministic TS. As `TransitionSystem` only provides iterators over the outgoing edges, it can be used to deal with nondeterministic TS, that have multiple overlapping edges. By implementing `Deterministic`, we guarantee, that there is always a single unique outgoing transition for each state.
- `Sproutable` enables growing a TS state by state and edge/transition by edge/transition. Naturally, this is only implemented for the basic building blocks, i.e. `BTS`, `DTS` and `NTS`.

### Profiling while Benchmarking
We can profile the benchmark code with `cargo bench --bench forc_paper -- --profile-time 20` where `20` is the time for which the benchmarks will be run. A flamegraph will be generated and placed in `target/criterion/forc_paper/profile/flamegraph.csv`.
