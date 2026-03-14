# Demand-Driven Interprocedural Recovery

## Goal

Avoid whole-binary lifting and analysis when only a small reachable slice matters.

The intended strategy is:

1. start from an entry function such as `main`
2. lift and analyze only that function
3. execute symbolically just enough to reach call sites
4. resolve direct and indirect calls as far as possible
5. suspend caller state at the call boundary
6. analyze the callee on demand
7. summarize the callee
8. resume the caller with the callee summary
9. repeat until the reachable slice reaches a fixpoint

This gives a demand-driven reachable-function analysis instead of a whole-program one.

## Why It Fits BinRevX

`BinRevX` already has the right ingredients:

- function-at-a-time lifting from ELF into `MicroIR`
- symbolic states over lifted blocks
- loop and region recovery
- function summaries
- loop summaries, invariants, and VCs

So the missing piece is not a new IR. It is an interprocedural control algorithm.

## Abstract Machine

Use states of the form $S = (f, pc, \sigma, \pi, h, k)$ where:

- $f$ is the current function
- $pc$ is the current program point
- $\sigma$ is the symbolic environment
- $\pi$ is the path condition
- $h$ is the symbolic heap or memory abstraction
- $k$ is a continuation stack of suspended caller frames

Use continuations of the form $K = (f_c, pc_r, \sigma_c, \pi_c, h_c, dst)$ where:

- $f_c$ is the caller
- $pc_r$ is the resume point after the call
- $\sigma_c, \pi_c, h_c$ are the suspended caller components
- `dst` is the optional destination receiving the return value

## Demand-Driven Transition Rule

At a call site:

- if the callee is a direct internal function:
  - save the caller continuation
  - construct the callee entry state from argument bindings
  - analyze the callee
  - reuse or compute a summary
  - resume the caller

- if the callee is an external or unresolved function:
  - apply a stub summary
  - continue the caller

- if the call is indirect:
  - attempt target-set resolution from the current symbolic state
  - branch over feasible internal targets
  - fall back to an unknown-call summary when unresolved

## Fixpoint View

Let:

- $Lift(Binary, f)$ lift function $f$ from the binary
- $Recover(F)$ recover structure for lifted function $F$
- $Analyze(F, a)$ analyze function $F$ under abstract input state $a$

Then demand-driven analysis computes the least reachable set:

$Reach(entry, a_0)$

where transitions are induced by call sites:

$(f, a) \to (g, a_g)$

if analysis of $f$ under $a$ reaches a call to $g$ with abstract argument state $a_g$.

This is a reachable-function fixpoint, not a whole-program pass.

## Benefits

- only reachable functions are lifted
- call resolution is guided by actual symbolic state
- summaries are created only when needed
- loop reasoning remains local and compositional
- modular symbolic execution becomes practical on large binaries

## Known Gaps

### Indirect calls

Simple value propagation is often not enough. Real binaries use:

- callback slots
- dispatch tables
- loaded function pointers
- virtual dispatch

So the design needs target-set resolution, not just direct lookup.

### Caller suspension

The saved state must preserve:

- locals and registers
- stack abstraction
- heap / memory state
- path constraints
- expected return destination

### Recursion and cycles

Call graphs are not trees. The implementation needs:

- memoized summaries
- recursion cutoffs or widening
- context abstraction

### Context sensitivity

Per-call-site reanalysis is precise but expensive. Whole-function reuse is cheap but imprecise.

The practical compromise is:

- summary cache keyed by function plus abstract input class

### External calls

The system needs stub summaries for:

- allocators
- frees
- string / array primitives
- error / abort functions
- opaque crypto calls

## First Implementation Scope

The first implementation inside `BinRevX` is intentionally narrower:

- direct internal calls only
- DFS over reachable internal functions
- saved call-site state and resume metadata
- summary reuse by function name
- recursion cut off by an active-function set

This is enough to prove the architecture and to stop analyzing entire binaries upfront.
