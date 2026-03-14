# BinRevX Architecture

## 1. Problem Statement

Binary symbolic execution is hard mainly because of:

- instruction-set size
- implicit machine state
- path explosion
- loop explosion
- poor modularity
- optimized and irregular control flow

The central design decision in `BinRevX` is:

do not symbolically execute raw assembly as the semantic working language.

Instead use:

`binary -> lifted CFG -> SSA-like MicroIR -> structural recovery -> summaries -> symbolic execution`

Related note:

- `architecture/DEMAND_DRIVEN_INTERPROC.md` describes the demand-driven interprocedural strategy for reachable-function lifting and analysis.

## 2. Instruction-Set Challenge

The instruction-set problem is real. Raw symbolic execution over assembly is expensive because:

- every architecture has hundreds to thousands of instructions and variants
- flags and partial-register effects are subtle
- memory addressing modes are irregular
- vector, floating-point, and system instructions explode the semantic surface

So the architecture separates concerns:

### Layer A: ISA Lifter

Maps assembly instructions into a canonical IR:

- arithmetic
- load/store
- branch
- call/return
- compare/test
- memory address computation
- explicit flag values as SSA terms

The lifter is the only ISA-specific layer.

### Layer B: MicroIR

A normalized semantic core with:

- explicit basic blocks
- SSA values
- explicit loads/stores
- explicit branch conditions
- explicit memory objects when recoverable
- explicit call edges

### Layer C: Structural Recovery

Recover:

- loops
- branch/merge regions
- switch-like dispatch
- loop-carried variables
- break/continue style exits

This is where the `LlvmReversed` idea transfers most directly.

### Layer D: Summarization

Produce:

- loop summaries
- candidate invariants
- recurrence relations
- modified-variable sets
- memory footprint summaries
- function summaries

### Layer E: Symbolic Execution

Run symbolic execution over:

- structured regions
- summaries
- residual unstructured fragments

The executor should not unfold every loop path by default.

## 3. Core Pipeline

Let:

- $L$ be lifting from binary to `MicroIR`
- $S$ be structural recovery
- $I$ be invariant and summary synthesis
- $E$ be symbolic execution over the structured form

Then the intended pipeline is:

$\Pi = E \circ I \circ S \circ L$

### 3.1 Lifting

Input:

- machine code
- entry points
- symbol information if available

Output:

- CFG
- SSA-like program
- architectural state model

### 3.2 Recovery

Input:

- `MicroIR` CFG

Output:

- structured regions
- loop descriptors
- join descriptors
- call graph skeleton

### 3.3 Summaries

Input:

- loop regions
- function regions

Output:

- invariants
- summaries
- side-effect sets

### 3.4 Execution

Input:

- structured program plus summaries

Output:

- path conditions
- state summaries
- alarms or proofs

## 4. Mathematical Interfaces

### 4.1 Lifting

$L : Binary ~> MicroProg$

Partial because disassembly, indirect control flow, or unsupported instructions may block precise lifting.

### 4.2 Structural Recovery

$S : MicroProg ~> StructProg$

Where `StructProg` contains:

- `IfRegion`
- `LoopRegion`
- `SwitchRegion`
- `SeqRegion`
- `ResidualRegion`

### 4.3 Summary Synthesis

$I : StructProg ~> SummaryProg$

Where summaries are attached to loops and functions.

### 4.4 Symbolic Execution

$E : SummaryProg \times Query -> Result$

## 5. Loop Recovery Goal

Loop detection alone is not enough. The system should recover:

- loop header
- loop body
- exit edges
- induction-like variables
- loop-carried dependencies
- modified memory region
- nesting structure

The target abstraction is:

$Loop = (H, B, X, V_c, M_w, \phi)$

where:

- $H$ is the header
- $B$ is the body region
- $X$ is the exit set
- $V_c$ is the loop-carried variable set
- $M_w$ is the written-memory abstraction
- $\phi$ is the loop condition

## 6. Modular Analysis Goal

For each recovered function:

$Summary(f) = (Pre_f, Post_f, Mod_f, Ret_f, Calls_f)$

where:

- $Pre_f$ is the precondition
- $Post_f$ is the postcondition
- $Mod_f$ is the modified-variable and modified-memory set
- $Ret_f$ is the return-value relation
- $Calls_f$ is the set of callees

## 7. Slacsec-Inspired Symbolic Heap Layer

`slacsec` is the right reference for the next `BinRevX` step, not because of
its frontend, but because of its middle-end analysis architecture:

- symbolic state threaded by a `precond` function
- symbolic heaps with points-to and inductive predicates
- entailment checks between heap states
- bi-abduction to infer the missing caller heap and frame
- modular function summaries expressed as heap + pure constraints

The core import from `slacsec` should be the analysis shape:

`state -> command -> state-set + return-set`

augmented with heap reasoning:

`state = <sigma, delta, heap, frame, rho, pc>`

where:

- `sigma` is the symbolic store
- `delta` is the path-condition set
- `heap` is the known symbolic heap
- `frame` is the caller-owned heap required to make execution valid
- `rho` is an optional metadata environment for provenance/security/taint
- `pc` is the current path context

### 7.1 Suggested BinRevX State

For `BinRevX`, the practical symbolic state should be:

$State = (Sigma, Delta, Heap, Frame, Regions, Mods, Meta)$

where:

- $Sigma : Var -> Term$
- $Delta$ is a conjunction or disjunction-normalized set of path constraints
- $Heap$ is a symbolic heap
- $Frame$ is the abducted missing heap
- $Regions$ tracks recovered structural regions and current loop context
- $Mods$ tracks modified registers, stack slots, globals, and heap objects
- $Meta$ stores ABI, architecture, and provenance facts

### 7.2 Symbolic Heap Vocabulary

The heap language should be small and map directly from recovered memory
patterns:

- `Pto(base, fields)` for concrete cells / structs
- `Arr(base, lo, hi, width)` for contiguous arrays
- `Str(base, lo, hi)` for string segments
- `Ls(x, y, next)` for linked-list segments

This lines up well with the current `BinRevX` memory-pattern detector:

- array loops should propose `Arr`
- string loops should propose `Str`
- linked-list loops should propose `Ls`

### 7.3 Role of Entailment

`slacsec`'s `entlcheckA` is the key model here.

In `BinRevX`, heap entailment should answer:

$Heap_1 |= Heap_2$

meaning every concrete memory state satisfying $Heap_1$ also satisfies
$Heap_2$.

This is needed for:

- loop invariant checking
- pruning subsumed symbolic states
- modular call checking against callee preconditions
- validating candidate summaries

### 7.4 Role of Bi-Abduction

`slacsec`'s `biabd` family solves the modular-analysis problem:

given caller heap $H_c$ and callee requirement $H_r$, find:

- anti-frame $X$ such that $H_c * X$ satisfies the callee precondition
- frame $Y$ such that the unused caller heap is preserved

Mathematically:

$H_c * X |= H_r * Y$

For `BinRevX`, this should become the default call rule for unknown or
partially known callees.

### 7.5 How Structural Recovery Feeds Heap Analysis

`BinRevX` has one major advantage over `slacsec`: it can recover loops and
memory traversals directly from binaries before symbolic execution.

That means structural recovery should synthesize heap candidates before the
heap engine runs:

- array cursor + stable base + bound -> `Arr`
- byte cursor + zero-sentinel exit -> `Str`
- pointer chase + null exit + next field -> `Ls`

This gives a pipeline:

`binary -> MicroIR -> regions -> memory patterns -> heap predicates -> symheap analysis`

### 7.6 Function Summaries

The next `BinRevX` function summary should move from:

`params/pre/post/modifies/calls`

to:

$Summary(f) = (Pre_f, Post_f, Frame_f, Mod_f, Ret_f, Calls_f)$

with:

- $Pre_f$ containing pure constraints and heap predicates required at entry
- $Post_f$ containing return/value and heap-shape relations after execution
- $Frame_f$ containing preserved caller heap
- $Mod_f$ containing modified abstract locations

### 7.7 Loop Summaries

The current recurrence summaries should be lifted into heap-aware loop
summaries:

$LoopSummary = (Inv, Guard, Step, Footprint, Variant)$

where:

- $Inv$ is a pure + heap invariant
- $Guard$ is the loop-continuation condition
- $Step$ is the recurrence / heap-update relation
- $Footprint$ is the set of touched heap regions
- $Variant$ is the progress measure

### 7.8 Immediate Implementation Plan

The next concrete steps for `BinRevX` should be:

1. Introduce a `SymHeap` module with `Pto`, `Arr`, `Str`, and `Ls`.
2. Extend symbolic states from `env + mem + path` to store/frame heaps.
3. Convert existing memory-pattern outputs into candidate heap predicates.
4. Add a small entailment engine for pure + heap subsumption.
5. Add a first bi-abduction rule for function calls and loop widening.
6. Replace ad hoc memory facts with heap predicates in summaries and VCs.

## 8. Naming and Interface Preservation

For imported binaries, locals that are provably immutable spills of parameters
should be normalized back to the corresponding argument names. This matters for:

- readable summaries
- stable modular interfaces
- correct matching between call arguments and callee preconditions
- later heap predicates keyed by formal parameters rather than frame slots

The current importer now performs this normalization when the evidence is
strong enough, so recovered guards can refer to names like `arg1` instead of
temporary stack-slot names such as `local_18`.
- $Mod_f$ is the modified state set
- $Ret_f$ is the return relation
- $Calls_f$ is the call-summary dependency set

This lets the engine avoid inlining all callees during symbolic exploration.

## 7. Residual Principle

Not all binary code will structure cleanly.

So the system should support:

- structured regions when recovered
- residual path-wise symbolic execution otherwise

This is important. The project should not fail merely because one CFG fragment is ugly.

## 8. Initial Deliverables

The first useful milestones are:

1. define `MicroIR`
2. define loop descriptors and summary interfaces
3. build a structural recovery front end over lifted CFGs
4. integrate a symbolic state that can consume recovered loops
5. fall back to residual execution when structuring fails

## 9. Immediate Research Claim

The strongest defensible early claim is:

structured recovery before symbolic execution reduces path explosion by turning repeated low-level CFG exploration into loop-level and function-level reasoning.

That is a better claim than simply saying the system "detects loops".
