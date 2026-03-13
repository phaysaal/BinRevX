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
