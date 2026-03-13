# BinRevX

`BinRevX` is a new project skeleton for binary symbolic execution driven by structural recovery.

The core idea is:

- lift binary code into a normalized SSA-like `MicroIR`
- recover structured loops and modular function summaries
- run symbolic execution over the recovered structure instead of raw path explosion

This project does not try to symbolically execute the full assembly instruction set directly.
Instead, ISA-specific lifting is isolated in the front end, and the symbolic engine works on a small canonical IR.

## Why This Exists

Classic symbolic execution on binaries suffers from:

- path explosion
- poor loop handling
- weak modularity
- architecture-specific instruction complexity

The intended architecture of `BinRevX` addresses those by:

1. `Lift`: binary to CFG + SSA-like `MicroIR`
2. `Recover`: loops, branches, and summaries from low-level control flow
3. `Summarize`: loop invariants and function summaries
4. `Execute`: symbolic execution over structured regions and residual paths

## Current Status

This is an architecture-first scaffold. It currently provides:

- a design document in [ARCHITECTURE.md](/home/faisal/code/hobby/llvmpattern/BinRevX/ARCHITECTURE.md)
- a small OCaml skeleton for:
  - canonical IR definitions
  - loop recovery interfaces
  - modular summary interfaces
  - top-level pipeline reporting
  - a simple text-based `MicroIR` loader

## Build

```bash
make
```

## Run

```bash
./binrevx
./binrevx examples/loops.microir
```

## MicroIR Input

The first input format is intentionally small and strict.

Example:

```text
func sum_to_ten entry entry
block entry
assign i const 0
assign sum const 0
term jump hdr
block hdr
term branch lt i 10 body exit
block body
assign sum binop add sum i
assign i binop add i 1
term jump hdr
block exit
term ret sum
endfunc
```

Supported statements:

- `assign <dst> const <n>`
- `assign <dst> reg <x>`
- `assign <dst> val <token>`
- `assign <dst> binop <op> <a> <b>`
- `assign <dst> cmp <pred> <a> <b>`
- `assign <dst> load <ptr>`
- `assign <dst> addr <base> <offset>`
- `store <src> <dstptr>`
- `assume ...`
- `assert ...`
- `call <dst|-> <callee> <arg...>`

Supported terminators:

- `term jump <label>`
- `term branch <pred> <a> <b> <then> <else>`
- `term switch <value> <default> <n> <c1> <l1> ...`
- `term ret <value>`
- `term ret void`
- `term stop`

## Design Principle

The huge assembly instruction set is not handled by pushing all ISA semantics into the symbolic executor.
Instead:

- the lifter translates instructions into a small semantic core,
- the structurer reasons over CFGs and loop schemas,
- the symbolic engine reasons over canonical effects, not opcode catalogs.
