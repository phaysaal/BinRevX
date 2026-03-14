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

This is now a working prototype. It currently provides:

- a design document in [ARCHITECTURE.md](/home/faisal/code/hobby/llvmpattern/BinRevX/ARCHITECTURE.md)
- a text-based `MicroIR` loader
- a single-function ELF importer for symbolized binaries
- CFG, dominator, and natural-loop recovery
- region recovery for loops and branch headers
- symbolic state seeding and block interpretation
- loop summaries, invariant candidates, and VC stubs
- loop memory-pattern classification for:
  - arrays
  - strings
  - linked lists

## Build

```bash
make
```

## Run

```bash
./binrevx
./binrevx examples/loops.microir
./binrevx examples/memory_kinds.microir
./binrevx --func main ../bins/ecdsa_sign_openssl-O0_32
./binrevx --func ecdsa_sign_tester ../bins/ecdsa_sign_openssl-O0_32
```

When the input is an ELF binary, `BinRevX` imports one symbolized function at a time.
This path is currently aimed at structural recovery rather than full instruction-faithful lifting.

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
- `assign <dst> index <base> <idx> <width>`
- `assign <dst> field <base> <field>`
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

## ELF Import Scope

The current ELF importer is intentionally narrow.
It uses `nm` and `objdump` to import one function and lift a small subset of Intel-style x86 instructions into `MicroIR`.

What it is good for:

- wrapper detection
- branch recovery
- back-edge and loop discovery
- coarse symbolic states for selected functions

Current limits:

- one function at a time via `--func`
- requires symbols to be present in the binary
- partial instruction coverage
- approximate memory semantics
- memory-pattern classification on imported assembly is still weaker than on hand-authored `MicroIR`

## Memory Patterns

`BinRevX` now classifies recovered loops by memory behavior rather than only by CFG shape.
The current loop classifier looks for:

- array traversals: indexed access plus stride progression
- string traversals: byte-indexed access plus zero-sentinel exit
- linked-list traversals: field-based pointer chasing plus null exit

Example corpus:

- [examples/loops.microir](/home/faisal/code/hobby/llvmpattern/BinRevX/examples/loops.microir)
- [examples/memory_kinds.microir](/home/faisal/code/hobby/llvmpattern/BinRevX/examples/memory_kinds.microir)
