# Memory Pattern Recognition for Loop Classification

## Goal

Detect whether a loop is processing:

- an array
- a string
- a linked list

The key idea is not to classify by syntax, but by the loop's memory-usage pattern.

For binaries this should be done after lifting into a normalized IR, but before full symbolic execution.

## Method

For each candidate loop:

1. compile or lift examples
2. observe the loop-carried cursor variable
3. observe how memory addresses are computed
4. observe what terminates the loop
5. observe whether the cursor advances by arithmetic or by following a loaded pointer

The object kind is then recognized from the feature combination.

## Example Corpus

I created two examples for each family under:

- [examples/patterns](/home/faisal/code/hobby/llvmpattern/BinRevX/examples/patterns)

and compiled them to LLVM IR at:

- [examples/patterns/ir/O0](/home/faisal/code/hobby/llvmpattern/BinRevX/examples/patterns/ir/O0)
- [examples/patterns/ir/O1](/home/faisal/code/hobby/llvmpattern/BinRevX/examples/patterns/ir/O1)

The `-O1` form is the most useful baseline because it removes stack noise while preserving loop structure.

## 1. Array Processing Pattern

Examples:

- [array_sum_idx.c](/home/faisal/code/hobby/llvmpattern/BinRevX/examples/patterns/array_sum_idx.c)
- [array_fill_ptr.c](/home/faisal/code/hobby/llvmpattern/BinRevX/examples/patterns/array_fill_ptr.c)

Observed `-O1` signatures:

- [array_sum_idx.ll](/home/faisal/code/hobby/llvmpattern/BinRevX/examples/patterns/ir/O1/array_sum_idx.ll#L13)
- [array_fill_ptr.ll](/home/faisal/code/hobby/llvmpattern/BinRevX/examples/patterns/ir/O1/array_fill_ptr.ll#L13)

### 1.1 Indexed Array Traversal

From [array_sum_idx.ll](/home/faisal/code/hobby/llvmpattern/BinRevX/examples/patterns/ir/O1/array_sum_idx.ll#L16):

- integer PHI cursor:
  - `%7 = phi i64 [ 0, %4 ], [ %12, %6 ]`
- address computed by GEP from stable base plus cursor:
  - `%9 = getelementptr inbounds i32, ptr %0, i64 %7`
- element read:
  - `%10 = load i32, ptr %9`
- cursor increment:
  - `%12 = add ... i64 %7, 1`
- bound check against loop-invariant limit:
  - `%13 = icmp eq i64 %12, %5`

This is the canonical array-index loop.

### 1.2 Pointer-Walk Array Traversal

From [array_fill_ptr.ll](/home/faisal/code/hobby/llvmpattern/BinRevX/examples/patterns/ir/O1/array_fill_ptr.ll#L14):

- pointer PHI cursor:
  - `%8 = phi ptr [ %9, %7 ], [ %0, %3 ]`
- store through the cursor:
  - `store i32 %2, ptr %8`
- pointer increment by element size:
  - `%9 = getelementptr inbounds i32, ptr %8, i64 1`
- exit by pointer-vs-end comparison:
  - `%10 = icmp ult ptr %9, %5`

This is still array processing, but with a pointer cursor instead of an integer index.

### 1.3 Array Loop Signature

A loop is likely array-processing if it has:

- a stable base pointer $b$
- a loop-carried cursor $c$
- element addresses of the form `gep(b, c)` or `gep(c, 1)`
- a constant stride update
- a bound comparison against:
  - a length-derived limit, or
  - an end pointer

Feature vector:

$ArrayLoop = (StableBase, ConstantStride, ElementGEP, BoundCompare, NoNextFieldLoad)$

## 2. String Processing Pattern

Examples:

- [string_strlen_loop.c](/home/faisal/code/hobby/llvmpattern/BinRevX/examples/patterns/string_strlen_loop.c)
- [string_copy_loop.c](/home/faisal/code/hobby/llvmpattern/BinRevX/examples/patterns/string_copy_loop.c)

Observed `-O1` signatures:

- [string_strlen_loop.ll](/home/faisal/code/hobby/llvmpattern/BinRevX/examples/patterns/ir/O1/string_strlen_loop.ll#L10)
- [string_copy_loop.ll](/home/faisal/code/hobby/llvmpattern/BinRevX/examples/patterns/ir/O1/string_copy_loop.ll#L12)

### 2.1 Sentinel-Read String Traversal

From [string_strlen_loop.ll](/home/faisal/code/hobby/llvmpattern/BinRevX/examples/patterns/ir/O1/string_strlen_loop.ll#L11):

- integer or pointer cursor
- byte-address GEP:
  - `%4 = getelementptr inbounds i8, ptr %0, i64 %3`
- byte load:
  - `%5 = load i8, ptr %4`
- sentinel test against zero:
  - `%6 = icmp eq i8 %5, 0`
- cursor increment by one:
  - `%7 = add ... i64 %3, 1`

This differs from generic array traversal because the loop exit depends on the loaded element being zero.

### 2.2 Dual-Cursor String Copy

From [string_copy_loop.ll](/home/faisal/code/hobby/llvmpattern/BinRevX/examples/patterns/ir/O1/string_copy_loop.ll#L13):

- source and destination pointer PHIs:
  - `%7 = phi ptr ...`
  - `%8 = phi ptr ...`
- source byte load:
  - `%11 = load i8, ptr %10`
- destination byte store:
  - `store i8 %6, ptr %8`
- both cursors increment by one:
  - `%9 = gep i8, ptr %8, 1`
  - `%10 = gep i8, ptr %7, 1`
- sentinel exit from loaded byte:
  - `%12 = icmp eq i8 %11, 0`

This is a stronger string signature than generic byte-array copy because termination is sentinel-based rather than length-based.

### 2.3 String Loop Signature

A loop is likely string-processing if it has:

- byte-level element accesses (`i8`)
- stride `+1`
- a loaded-byte test against zero
- optionally two synchronized byte cursors for copy/compare loops

Feature vector:

$StringLoop = (ByteAccess, Stride1, SentinelZeroTest, OptionalDualCursor)$

The most discriminating feature is:

- loop exit depends on `load(...) == 0`

That almost never happens in ordinary length-bounded array loops.

## 3. Linked List Processing Pattern

Examples:

- [list_sum.c](/home/faisal/code/hobby/llvmpattern/BinRevX/examples/patterns/list_sum.c)
- [list_length.c](/home/faisal/code/hobby/llvmpattern/BinRevX/examples/patterns/list_length.c)

Observed `-O1` signatures:

- [list_sum.ll](/home/faisal/code/hobby/llvmpattern/BinRevX/examples/patterns/ir/O1/list_sum.ll#L13)
- [list_length.ll](/home/faisal/code/hobby/llvmpattern/BinRevX/examples/patterns/ir/O1/list_length.ll#L14)

### 3.1 Node Traversal

From [list_sum.ll](/home/faisal/code/hobby/llvmpattern/BinRevX/examples/patterns/ir/O1/list_sum.ll#L14):

- pointer PHI cursor over nodes:
  - `%5 = phi ptr [ %9, %3 ], [ %0, %1 ]`
- direct node payload read:
  - `%6 = load i32, ptr %5`
- GEP to next-field:
  - `%8 = getelementptr inbounds %struct.Node, ptr %5, i64 0, i32 1`
- load next pointer:
  - `%9 = load ptr, ptr %8`
- null test on next pointer:
  - `%10 = icmp eq ptr %9, null`

From [list_length.ll](/home/faisal/code/hobby/llvmpattern/BinRevX/examples/patterns/ir/O1/list_length.ll#L14):

- same pointer PHI structure
- same `field 1` next-pointer load
- loop body advances by pointer chase, not pointer arithmetic

### 3.2 Linked List Signature

A loop is likely list-processing if it has:

- a loop-carried pointer cursor
- a load or field-GEP that extracts another pointer from the current node
- cursor update by pointer chase:
  - `$p' = load(gep(p, next\_field))$`
- null-based termination
- no constant-stride pointer arithmetic for traversal

Feature vector:

$ListLoop = (PointerPHI, NextFieldLoad, PointerChaseUpdate, NullTermination, NoStrideGEP)$

This is the key distinction from arrays:

- arrays advance by arithmetic on the cursor
- linked lists advance by loading the next cursor from memory

## 4. Core Classification Idea

For each loop, recover a **memory cursor graph**.

Let the loop-carried cursor set be $C = \{c_1, \dots, c_k\}$.

For each $c_i$, compute:

- `cursor-kind`:
  - integer index
  - pointer cursor
- `update-kind`:
  - arithmetic stride
  - loaded-next-pointer
- `access-kind`:
  - direct load/store through `gep(base, idx)`
  - byte access
  - field access
- `termination-kind`:
  - bound compare
  - sentinel compare
  - null compare

Then classify by score.

### 4.1 Suggested Score Rules

Array score:

- `+3` if address is `gep(base, idx)` with stable base
- `+3` if cursor update is `idx := idx + const` or `ptr := gep(ptr, const)`
- `+2` if exit is bound compare against length or end pointer
- `-4` if cursor update is pointer chase from memory
- `-3` if exit is sentinel zero test

String score:

- `+4` if loaded byte is compared to zero in loop exit
- `+2` if element width is one byte
- `+2` if source and destination byte cursors advance together
- `-3` if exit is only length-bound compare

Linked-list score:

- `+4` if cursor update is `cursor := load(next-field(cursor))`
- `+3` if termination compares cursor or next pointer to null
- `+2` if there is a struct-field GEP on the cursor
- `-4` if traversal uses constant stride arithmetic

Choose the maximal score if it is above threshold. Otherwise classify as `unknown`.

## 5. Why This Fits BinRevX

`BinRevX` already recovers:

- loop headers
- guards
- carried variables
- one-step transitions

So the next structural layer should recover:

- memory cursor candidates
- update kind
- access kind
- termination kind

That means the detector should sit **after** loop recovery but **before** invariant generation.

Pipeline extension:

`recover loops -> detect memory pattern -> choose domain-specific invariants`

Examples:

- array loop:
  infer bounds, stride, maybe affine index relations
- string loop:
  infer sentinel reachability and prefix-copy relations
- linked list loop:
  infer acyclic next-step reachability, null-termination, visited-node summaries

## 6. Concrete Detector Design

For each loop $L$:

1. identify loop-carried cursors from PHI-like updates
2. inspect all memory accesses in the loop body
3. build tuples:

$Access = (cursor, addr\_form, elem\_width, is\_load, is\_store)$

4. inspect exit guard:

$Exit = (kind, operand\_shape)$

5. inspect cursor transition:

$Update = (cursor, arithmetic\_stride \mid pointer\_chase \mid unknown)$

6. classify by scoring

## 7. Minimum Detectable Signatures

Minimum array signature:

- one cursor
- one arithmetic update
- one GEP-based access
- one bound compare

Minimum string signature:

- one byte access
- one zero-sentinel compare
- one stride-1 update

Minimum linked-list signature:

- one pointer cursor
- one next-pointer load from current node
- one null exit test

## 8. Important Caveat

This should not be done on raw assembly opcodes.

It should be done on normalized lifted IR with:

- explicit memory loads/stores
- explicit pointer arithmetic
- explicit field projections when recoverable
- explicit loop-carried values

Otherwise the signal is too noisy.

## 9. Best Immediate Next Step

Implement a `MemoryPattern` analyzer in `BinRevX` that consumes:

- `LoopRecovery.loop_desc`
- symbolic one-step loop summary
- lifted memory accesses from `MicroIR`

and emits:

- `ArrayLoop`
- `StringLoop`
- `LinkedListLoop`
- `UnknownLoop`

with a score and supporting evidence.

That is the right path to make this feature practical.
