# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build & Run

There is no cabal/stack project file. Build with `ghc` directly, sending intermediate artifacts to `build/`:

```
ghc -dynamic -outputdir build Main.hs -o Main
./Main
```

`-dynamic` is required on this machine — the local ghc only ships the dynamic build of `base`, and without it you get spurious "Could not find module 'Data.Word'" errors.

`Main`, `build/`, `*.hi`, `*.o` are gitignored.

## What this is

A Haskell emulator for an Explicit Token Store (ETS) dataflow architecture, modeled after the Monsoon machine described in the MIT PhD thesis referenced in `notes.md`. The concrete ISA implemented is "squall" — a simplified 64-bit ETS variant the author defines in `notes.md` ("what if ETS was easy"). `notes.md` is the design doc and is the source of truth for word/instruction layouts and semantics.

## Architecture

ETS is a tagged-token dataflow model. Execution state is just `(memory, pending tokens)`; there is no PC. Tokens carry `(ctx, stmnt, port, val)` and "fire" instructions when matched against memory presence bits.

The emulator (`Emulator.hs`) is structured around an `ISA` record (`parseInstr`, `applyInstr`, `selector`) so that alternate ISAs could plug in, though only `squall` exists. The core loop is `clock :: ISA -> ArchState -> ArchState`:

1. `selector` picks which subset of pending tokens to process this cycle (uarchitectural — current squall selector just takes the head, which is an explicit assumption noted in code comments).
2. For each chosen token, `applyToken` reads the instruction at `stmnt t`, runs `applyInstr`, and produces `(memory', new tokens)`.
3. New tokens are appended to the unprocessed remainder.

Key squall pieces in `Emulator.hs`:

- **Presence bits** (`Empty | Present | Constant`) live alongside each memory word and drive the token-matching state machine in `matchingFunction`. The three matching rules are `Dyadic` (waits for both operands), `Monadic` (always fires), `ConstDyadic` (left writes, right makes constant). `DecrExchange` is stubbed.
- **Token forming rules** (`Arith | Switch | Send | Extract`) determine what tokens get emitted after the ALU op runs. `Send` uses `squallParseTag` / `squallPackTag` to treat a value as a `(ctx, stmnt, port)` tag — this is how tokens cross contexts. `Extract` is the inverse, packing a tag into a value (used for return-address passing).
- **Instruction encoding** is the bit layout described at the bottom of `notes.md`. `squallParse` decodes a 64-bit word into an `Instruction`; `squallAsm` is the inverse and is what `Main.hs` uses to hand-assemble programs. If you change the layout, both must move together — and the field widths in `squallAsm`'s `place` helper must match the shifts in `squallParse`.
- **ALU ops** are defined twice: `squallALUOp` gives the value, `squallALUOutputLR` gives which of the two destination ports actually receive a token (e.g. `Dup` issues to both, others only to d1). Adding an ALU op means updating both, plus `squallAORepr`.

## Programs

Programs are written in `Main.hs` as Haskell lists of `(address, squallAsm Instruction)` pairs (or raw `AWord` constants), loaded into memory with all words marked `Constant`, and seeded with initial tokens. `clockN` steps the machine N times. `squallFactorial` in `Main.hs` is an in-progress hand-coded recursive factorial — it exercises `Switch`, `ConstDyadic`, and context construction for recursion, and is the closest thing to documentation for how non-trivial squall code is structured.

The `Emulator.trace'` helper (`show`-and-return) is the main debugging tool — wrap any value to dump it during evaluation.
