file:///home/lyssia/Downloads/22154101-MIT.pdf <- phd thesis thats relevant

state is just memory and ready tokens
so tokens are in arch!
memory has presence bits (3/word)
does this mean we have a 61bit machine?
^ the impl of the presence states is (u?)arch-defined

'activities' -> < context + segment, value_l, value_r, ALU_op, T.d >
T.d is a 'token forming rule including destination offsets'
^ ~~uarch i believe (??), is the thing that goes through the pipeline~~
^ arith, send, extract (see below)
this includes s1 and s2 for dests

'activity generation' :: token -> activity
loads from a memory location based on 'effective addressing modes'
this is stored in the instruction string

we have 3 types of operators: dyadic (2->1), monadic (1->1), and constant dyadic (1->1)
each of these defines a state machine on the *presence bits* of the location in memory referenced.
dyadic: empty -set mem-> present -release activity-> empty
monadic always fires
const dyadic: empty -l write-> present -read-> constant
                    \-r write-> constant

when we emit a new token we use a Rule specified by the instr
arithmetic, send, extract

## arithmetic
same context, segment += value in instr, ALU_op v_l v_r
this can have multiple return values.
can be used for switch instructions by muxing the dest offset
^ this is later labelled a different forming rule

## send
tags are pointers
we need a way to explicitly change tag of token
result tag is `v_l`, and value is `v_r`
also sends `v_r` in the original context as an ack

## extract
oppositeish of send, moves tags to values
can be used to give callees return address locations

instruction set is formed from cartesian product of {dyadic, monadic, sticky} operators and {arithmetic, switch, send, extract} token forming rules
^ these can be fused (??) somehow

# monsoon specifics

72 bit word - 8 type-tag 64 data
type-tags are not actually used for hw checking (?)
^ they are used for presence state stuff
because data has to store a full tag this means 32 bit addr space
tokens are two words, but higher word is always a tag type

tags
port :: [map ; 7] :: [ip ; 24] :: [pe ; 10] :: [fp ; 22]
map is "alias and interleave control" <- for sharing data across PEs
pe bits can be shifted to fp
fp and ip are both physical addresses
limited to 36GiB memory

instrs
32 bits, stored 2/word
*present* presence bit status
[opcode ; 10] :: [r ; 10] :: port :: [s ; 11]
r is an offset for the operand
depending on the opcode, this can be an offset to the ip, fp, or an abs addr
s is an offset for rhs destination
lhs destination is always IP + 1

# squall

what if ETS was easy
for me

64 bit word, 8 bit presence
instruction:
[d1 [addr ; 20] :: [port ; 1]] (43)
:: [d2 [addr ; 20] :: [port ; 1]] (22)
:: [r ; 10] :: [ea ; 2]
:: [tm ; 2] :: [ao ; 6] :: [tf ; 2]

yeah we just assume destinations are in a 25 bit addr region
bc im lazy

0 = left

## field encodings

ea (effective addressing mode, 2 bits) — how `r` resolves to a memory address:
- 00 FrameRelative — `ctx t + r(instr@stmnt)`, i.e. r is read from the *frame slot* pointed at by the firing token's stmnt, then added to ctx
- 01 CodeRelative  — `stmnt t + r`
- 10 Global        — `r` (absolute)

tm (token matching rule, 2 bits) — state machine on the presence bits at the resolved address:
- 00 Dyadic      — empty -(write)-> present -(read, fire)-> empty. waits for both operands.
- 01 Monadic     — always fires, single operand. left port only.
- 10 ConstDyadic — empty -(L write)-> present -(R exchange, fire)-> constant
                       \-(R write)-> constant -(L read, fire)-> constant
                   used to latch a constant operand and then fire repeatedly against it.

ao (ALU op, 6 bits in the layout, 5 used) — operation and which dest ports get an output token:
- 00000 Nop    — passthrough lhs; issues d1 only
- 00001 AddVal — lhs + rhs;       issues d1 only
- 00010 Lt     — lhs < rhs ? 1 : 0; issues d1 only
- 00011 Dup    — passthrough lhs; issues BOTH d1 and d2 (the only op that fans out via the ALU stage)
- 00100 MulVal — lhs * rhs;       issues d1 only

which destinations actually fire is `squallALUOutputLR ao` — adding a new ao means updating both the value function and the (il, ir) function.

tf (token forming rule, 2 bits) — what tokens come out of a firing:
- 00 Arith   — emit value(s) from the ALU op to d1 (and d2 if the op fans out). dest stmnt = `stmnt t + offset`.
- 01 Switch  — predicate on rhs: if rhs /= 0 emit lhs to d1, else emit lhs to d2. ALU op is ignored on the value path.
- 10 Send    — interpret lhs as a packed tag (ctx, stmnt, port); emit a token with that tag carrying rhs as value, AND emit rhs to d1 in the *current* ctx as an ack.
- 11 Extract — pack `(ctx t, stmnt t + offset(d2), port(d2))` into a tag value and emit it to d1. inverse of Send; used to mint return-address tags.

tag layout (used by Send/Extract): `[ctx ; 32] :: [stmnt ; 31] :: [port ; 1]` packed into one 64-bit word, low bit = port. NOTE: `squallParseTag` and `squallPackTag` currently disagree on the port bit polarity (parse: even→Right, pack: Left→0), and they don't mask `ctx` to 32 bits — both are bugs to fix before Send/Extract round-trips reliably.

add two instructions and send them to the same
frame relative addr
00000000000000000010 0
00000000000000000000 0 # nop
0000000000 00
00 00000 00
