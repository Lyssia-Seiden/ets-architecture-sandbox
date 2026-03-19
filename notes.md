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
