.include "../include/nes.inc"
.include "header.inc"
.include "reset.inc"
.include "utils.inc"


.segment "ZEROPAGE"


.segment "CODE"
RESET:
    INIT_NES            ; From reset.inc

LoopForever:
    jmp LoopForever


NMI:
    rti

IRQ:
    rti


.segment "CHARS"


.segment "VECTORS"
.word NMI
.word RESET
.word IRQ