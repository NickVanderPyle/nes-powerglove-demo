.macro PPU_SETADDR addr

    bit PPU_STATUS          ; read from PPU_STATUS to reset PPU_ADDR latch
    lda #>addr              ; get high byte of addr
    sta PPU_ADDR
    lda #<addr              ; get lo byte of addr
    sta PPU_ADDR

.endmacro

.macro PUSH_REGS
    pha             ; push A to the stack
    txa
    pha             ; push X to stack
    tya
    pha             ; push Y to stack
    php             ; Push processor status glags to stack
.endmacro

.macro PULL_REGS
    plp                 ; restore status flag from stack
    pla                 ; retore Y from stack
    tay
    pla                 ; restore X from stack
    tax
    pla                 ; restore A from the stack
.endmacro
