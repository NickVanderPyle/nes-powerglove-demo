.include "consts.inc"
.include "header.inc"
.include "reset.inc"
.include "utils.inc"


.segment "ZEROPAGE"
IsDrawComplete:                     .res 1      ; Flag to indicate when BLank is done drawing.
Counter:                            .res 1      ; Frame counter
UpdateHexToTiles_param_value:       .res 1      ; param to convert to hex tiles.
UpdateHexToTiles_return_digit1:     .res 1      ; return hi-nibble tile number.
UpdateHexToTiles_return_digit2:     .res 1      ; return lo-nibble tile number.
SetBGColToTileIndex_param_xCol:     .res 1
SetBGColToTileIndex_param_yCol:     .res 1
SetBGColToTileIndex_param_value:    .res 1
SetBGColToTileIndex_tile_addr:      .res 2
WriteAToJoy1_temp:                  .res 1

PowerGloveStatus:                   .res 1      ; $5F when Power Glove ready to send more bytes.
PowerGloveData:                     .res 7      ; 1st byte: Signed X-Coordinate
                                                ; 2nd byte: Signed Y-Coordinate
                                                ; 3rd byte: Signed Z-Coordinate
                                                ; 4th byte: Wrist Rotation Angle (around Z-axis)
                                                ; 5th byte: Finger Flex Sensors
                                                ; 6th byte: Control Pad Buttons
                                                ; NA  These two unused because...
                                                ; NA  init bytes end in $3F, $01
                                                ; 7th byte: Error Flags

.segment "RODATA"
PowerGloveAnalogMode7Bytes:
    .byte $06, $C1, $08, $00, $02, $3F, $01
    ;     │    │    │         │    │ 
    ;     │    │    │         │    └─────── $3F $01  Mask Word $01FF (bit0-8: request 1st..7th response byte)
    ;     │    │    │         └──────────── $02      Two 8bit "opcodes" (in Analog mode, they are "masks", not "opcodes")
    ;     │    │    │
    ;     │    │    └────────────────────── $08 $00  Opcode $0800 (maybe analog request, or maybe just a dummy-opcode)
    ;     │    └─────────────────────────── $C1      Analog Mode (bit7), one 16bit opcode (bit3-0)
    ;     └──────────────────────────────── $06      Length, total number of following bytes
    ; https://problemkaputt.de/everynes.htm#controllerspowerglovetxpacketsconfigurationopcodes

.segment "CODE"

.proc Wait3100Cycles
    ; jsr here is 6cy
    pha                     ; 3cy push A to the stack
    php                     ; 3cy push processor status to stack
    lda #0                  ; 2cy
    Loop256Times:           ; Loop waits 3072 cpu cycles
        nop
        nop
        nop
        nop
        sbc #1
        bne Loop256Times
    plp                     ; 4cy restore status flag from stack
    pla                     ; 4cy retore A from stack
    rts                     ; 6cy
.endproc

.proc Wait1052Cycles
    ; jsr here is 6cy
    pha                     ; 3cy push A to the stack
    php                     ; 3cy push processor status to stack
    lda #0                  ; 2cy
    Loop256Times:           ; 1024cy
        sbc #1
        bne Loop256Times
    plp                     ; 4cy restore status flag from stack
    pla                     ; 4cy retore A from stack
    rts                     ; 6cy
.endproc

.proc Wait100Cycles
    ; jsr here is 6cy
    pha                     ; 3cy push A to the stack
    php                     ; 3cy push processor status to stack
    lda #18                  ; 2cy
    Loop256Times:           ; 72cy
        sbc #1
        bne Loop256Times
    plp                     ; 4cy restore status flag from stack
    pla                     ; 4cy retore A from stack
    rts                     ; 6cy
.endproc

; Clobbers A, X
.proc WriteAToJoy1
    sta WriteAToJoy1_temp
    
    txa
    pha
    php

    ldx #$08
    ForX8To0:
        lda #%00000000              ; Clear A to all zero bits
        asl WriteAToJoy1_temp       ; Shift MSB into Carry
        rol                         ; Rotate the Carry into A's 0th bit
        sta APU_PAD1                ; Write 0th bit to APU_PAD1
        lda APU_PAD1                ; Dummy read, sends a clock pulse to Power Glove
        dex
        bne ForX8To0                ; if X != 0 goto ForX0To8
    
    plp
    pla
    tax

    rts
.endproc

; https://problemkaputt.de/everynes.htm#controllerspowerglove
; Write %00000001 to $4016 then wait 3330cycles.
; Write each byte's bit, one at a time, MSB first, %0000000B to $4016.
; Every other byte, wait 1280cycles.
; Clobbers A, X
.proc InitPowerGlove
        ldx #%00000001
        stx APU_PAD1
        jsr Wait3100Cycles

        ldx #0
        Loop7Bytes:
            lda PowerGloveAnalogMode7Bytes,x
            jsr WriteAToJoy1
            inx

            txa                     ; https://problemkaputt.de/everynes.htm#controllerspowerglove
            and #%00000001          ; Suggests to have a delay after every other byte.
            beq SkipIfXIsEven
                jsr Wait1052Cycles
            SkipIfXIsEven:

            cpx #7
            bne Loop7Bytes
        rts
.endproc

; https://problemkaputt.de/everynes.htm#controllerspowerglovetransmissionprotocolrxtx
; To read data, first read a status byte.
; Saves to PowerGloveStatus
; Clobbers A, X
.proc ReadPowerGloveStatus
    lda #%00000001
    sta APU_PAD1
    lda #%00000000 
    sta APU_PAD1

    ldx #$08
    ForX8To0:
        lda APU_PAD1            ; Read 0th bit in from Power Glove.
        lsr                     ; Shift-right 0th bit onto Carry.
        rol PowerGloveStatus    ; Rotate-left the 0th bit from carry.
        dex
        bne ForX8To0
    rts
.endproc

.proc ReadPowerGloveData
    lda #%00000001
    sta APU_PAD1
    lda #%00000000 
    sta APU_PAD1

    jsr Wait100Cycles

    ldx #0
    ForX0To7:
        lda #0
        sta PowerGloveData,x

        ldy #$08
        ForY8To0:
            lda APU_PAD1            ; Read 0th bit in from Power Glove.
            lsr                     ; Shift-right 0th bit onto Carry.
            rol PowerGloveData,x    ; Rotate-left the 0th bit from carry.
            dey
            bne ForY8To0

        lda PowerGloveData,x
        eor #$FF                    ; Undo inversion.
        sta PowerGloveData,x

        inx
        cpx #7
        bne ForX0To7
    
    rts
.endproc

; Param: UpdateHexToTiles_param_value is number to be converted to hex tiles.
; Hex tile indexes placed in UpdateHexToTiles_return_digit1 and UpdateHexToTiles_return_digit2
.proc UpdateHexToTiles
    ; Split UpdateHexToTiles_param_value into two nibbles

    lda UpdateHexToTiles_param_value
    lsr             ; shift hi-nibble to lo-nibble
    lsr             ;
    lsr             ;
    lsr             ;

    ; digit tiles begin at tile idx CHARS_HEX_ZERO_INDEX
    clc
    adc #CHARS_HEX_ZERO_INDEX
    sta UpdateHexToTiles_return_digit1

    lda UpdateHexToTiles_param_value
    and #$0F        ; mask hi-nibble
    clc
    adc #CHARS_HEX_ZERO_INDEX
    sta UpdateHexToTiles_return_digit2

    rts
.endproc

.proc LoadPalette
    PPU_SETADDR $3F00
    ldy #0
:   lda PaletteData,y
    sta PPU_DATA
    iny
    cpy #32                  ; if y != 32 then
    bne :-                   ; goto prev label
    rts
.endproc

.proc ClearBackground
    PPU_SETADDR $2000

    ldy #0
    lda #0
:   sta PPU_DATA
    iny
    cpy #255                 ; if y != 255 then
    bne :-                   ; goto prev label
    rts
.endproc

; params:
;   SetBGColToTileIndex_param_xCol
;   SetBGColToTileIndex_param_yCol
;   SetBGColToTileIndex_param_value
; clobbers: A register
.proc SetBGColToTileIndex
    ; A = (yCol * 32) + xCol

    lda #$20
    sta SetBGColToTileIndex_tile_addr+1
    lda #$00
    sta SetBGColToTileIndex_tile_addr

    ; Add 32 to the address to skip a row per yCol
    lda SetBGColToTileIndex_param_yCol
    Add32ColumnsPerRowLoop:
    beq DoneMultiplyingByRow
        lda SetBGColToTileIndex_tile_addr
        clc
        adc #32             ; for each row, increment 32 tiles
        sta SetBGColToTileIndex_tile_addr
        bcc :+
            lda SetBGColToTileIndex_tile_addr+1
            adc #0
            sta SetBGColToTileIndex_tile_addr+1
        :
        dec SetBGColToTileIndex_param_yCol
        jmp Add32ColumnsPerRowLoop
    DoneMultiplyingByRow:

    ; Add XCols to the tile index
    lda SetBGColToTileIndex_tile_addr
    clc
    adc SetBGColToTileIndex_param_xCol
    sta SetBGColToTileIndex_tile_addr
    bcc :+
        lda SetBGColToTileIndex_tile_addr+1
        adc #0
        sta SetBGColToTileIndex_tile_addr+1
    :

    ; Tell PPU which tile we're about to write
    bit PPU_STATUS          ; read from PPU_STATUS to reset PPU_ADDR latch
    lda SetBGColToTileIndex_tile_addr+1
    sta PPU_ADDR
    lda SetBGColToTileIndex_tile_addr
    sta PPU_ADDR

    ; write the updated tile value
    lda SetBGColToTileIndex_param_value
    sta PPU_DATA

    rts
.endproc

RESET:
    INIT_NES            ; From reset.inc
    
    jsr InitPowerGlove

Main:
    jsr LoadPalette
    jsr ClearBackground

EnableRendering:
    ; Enable NMI and set background to use the 2nd pattern table (at $1000)
    lda #%10010000
    ;     ││││││└── 0,1 - Base nametable address (0 = $2000; 1 = $2400; 2 = $2800; 3 = $2C00)
    ;     │││││└───   2 - VRAM address increment per CPU read/write of PPUDATA (0: add 1, going across; 1: add 32, going down)
    ;     ││││└────   3 - Sprite pattern table address for 8x8 sprites (0: $0000; 1: $1000; ignored in 8x16 mode)
    ;     │││└─────   4 - Background pattern table address (0: $0000; 1: $1000)
    ;     ││└──────   5 - Sprite size (0: 8x8 pixels; 1: 8x16 pixels – see PPU OAM#Byte 1)
    ;     │└───────   6 - PPU master/slave select (0: read backdrop from EXT pins; 1: output color on EXT pins)
    ;     └────────   7 - Generate an NMI at the start of the vertical blanking interval (0: off; 1: on)
    sta PPU_CTRL

    lda #0
    sta PPU_SCROLL           ; Disable scroll in X
    sta PPU_SCROLL           ; Disable scroll in Y

    lda #%00011110
    ;     │││││││└── Greyscale (0: normal color, 1: produce a greyscale display)
    ;     ││││││└─── 1: Show background in leftmost 8 pixels of screen, 0: Hide
    ;     │││││└──── 1: Show sprites in leftmost 8 pixels of screen, 0: Hide
    ;     ││││└───── 1: Show background
    ;     │││└────── 1: Show sprites
    ;     ││└─────── Emphasize red (green on PAL/Dendy)
    ;     │└──────── Emphasize green (red on PAL/Dendy)
    ;     └───────── Emphasize blue
    sta PPU_MASK
    

LoopForever:
    inc Counter
    
    WaitForVBlank:
        lda IsDrawComplete
        beq WaitForVBlank
    lda #0
    sta IsDrawComplete

    jsr ReadPowerGloveStatus
    lda PowerGloveStatus
    cmp #$5F
    bne SkipPowerGloveUpdate
        jsr ReadPowerGloveData
    SkipPowerGloveUpdate:

    jmp LoopForever


NMI:
    PUSH_REGS               ; macros to save register vals

    lda #0
    sta PPU_CTRL                ; Disable NMI

    X_value:
        lda PowerGloveData
        sta UpdateHexToTiles_param_value
        jsr UpdateHexToTiles

        ; draw hex values to xCol=10, yCol=10
        lda #5
        sta SetBGColToTileIndex_param_xCol
        lda #10
        sta SetBGColToTileIndex_param_yCol
        lda UpdateHexToTiles_return_digit1
        sta SetBGColToTileIndex_param_value
        jsr SetBGColToTileIndex

        lda #6
        sta SetBGColToTileIndex_param_xCol
        lda #10
        sta SetBGColToTileIndex_param_yCol
        lda UpdateHexToTiles_return_digit2
        sta SetBGColToTileIndex_param_value
        jsr SetBGColToTileIndex

    Y_value:
        lda PowerGloveData+1
        sta UpdateHexToTiles_param_value
        jsr UpdateHexToTiles

        ; draw hex values to xCol=10, yCol=10
        lda #5
        sta SetBGColToTileIndex_param_xCol
        lda #11
        sta SetBGColToTileIndex_param_yCol
        lda UpdateHexToTiles_return_digit1
        sta SetBGColToTileIndex_param_value
        jsr SetBGColToTileIndex

        lda #6
        sta SetBGColToTileIndex_param_xCol
        lda #11
        sta SetBGColToTileIndex_param_yCol
        lda UpdateHexToTiles_return_digit2
        sta SetBGColToTileIndex_param_value
        jsr SetBGColToTileIndex

    Z_value:
        lda PowerGloveData+2
        sta UpdateHexToTiles_param_value
        jsr UpdateHexToTiles

        ; draw hex values to xCol=10, yCol=10
        lda #5
        sta SetBGColToTileIndex_param_xCol
        lda #12
        sta SetBGColToTileIndex_param_yCol
        lda UpdateHexToTiles_return_digit1
        sta SetBGColToTileIndex_param_value
        jsr SetBGColToTileIndex

        lda #6
        sta SetBGColToTileIndex_param_xCol
        lda #12
        sta SetBGColToTileIndex_param_yCol
        lda UpdateHexToTiles_return_digit2
        sta SetBGColToTileIndex_param_value
        jsr SetBGColToTileIndex

    ; reset scroll because changing PPU_ADDR will also change the scroll: https://www.nesdev.org/wiki/PPU_scrolling#Frequent_pitfalls
    lda #0
    sta PPU_SCROLL
    sta PPU_SCROLL

    RefreshRendering:
        lda PPU_STATUS
        lda #%10010000           ; Enable NMI, sprites from Pattern Table 0, background from Pattern Table 1
        sta PPU_CTRL
        lda #%00011110           ; Enable sprites, enable background, no clipping on left side
        sta PPU_MASK

    SetDrawComplete:
        lda #1
        sta IsDrawComplete      ; set drawcomplete to indicate ppu is done

    PULL_REGS
    rti


IRQ:
    rti

PaletteData:
.incbin "palette.pal"
.incbin "palette.pal"


.segment "CHARS"
.incbin "chars.chr"

.segment "VECTORS"
.word NMI
.word RESET
.word IRQ