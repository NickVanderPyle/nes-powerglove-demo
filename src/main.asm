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

.segment "CODE"

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

    jmp LoopForever


NMI:
    PUSH_REGS               ; macros to save register vals

    lda #0
    sta PPU_CTRL                ; Disable NMI


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
.byte $1C,$0F,$22,$1C, $1C,$37,$3D,$0F, $1C,$37,$3D,$30, $1C,$0F,$3D,$30 ; Background palette
.byte $1C,$0F,$2D,$10, $1C,$0F,$20,$27, $1C,$2D,$38,$18, $1C,$0F,$1A,$32 ; Sprite palette


.segment "CHARS"
.incbin "chars.chr"

.segment "VECTORS"
.word NMI
.word RESET
.word IRQ