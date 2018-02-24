; iNES header
.segment "HEADER"

INES_MAPPER = 0
INES_MIRROR = 0 ; 0 = horizontal mirroring, 1 = vertical mirroring
INES_SRAM   = 0 ; 1 = battery backed SRAM at $6000-7FFF

.byte 'N', 'E', 'S', $1A ; ID
.byte $02 ; 16k PRG bank count
.byte $01 ; 4k CHR bank count
.byte INES_MIRROR | (INES_SRAM << 1) | ((INES_MAPPER & $f) << 4)
.byte (INES_MAPPER & %11110000)
.byte $0, $0, $0, $0, $0, $0, $0, $0 ; padding

; CHR ROM
.segment "TILES"
.incbin "tiles.chr"

; Vectors, defined in CODE segment.
.segment "VECTORS"
.word nmi
.word reset
.word irq

; zero page variables
.segment "ZEROPAGE"

buf2000:        .res 1
buf2001:        .res 1
update_sprites: .res 1
update_bg:      .res 1
update_ppu:     .res 1
sleeping:       .res 1
gamepad:        .res 1
gamepad_last:   .res 1
game_mode:      .res 1
setup_state:    .res 1
grid_size_x:    .res 1
grid_size_y:    .res 1
rover_count:    .res 1
rovers_state:   .res 1
rovers_ad_help: .res 1
curr_rover:     .res 1
curr_rover_ptr: .res 2
curr_rover_x:   .res 1
curr_rover_y:   .res 1
curr_rover_h:   .res 1
curr_rover_ins: .res 36
next_rover:     .res 1

.segment "OAM"
.assert ((* & $FF) = 0),error,"oam not aligned to page"
oam:            .res 256

.segment "DRAW"
draw:           .res 256

; RAM variables
.segment "BSS"
rover1:         .res 40
rover2:         .res 40
rover3:         .res 40
rover4:         .res 40

; CODE
.segment "CODE"

ppu_mask = %00011110
nmi_on   = %10000010
nmi_off  = %00000010

color_blue  = $02
color_rust  = $07
color_gold  = $28
color_white = $30

palette:
.byte color_blue, color_rust, color_gold, color_white
.byte color_blue, color_white, color_white, color_gold

headings:
.byte "NESW"

press_start:
.byte "PRESS START"

logo:
.byte "DEALER",$BC,$BD

size_of_plateau:
.byte "SIZE OF PLATEAU?   ",$BE

how_many_rovers:
.byte "HOW MANY ROVERS?"

rover_number:
.byte "ROVER #  :"

rover_start:
.byte "START: "

rover_instructions:
.byte "INSTRUCTIONS:"

rover_help1:
.byte $BA,": LEFT   ",$BB,": RIGHT   ",$B0,": MOVE"

rover_help2:
.byte $B1,": ERASE ",$B5,$B6,$B7,": NEXT"

sprite_size_x:
.byte $1F, '5', $01, $9C

sprite_size_y:
.byte $1F, '5', $00, $B4

sprite_rover_count:
.byte $3F, '2', $00, $9C

sprite_rover_current:
.byte $1F, '1', $00, $50

sprite_start_x:
.byte $37, '0', $01, $50

sprite_start_y:
.byte $37, '0', $00, $60

sprite_start_h:
.byte $37, 'N', $00, $70

; fill remainder with $FF 
.repeat 256
		.byte $FF
.endrepeat

.macro DRAW_ROM start, len, ppu_addr, rom_addr
	lda #len
	sta draw+start
	lda #>ppu_addr
	sta draw+start+1
	lda #<ppu_addr
	sta draw+start+2
	.repeat len, i
    lda rom_addr+i
		sta draw+start+3+i
	.endrepeat
	lda #0
	sta draw+start+3+len
.endmacro

.macro PPU_OFF
  lda #0
	sta buf2001
  lda #1
	sta update_ppu

	jsr WaitFrame
.endmacro

.macro PPU_ON
  lda #ppu_mask
	sta buf2001
  lda #1
	sta update_ppu

	jsr WaitFrame
.endmacro

.macro DRAW_CLR
	lda #>$2800
	sta $2006
	lda #<$2800
	sta $2006

	ldy #4
	: ldx #0
		: sta $2007
			inx
			bne :-
		dey
		bne :--
.endmacro

.macro PPU_LATCH addr
	lda $2002
	lda #>addr
	sta $2006
	lda #<addr
	sta $2006
.endmacro
