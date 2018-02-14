; DealerOn Mars Rovers
; Nick Rogers, 2018
;

.feature force_range
.macpack longbranch

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

game_mode:      .res 1
clr_screen:     .res 1
updating_bg:    .res 1
gamepad:        .res 1
gamepad_last:   .res 1
setup_state:    .res 1
grid_size_x:    .res 1
grid_size_y:    .res 1
rover_count:    .res 1
rovers_state:   .res 1
curr_rover:     .res 1
curr_rover_ptr: .res 2
curr_rover_x:   .res 1
curr_rover_y:   .res 1
curr_rover_h:   .res 1

.segment "OAM"
.assert ((* & $FF) = 0),error,"oam not aligned to page"
oam:            .res 256

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
.byte "ROVER #   :"

rover_start:
.byte "START: "

rover_help1:
.byte $BA,": LEFT   ",$BB,": RIGHT   ",$B0,": MOVE"

sprite_size_x:
.byte $1F, '5', $01, $9C

sprite_size_y:
.byte $1F, '5', $00, $B4

sprite_rover_count:
.byte $3F, '2', $00, $9C

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

.macro PPU_LATCH addr
	lda $2002
	lda #>addr
	sta $2006
	lda #<addr
	sta $2006
.endmacro

main:
	; setup default palettes
	PPU_LATCH $3F00
	ldy #8
	:
		ldx #0
		:
			lda palette, X
			sta $2007
			inx
			cpx #8
			bcc :-
		dey
		bne :--

	; setup variables
	lda #1
	sta clr_screen
	lda #5
	sta grid_size_x
	sta grid_size_y
	lda #2
	sta rover_count

; start NMI
	lda #nmi_on
	sta $2000

	jmp TitleScreen

ClearScreen:
	PPU_LATCH $2800
	ldy #4
	:
		ldx #0
		:
			lda #0
			sta $2007
			inx
			bne :-
		dey
		bne :--

  PPU_LATCH $2B56
	ldx #0
	:
	  lda logo, X
		sta $2007
		inx
		cpx #8
		bne :-

	ldx #0
	lda #0
:
	sta oam, X
	inx
	bne :-
	
	lda #0
	sta clr_screen
	rts

; game screens / loops
.include "title.asm"
.include "setup.asm"
.include "rovers.asm"

PAD_A      = $01
PAD_B      = $02
PAD_SELECT = $04
PAD_START  = $08
PAD_U      = $10
PAD_D      = $20
PAD_L      = $40
PAD_R      = $80

gamepad_poll:
	lda #1
	sta $4016
	lda #0
	sta $4016
	ldx #8
	:
		pha
		lda $4016
		and #%00000011
		cmp #%00000001
		pla
		ror
		dex
		bne :-
	sta gamepad
	lda gamepad
	rts

nmi:
  ; push A, X, and Y to stack
	pha
	txa
  pha
	tya
	pha

  ; check if screen is set to be cleared
	lda #1
	cmp clr_screen
	bne FlagClear
	cmp updating_bg ; ...unless background is still being updated
	beq FlagClear

  ; disable rendering
	lda #0
	sta $2001

  ; disable NMI
	sta $2000

  ; clear the screen
	jsr ClearScreen

	; enable NMI
	lda #nmi_on
	sta $2000
	jmp nmi_end

FlagClear:
	; set mask
	lda #ppu_mask
	sta $2001

	; set scroll
	lda #0
	sta $2005
	sta $2005

	; update sprites
	lda #0
	sta $2003
	lda #>oam
	sta $4014

	; respond to gamepad
	jsr gamepad_poll
	jsr gamepad_poll
	lda gamepad_last
	jne @gamepad_end ; wait for all buttons released

	lda game_mode
	bne :+
	jsr TitleHandleGamepad
	jmp @gamepad_end
:
  lda game_mode
	cmp #1
	bne :+
	jsr SetupHandleGamepad
	jmp @gamepad_end
:
  lda game_mode
	cmp #2
	bne :+
  jsr RoversHandleGamepad
:
@gamepad_end:
	lda gamepad
	sta gamepad_last

nmi_end:
	; pull Y, X, and A from stack
	pla
	tay
	pla
	tax
	pla
	
	rti

irq:
	rti

reset:
	sei
	cld
	ldx #$40
	stx $4017
	ldx $ff
	txs
	ldx #$00
	stx $2000
	stx $2001
	stx $4010
	bit $2002
	:
		bit $2002
		bpl :-
	lda #$00
	tax
	:
		sta $0000, X
		sta $0100, X
		sta $0200, X
		sta $0300, X
		sta $0400, X
		sta $0500, X
		sta $0600, X
		sta $0700, X
		inx
		bne :-
	:
		bit $2002
		bpl :-
	jmp main
