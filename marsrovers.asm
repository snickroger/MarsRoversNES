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

game_mode:     .res 1
gamepad:       .res 1
gamepad_last:  .res 1

.segment "OAM"
.assert ((* & $FF) = 0),error,"oam not aligned to page"
oam:     .res 256

; RAM variables
.segment "BSS"

; CODE
.segment "CODE"

color_blue = $02
color_rust = $07
color_gold = $28
color_white = $30

palette:
.byte color_blue, color_rust, color_gold, color_white

press_start:
.byte 'P','R','E','S','S', ' ', 'S','T','A','R','T'

logo:
.byte 'D','E','A','L','E','R',$BC,$BD

size_of_grid:
.byte "SIZE OF GRID?"

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
	ldy #16
	:
		ldx #0
		:
			lda palette, X
			sta $2007
			inx
			cpx #4
			bcc :-
		dey
		bne :--
  
	; setup nametable
	PPU_LATCH $2000
	ldy #16
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

	; start NMI
	lda #%10000010
	sta $2000

TitleScreen:
  PPU_LATCH $2ACA
	ldx #0
	:
	  lda press_start, X
		sta $2007
		inx
		cpx #11
		bne :-

WaitForStart:
  lda game_mode
	bne GameScreen
	jmp WaitForStart ; infinite loop

GameScreen:
  PPU_LATCH $2ACA
	ldx #0
	:
	  lda #0
		sta $2007
		inx
		cpx #11
		bne :-
	
	PPU_LATCH $2882
	ldx #0
	:
	  lda size_of_grid, X
		sta $2007
		inx
		cpx #13
		bne :-

WaitForEnd:
	jmp WaitForEnd ; infinite loop

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
	; update sprites
	lda #0
	sta $2003
	lda #>oam
	sta $4014

	lda #0
	sta $2005
	sta $2005

	; set mask
	lda #%00011110
	sta $2001

	; respond to gamepad
	jsr gamepad_poll
	jsr gamepad_poll
	lda gamepad_last
	jne @gamepad_end ; wait for all buttons released

  lda gamepad
	cmp #PAD_START
	bne :+
		lda #1
		sta game_mode
	:

	@gamepad_end:
	lda gamepad
	sta gamepad_last

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
