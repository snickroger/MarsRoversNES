; DealerOn Mars Rovers
; Nick Rogers, 2018
;

.feature force_range
.macpack longbranch

.include "header.asm"

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
	lda #5
	sta grid_size_x
	sta grid_size_y
	lda #2
	sta rover_count
	lda #nmi_on
	sta buf2000
	lda #ppu_mask
	sta buf2001

  ; start NMI
	lda #nmi_on
	sta $2000

	jmp TitleScreen

; game screens / loops
.include "title.asm"
.include "setup.asm"
.include "rovers.asm"
.include "results.asm"

PAD_A      = $01
PAD_B      = $02
PAD_SELECT = $04
PAD_START  = $08
PAD_U      = $10
PAD_D      = $20
PAD_L      = $40
PAD_R      = $80

HandleGamepad:
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
  lda game_mode
	cmp #3
	bne :+
  jsr ResultsHandleGamepad
:
@gamepad_end:
	lda gamepad
	sta gamepad_last
  rts

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

WaitFrame:
	inc sleeping
	@loop:
		lda sleeping
		bne @loop
	rts

DoDrawing:
  ldx #0
	@draw_more:
		ldy draw, X
		beq @end_draw
		inx
		lda $2002
		lda draw, X
		sta $2006
		inx
		lda draw, X
		sta $2006
		inx
			:	lda draw, X
			sta $2007
			dey
			inx
			cpy #0
			bne :-
		jmp @draw_more
  @end_draw:
	lda #0
	sta draw
  rts

DoFrame:
	lda #1
	sta update_bg
	sta update_sprites
	sta update_ppu
	jsr WaitFrame
	jsr HandleGamepad
	rts

SetCurrRoverPtr:
	lda curr_rover
	cmp #1
	bne :+
	  lda #<rover1
		sta curr_rover_ptr
		lda #>rover1
		sta curr_rover_ptr+1
	: 
	lda curr_rover
	cmp #2
	bne :+
	  lda #<rover2
		sta curr_rover_ptr
		lda #>rover2
		sta curr_rover_ptr+1
	: 
	lda curr_rover
	cmp #3
	bne :+
	  lda #<rover3
		sta curr_rover_ptr
		lda #>rover3
		sta curr_rover_ptr+1
	: 
	lda curr_rover
	cmp #4
	bne :+
	  lda #<rover4
		sta curr_rover_ptr
		lda #>rover4
		sta curr_rover_ptr+1
	: rts

nmi:
  ; push A, X, and Y to stack
	pha
	txa
  pha
	tya
	pha

  lda update_sprites
	beq :+
	  lda #0
		sta $2003
		lda #>oam
		sta $4014
	:
	lda update_bg
	beq :+
	  bit $2002
		jsr DoDrawing
		dec update_bg
	:
	lda update_ppu
	beq :+
	  lda buf2001
		sta $2001
		lda buf2000
		sta $2000
	:
  lda #0
	sta sleeping

	lda #0
	sta $2005
	sta $2005

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
