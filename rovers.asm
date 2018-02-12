
RoverScreen:
	lda #1
	sta clr_screen
RoverWaitForClear:
  cmp clr_screen
	beq RoverWaitForClear

	lda #1
	sta updating_bg

	PPU_LATCH $2882
	ldx #0
	:
	  lda rover_number, X
		sta $2007
		inx
		cpx #11
		bne :-

	PPU_LATCH $28E2
	ldx #0
	:
	  lda rover_start, X
		sta $2007
		inx
		cpx #6
		bne :-

	PPU_LATCH $2AC2
	ldx #0
	:
	  lda rover_help1, X
		sta $2007
		inx
		cpx #28
		bne :-

	ldx #0
	ldy #0
:
  lda sprite_start_x, X
	sta oam, Y
	inx
	iny
	cpx #4
	bne :-
	ldx #0
:
  lda sprite_start_y, X
	sta oam, Y
	inx
	iny
	cpx #4
	bne :-
	ldx #0
:
  lda sprite_start_h, X
	sta oam, Y
	inx
	iny
	cpx #4
	bne :-

	lda #0
	sta updating_bg

WaitForRovers:
  jsr UpdateSpritesRovers
	jmp WaitForRovers ; infinite loop

UpdateSpritesRovers:
	lda #0
	cmp rovers_state
	bne :+
		lda #1
	  ldx #2
		sta oam, X
	  lda #0
	  ldx #6
		sta oam, X
	  ldx #10
		sta oam, X
		jmp EndSetColorRovers
	:
	lda #1
	cmp rovers_state
	bne :+
		lda #1
	  ldx #6
		sta oam, X
	  lda #0
	  ldx #2
		sta oam, X
	  ldx #10
		sta oam, X
		jmp EndSetColorRovers
	:
	lda #2
	cmp rovers_state
	bne EndSetColorRovers
		lda #1
	  ldx #10
		sta oam, X
	  lda #0
	  ldx #2
		sta oam, X
	  ldx #6
		sta oam, X
		jmp EndSetColorRovers
EndSetColorRovers:
  rts

RoversHandleGamepad:
  lda gamepad
	cmp #PAD_R
	bne :+
	  lda rovers_state
	  cmp #2
		beq RoversButtonHandled
		inc rovers_state
	:
  lda gamepad
	cmp #PAD_L
	bne :+
	  lda rovers_state
		beq RoversButtonHandled
		dec rovers_state
	:
RoversButtonHandled:
	rts