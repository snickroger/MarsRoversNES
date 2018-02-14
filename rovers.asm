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
	bne :+
		lda #1
	  ldx #10
		sta oam, X
	  lda #0
	  ldx #2
		sta oam, X
	  ldx #6
		sta oam, X
		jmp EndSetColorRovers
	: 
	lda #3
	cmp rovers_state
	bne EndSetColorRovers
	  lda $2002
		rol
		bcc EndSetColorRovers
		clc
	  PPU_LATCH $2AC2
	  ldx #0
	  :
	    lda rover_help1, X
		  sta $2007
		  inx
		  cpx #28
		  bne :-
		; set scroll
		lda #0
		sta $2005
		sta $2005
EndSetColorRovers:
  lda curr_rover_x
  clc
  adc #$30
  ldx #1
  sta oam, X
  lda curr_rover_y
  adc #$30
  ldx #5
  sta oam, X
  ldy curr_rover_h
  ldx #9
  lda headings, Y
  sta oam, X

  rts

RoversHandleGamepad:
  lda gamepad
	cmp #PAD_R
	bne :+
	  lda rovers_state
	  cmp #2
		jeq RoversButtonHandled
		inc rovers_state
	:
  lda gamepad
	cmp #PAD_L
	bne :+
	  lda rovers_state
		jeq RoversButtonHandled
		dec rovers_state
	:
  lda gamepad
  cmp #PAD_U
  bne :+
    lda rovers_state
    cmp #0
    bne IncCurrRoverY
      lda curr_rover_x
      clc
      adc #1
      cmp grid_size_x
      jeq RoversButtonHandled
      inc curr_rover_x
      jmp RoversButtonHandled
IncCurrRoverY:
    lda rovers_state
    cmp #1
    bne IncCurrRoverH
      lda curr_rover_y
      clc
      adc #1
      cmp grid_size_y
      jeq RoversButtonHandled
      inc curr_rover_y
      jmp RoversButtonHandled
IncCurrRoverH:
    lda rovers_state
    cmp #2
    bne RoversButtonHandled
      lda curr_rover_h
      clc
      adc #1
      cmp #4
      jeq RoversButtonHandled
      inc curr_rover_h
      jmp RoversButtonHandled
	:
  lda gamepad
  cmp #PAD_D
  bne :+
    lda rovers_state
    cmp #0
    bne DecCurrRoverY
      lda curr_rover_x
      cmp #0
      jeq RoversButtonHandled
      dec curr_rover_x
      jmp RoversButtonHandled
DecCurrRoverY:
    lda rovers_state
    cmp #1
    bne DecCurrRoverH
      lda curr_rover_y
      cmp #0
      jeq RoversButtonHandled
      dec curr_rover_y
      jmp RoversButtonHandled
DecCurrRoverH:
    lda rovers_state
    cmp #2
    bne RoversButtonHandled
      lda curr_rover_h
      cmp #0
      jeq RoversButtonHandled
      dec curr_rover_h
      jmp RoversButtonHandled
  :
  lda gamepad
	cmp #PAD_A
	bne :+
    lda #3
    sta rovers_state
    ldx #0
    lda curr_rover_x
    sta rover1, X
    inx
    lda curr_rover_y
    sta rover1, X
    inx
    lda curr_rover_h
    sta rover1, X
  :
RoversButtonHandled:
	rts