RoverScreen:
  lda #0
	sta next_rover

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
		cpx #10
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
  lda sprite_rover_current, X
	sta oam, Y
	inx
	iny
	cpx #4
	bne :-
	ldx #0
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
  lda next_rover
	jne RoverScreen
  jsr UpdateSpritesRovers
	jmp WaitForRovers ; infinite loop

UpdateSpritesRovers:
	lda $2002
	rol
	bcs BeginUpdateSpritesRovers
	rts
BeginUpdateSpritesRovers:
	clc
	lda #0
	cmp rovers_state
	bne :+
		lda #1
	  ldx #6
		sta oam, X
	  lda #0
	  ldx #10
		sta oam, X
	  ldx #14
		sta oam, X
		jmp EndSetColorRovers
	:
	lda #1
	cmp rovers_state
	bne :+
		lda #1
	  ldx #10
		sta oam, X
	  lda #0
	  ldx #6
		sta oam, X
	  ldx #14
		sta oam, X
		jmp EndSetColorRovers
	:
	lda #2
	cmp rovers_state
	bne :+
		lda #1
	  ldx #14
		sta oam, X
	  lda #0
	  ldx #6
		sta oam, X
	  ldx #10
		sta oam, X
		jmp EndSetColorRovers
	: 
	lda #3
	cmp rovers_state
	jne EndSetColorRovers
		lda #0
	  ldx #6
		sta oam, X
	  ldx #10
		sta oam, X
	  ldx #14
		sta oam, X

	  PPU_LATCH $2AC2
	  ldx #0
	  :
	    lda rover_help1, X
		  sta $2007
		  inx
		  cpx #28
		  bne :-

	  PPU_LATCH $2AEC
	  ldx #0
	  :
	    lda rover_help2, X
		  sta $2007
		  inx
		  cpx #8
		  bne :-

	  PPU_LATCH $2AF5
	  ldx #0
	  :
	    lda rover_help3, X
		  sta $2007
		  inx
		  cpx #9
		  bne :-

		PPU_LATCH $2942
	  ldx #0
	  :
	    lda rover_instructions, X
		  sta $2007
		  inx
		  cpx #13
		  bne :-

	  PPU_LATCH $298A
	  ldx #0
	  :
	    lda curr_rover_ins, X
		  sta $2007
		  inx
			cmp #0
		  bne :-
		
		; reset scroll
		lda #0
		sta $2005
		sta $2005
EndSetColorRovers:
  lda curr_rover
	adc #$30
	ldx #1
	sta oam, X
	lda curr_rover_x
  clc
  adc #$30
  ldx #5
  sta oam, X
  lda curr_rover_y
  adc #$30
  ldx #9
  sta oam, X
  ldy curr_rover_h
  ldx #13
  lda headings, Y
  sta oam, X

  rts

RoversHandleGamepad:
  lda rovers_state
	cmp #3
	bne :+
	jmp AddToInstructions
  :
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
		lda #<curr_rover_ins
		sta curr_rover_ptr
  :
RoversButtonHandled:
	rts

AddToInstructions:
  ldx #0
	lda gamepad
	cmp #PAD_L
	bne :+
	  lda #'L'
		sta (<curr_rover_ptr, X)
		inc curr_rover_ptr
	:
	lda gamepad
	cmp #PAD_R
	bne :+
	  lda #'R'
		sta (<curr_rover_ptr, X)
		inc curr_rover_ptr
	:
	lda gamepad
	cmp #PAD_A
	bne :+
	  lda #'M'
		sta (<curr_rover_ptr, X)
		inc curr_rover_ptr
	:
	lda gamepad
	cmp #PAD_B
	bne :+
	  lda #0
		dec curr_rover_ptr
		sta (<curr_rover_ptr, X)
	:
	lda gamepad
	cmp #PAD_START
	bne :+
		sta next_rover
		inc curr_rover
		jsr ClearInstructionsInput
	:
	jmp RoversButtonHandled

ClearInstructionsInput:
  ldx #0
	lda #0
	sta curr_rover_x
	sta curr_rover_y
	sta curr_rover_h
	sta rovers_state
	:
	  sta curr_rover_ins, X
		inx
	  cpx #36
		bne :-
	rts