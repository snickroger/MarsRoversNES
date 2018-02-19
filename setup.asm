SetupScreen:
  PPU_OFF
  DRAW_CLR
	DRAW_ROM 0, 8, $2B56, logo
  DRAW_ROM 12, 20, $2882, size_of_plateau
  DRAW_ROM 36, 16, $2902, how_many_rovers
: lda sprite_size_x, X
	sta oam, Y
	inx
	iny
	cpx #4
	bne :-
	ldx #0
: lda sprite_size_y, X
	sta oam, Y
	inx
	iny
	cpx #4
	bne :-
	ldx #0
: lda sprite_rover_count, X
	sta oam, Y
	inx
	iny
	cpx #4
	bne :-
	
	PPU_ON
	
WaitForSetup:
  jsr DoFrame
  lda game_mode
	cmp #2
	jeq RoverScreen
	jmp WaitForSetup ; infinite loop

SetupHandleGamepad:
  lda gamepad
	cmp #PAD_R
	bne :+
	  lda #1
		sta setup_state
		lda #0
		sta oam+2
		lda #1
		sta oam+6
	:
  lda gamepad
	cmp #PAD_L
	bne :+
	  lda #0
		sta setup_state
		lda #1
		sta oam+2
		lda #0
		sta oam+6
	:
	lda gamepad
	cmp #PAD_U
	bne :+
    lda setup_state
		bne @incYSize
		@incXSize:
		  ldx #9
			cpx grid_size_x
			beq :+
			inc grid_size_x
			inc oam+1
			jmp :+
		@incYSize:
		  cmp #1
			bne @incNumRovers
		  ldx #9
			cpx grid_size_y
			beq :+
			inc grid_size_y
			inc oam+5
			jmp :+
		@incNumRovers:
		  ldx #4
		  cpx rover_count
			beq :+
			inc rover_count
			inc oam+9
	:	lda gamepad
	cmp #PAD_D
	bne :+
    lda setup_state
		bne @decYSize
		@decXSize:
		  ldx #1
			cpx grid_size_x
			beq :+
			dec grid_size_x
			dec oam+1
			jmp :+
		@decYSize:
		  cmp #1
			bne @decNumRovers
		  ldx #1
			cpx grid_size_y
			beq :+
			dec grid_size_y
			dec oam+5
			jmp :+
		@decNumRovers:
		  ldx #1
		  cpx rover_count
			beq :+
			dec rover_count
			dec oam+9
  : lda gamepad
	cmp #PAD_A
	bne :+
	  lda #2
		cmp setup_state
		beq @nextPage
		sta setup_state
		lda #0
		sta oam+2
		sta oam+6
		lda #1
		sta oam+10
		jmp SetupButtonHandled
	@nextPage:
	  lda #2
		sta game_mode
		jmp SetupButtonHandled
	: lda gamepad
	cmp #PAD_B
	bne SetupButtonHandled
	  lda #0
		sta setup_state
		sta oam+10
		sta oam+6
		lda #1
		sta oam+2
SetupButtonHandled:
	rts