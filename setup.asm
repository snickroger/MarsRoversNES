SetupScreen:
	lda #1
	sta clr_screen
SetupWaitForClear:
  cmp clr_screen
	beq SetupWaitForClear

	lda #1
	sta updating_bg

	PPU_LATCH $2882
	ldx #0
	:
	  lda size_of_plateau, X
		sta $2007
		inx
		cpx #20
		bne :-

	PPU_LATCH $2902
	ldx #0
	:
	  lda how_many_rovers, X
		sta $2007
		inx
		cpx #16
		bne :-

	lda #0
	sta updating_bg

	ldx #0
	ldy #0
:
  lda sprite_size_x, X
	sta oam, Y
	inx
	iny
	cpx #4
	bne :-
	ldx #0
:
  lda sprite_size_y, X
	sta oam, Y
	inx
	iny
	cpx #4
	bne :-
	ldx #0
:
  lda sprite_rover_count, X
	sta oam, Y
	inx
	iny
	cpx #4
	bne :-

WaitForSetup:
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
		ldx #2
		lda #0
		sta oam, X
		ldx #6
		lda #1
		sta oam, X
	:
  lda gamepad
	cmp #PAD_L
	bne :+
	  lda #0
		sta setup_state
		ldx #2
		lda #1
		sta oam, X
		ldx #6
		lda #0
		sta oam, X
	:
	lda gamepad
	cmp #PAD_U
	bne :+
    lda setup_state
		bne IncYSize
		IncXSize:
		  ldx #9
			cpx grid_size_x
			beq :+
			inc grid_size_x
			ldx #1
			inc oam, X
			jmp :+
		IncYSize:
		  cmp #1
			bne IncNumRovers
		  ldx #9
			cpx grid_size_y
			beq :+
			inc grid_size_y
			ldx #5
			inc oam, X
			jmp :+
		IncNumRovers:
		  ldx #4
		  cpx rover_count
			beq :+
			inc rover_count
			ldx #9
			inc oam, X
	:
	lda gamepad
	cmp #PAD_D
	bne :+
    lda setup_state
		bne DecYSize
		DecXSize:
		  ldx #1
			cpx grid_size_x
			beq :+
			dec grid_size_x
			ldx #1
			dec oam, X
			jmp :+
		DecYSize:
		  cmp #1
			bne DecNumRovers
		  ldx #1
			cpx grid_size_y
			beq :+
			dec grid_size_y
			ldx #5
			dec oam, X
			jmp :+
		DecNumRovers:
		  ldx #1
		  cpx rover_count
			beq :+
			dec rover_count
			ldx #9
			dec oam, X
  :
  lda gamepad
	cmp #PAD_A
	bne :+
	  lda #2
		cmp setup_state
		beq NextPage
		sta setup_state
		lda #0
		ldx #2
		sta oam, X
		ldx #6
		sta oam, X
		lda #1
		ldx #10
		sta oam, X
		jmp SetupButtonHandled
	NextPage:
	  lda #2
		sta game_mode
		jmp SetupButtonHandled
	:
  lda gamepad
	cmp #PAD_B
	bne :+
	  lda #0
		sta setup_state
		ldx #10
		sta oam, X
		ldx #6
		sta oam, X
		lda #1
		ldx #2
		sta oam, X
	:
SetupButtonHandled:
	rts