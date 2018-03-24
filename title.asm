TitleScreen:
  DRAW_CLR
  DRAW_ROM 0, 11, $2ACA, press_start
	DRAW_ROM 14, 8, $2B56, logo
	DRAW_ROM 25, 8, $2887, title_row1
	DRAW_ROM 36, 8, $28A7, title_row2
	DRAW_ROM 47, 21, $28C7, title_row3
	DRAW_ROM 71, 22, $28E7, title_row4
	jsr DoFrame
  DRAW_ROM 0, 27, $2902, title_row5
	DRAW_ROM 30, 28, $2921, title_row6
	DRAW_ROM 61, 30, $2941, title_row7
	jsr DoFrame
	DRAW_ROM 0, 30, $2961, title_row8
	DRAW_ROM 33, 30, $2981, title_row9
	DRAW_ROM 66, 30, $29A1, title_row10
  PPU_ON
WaitForStart:
  jsr DoFrame
  lda game_mode
	bne SetupScreen
	jmp WaitForStart ; infinite loop

TitleHandleGamepad:
  lda gamepad
	cmp #PAD_START
	bne :+
		lda #1
		sta game_mode
	:
	rts