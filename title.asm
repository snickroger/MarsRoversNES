TitleScreen:
  DRAW_CLR
  DRAW_ROM 0, 11, $2ACA, press_start
	DRAW_ROM 14, 8, $2B56, logo

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