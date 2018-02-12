TitleScreen:
	lda #1
	sta clr_screen
TitleWaitForClear:
  cmp clr_screen
	beq TitleWaitForClear
	
	lda #1
	sta updating_bg

  PPU_LATCH $2ACA
	ldx #0
	:
	  lda press_start, X
		sta $2007
		inx
		cpx #11
		bne :-

  lda #0
	sta updating_bg
WaitForStart:
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