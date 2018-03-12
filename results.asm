ResultsScreen:
  PPU_OFF

  DRAW_CLR
  DRAW_ROM 0, 8, $2B56, logo
  ldy #4
  ldx #0
  lda #0
  : sta oam, X
    inx
    sta oam, X
    inx
    sta oam, X
    inx
    sta oam, X
    inx
    dey
    bne :-

  jsr CalculateResults

  ldx #0
  ; print rover 1
  PPU_LATCH $2882
  lda rover1
  sta $2007
  PPU_LATCH $2884
  lda rover1+1
  sta $2007
  PPU_LATCH $2886
  lda rover1+2
  sta $2007
  
  inx
  cpx rover_count
  jeq @done_printing
  ; print rover 2
  PPU_LATCH $28C2
  lda rover2
  sta $2007
  PPU_LATCH $28C4
  lda rover2+1
  sta $2007
  PPU_LATCH $28C6
  lda rover2+2
  sta $2007

  inx
  cpx rover_count
  jeq @done_printing
  ; print rover 3
  PPU_LATCH $2902
  lda rover3
  sta $2007
  PPU_LATCH $2904
  lda rover3+1
  sta $2007
  PPU_LATCH $2906
  lda rover3+2
  sta $2007

  inx
  cpx rover_count
  jeq @done_printing
  ; print rover 4
  PPU_LATCH $2942
  lda rover4
  sta $2007
  PPU_LATCH $2944
  lda rover4+1
  sta $2007
  PPU_LATCH $2946
  lda rover4+2
  sta $2007
  
@done_printing:
  lda #0
  sta $2005
  sta $2005

  PPU_ON

WaitForReset:
  jsr DoFrame
  jmp WaitForReset

ResultsHandleGamepad:
  lda gamepad
  cmp #PAD_START
  bne :+
    jmp reset
  : rts

CalculateResults:
  lda #0
  sta curr_rover
NextRover:
  inc curr_rover
  jsr SetCurrRoverPtr
  ldy #0
  lda (curr_rover_ptr), Y
  sta curr_rover_x
  iny
  lda (curr_rover_ptr), Y
  sta curr_rover_y
  iny
  lda (curr_rover_ptr), Y
  sta curr_rover_h
  iny
@next_ins:
  iny
  lda (curr_rover_ptr), Y
  cmp #'L'
  bne :+
    jsr TurnLeft
    sta curr_rover_h
    jmp @next_ins
: cmp #'R'
  bne :+
    jsr TurnRight
    sta curr_rover_h
    jmp @next_ins
: cmp #'M'
  bne :+
    jsr MoveRover
    jmp @next_ins
:
  ldy #0
  lda curr_rover_x
  clc
  adc #$30
  sta (curr_rover_ptr), Y
  iny
  lda curr_rover_y
  adc #$30
  sta (curr_rover_ptr), Y
  iny
  jsr GetHeadingChar
  sta (curr_rover_ptr), Y
  lda curr_rover
  cmp rover_count
  bne NextRover
  rts

TurnLeft:
  lda curr_rover_h
  cmp #0
  bne :+
    lda #3
    rts
: sbc #1
  rts

TurnRight:
  lda curr_rover_h
  cmp #3
  bne :+
    lda #0
    rts
: adc #1
  rts

MoveRover:
  lda curr_rover_h
  cmp #0 ; North (Y+1)
  bne :+
    lda curr_rover_y
    cmp grid_size_y
    beq @skip_move
    inc curr_rover_y
    rts
  : cmp #1 ; East (X+1)
  bne :+
    lda curr_rover_x
    cmp grid_size_x
    beq @skip_move
    inc curr_rover_x
    rts
  : cmp #2 ; South (Y-1)
  bne :+
    lda curr_rover_y
    beq @skip_move
    dec curr_rover_y
    rts
  : ; West (X-1)
    lda curr_rover_x
    beq @skip_move
    dec curr_rover_x
@skip_move:
    rts

GetHeadingChar:
  lda curr_rover_h
  cmp #0 ; North (Y+1)
  bne :+
    lda #$4E
    rts
  : cmp #1 ; East (X+1)
  bne :+
    lda #$45
    rts
  : cmp #2 ; South (Y-1)
  bne :+
    lda #$53
    rts
  : ; West (X-1)
    lda #$57
    rts
    