RoverScreen:
  lda #0
  sta next_rover
  sta rovers_ad_help

  lda curr_rover
  cmp rover_count
  bne :+
    lda #3
    sta game_mode
    jmp ResultsScreen

: PPU_OFF
  DRAW_CLR
  DRAW_ROM 0, 10, $2882, rover_number
  DRAW_ROM 13, 6, $28E2, rover_start
  DRAW_ROM 22, 8, $2B56, logo

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

  PPU_ON

WaitForRovers:
  jsr DoFrame
  lda next_rover
  jne RoverScreen
  jsr UpdateSpritesRovers
  jmp WaitForRovers ; infinite loop

UpdateSpritesRovers:
  lda #0
  cmp rovers_state
  bne :+
    lda #1
    sta oam+6 ; change the palette of the selected menu item
    lda #0
    sta oam+10 ; reset the palette of the unselected menu items
    sta oam+14
    jmp EndSetColorRovers
  :
  lda #1
  cmp rovers_state
  bne :+
    lda #1
    sta oam+10
    lda #0
    sta oam+6
    sta oam+14
    jmp EndSetColorRovers
  :
  lda #2
  cmp rovers_state
  bne :+
    lda #1
    sta oam+14
    lda #0
    sta oam+6
    sta oam+10
    jmp EndSetColorRovers
  : 
  lda #3
  cmp rovers_state
  jne EndSetColorRovers
    lda #0
    sta oam+6
    sta oam+10
    sta oam+14
    lda rovers_ad_help
    jne @help_visible

    ; print help text to screen
    DRAW_ROM 0, 28, $2AC2, rover_help1
    DRAW_ROM 31, 18, $2AEC, rover_help2
    DRAW_ROM 52, 13, $2942, rover_instructions

    lda #1
    sta rovers_ad_help

@help_visible:
    PPU_LATCH $298A
    ldx #0
    : lda curr_rover_ins, X
      sta $2007
      inx
      cmp #0
      bne :-
    lda #0
    sta $2005
    sta $2005

EndSetColorRovers:
  lda curr_rover
  adc #$30
  sta oam+1
  lda curr_rover_x
  clc
  adc #$30
  sta oam+5
  lda curr_rover_y
  adc #$30
  sta oam+9
  ldy curr_rover_h
  lda headings, Y
  sta oam+13

  rts

RoversHandleGamepad:
  lda rovers_state
  cmp #3
  bne :+ ; currently inputting rover instructions?
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
      cmp grid_size_x
      jeq RoversButtonHandled
      inc curr_rover_x
      jmp RoversButtonHandled
IncCurrRoverY:
    lda rovers_state
    cmp #1
    bne IncCurrRoverH
      lda curr_rover_y
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
    lda curr_rover_ptr
    cmp #<curr_rover_ins
    beq :+
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
  lda curr_rover
  jsr SetCurrRoverPtr

  ; move the next 40 bytes
  ldy #0
  : cpy #40
  beq @copy_done
  lda curr_rover_x, Y
  sta (curr_rover_ptr), Y
  lda #0
  sta curr_rover_x, Y
  iny
  jmp :-
  
@copy_done:
  lda #0
  sta rovers_state
  sta curr_rover_ptr+1
  lda #<curr_rover_ins
  sta curr_rover_ptr
  rts