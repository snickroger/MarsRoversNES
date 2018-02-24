ResultsScreen:
  PPU_OFF
  DRAW_CLR
  DRAW_ROM 0, 8, $2B56, logo
  PPU_ON

  WaitForReset:
    jsr DoFrame
    jmp WaitForReset