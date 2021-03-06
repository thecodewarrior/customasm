PPU_CTRL    = 0x2000
PPU_MASK    = 0x2001
PPU_STATUS  = 0x2002
PPU_ADDR    = 0x2006
PPU_DATA    = 0x2007
APU_DMC     = 0x4010
APU_FRMCNTR = 0x4017

PPU_CTRL_NMI = 0b10000000

PPU_MASK_LEFTBKG = 0b00000010
PPU_MASK_LEFTSPR = 0b00000100
PPU_MASK_SHOWBKG = 0b00001000
PPU_MASK_SHOWSPR = 0b00010000

VRAM_PALETTE = 0x3f00