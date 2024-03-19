
section "tiles", rom0
rept 8
	dw `00000000
endr
rept 8
	dw `11111111
endr
rept 8
	dw `22222222
endr
rept 8
	dw `33333333
endr


section "load_tiles", rom0
; assumes LCD is off
load_tiles::
	ld de, startof("tiles")
	ld bc, sizeof("tiles")
	ld hl, $8800
	jp memcpy

