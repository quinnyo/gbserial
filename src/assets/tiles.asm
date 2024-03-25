
section "bgtiles", rom0
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

section "obtiles", rom0
_blob0:
dw `03333330
dw `31111113
dw `31000013
dw `31000013
dw `31000013
dw `31000013
dw `31111113
dw `03333330
_blob1:
dw `00333300
dw `03111130
dw `31000013
dw `31000013
dw `31000013
dw `31000013
dw `03111130
dw `00333300
_blob2:
dw `00000000
dw `00033000
dw `00311300
dw `03100130
dw `03100130
dw `00311300
dw `00033000
dw `00000000
_blob3:
dw `00000000
dw `00000000
dw `00031300
dw `00010100
dw `00031300
dw `00000000
dw `00000000
dw `00000000
_blob4:
dw `00000000
dw `00020000
dw `00213000
dw `00030000
dw `00000000
dw `00000000
dw `00000000
dw `00000000


section "load_tiles", rom0
; assumes LCD is off
load_tiles::
	ld de, startof("obtiles")
	ld bc, sizeof("obtiles")
	ld hl, $8000
	call memcpy
	ld de, startof("bgtiles")
	ld bc, sizeof("bgtiles")
	ld hl, $8800
	jp memcpy

