
include "hw.inc"


section "Header", rom0[$100]
	di
if def(TESTS)
	jp Tests_EntryPoint
else
	jp EntryPoint
endc

	; Make sure to allocate some space for the header, so no important
	; code gets put there and later overwritten by RGBFIX.
	; RGBFIX is designed to operate over a zero-filled header, so make
	; sure to put zeros regardless of the padding value. (This feature
	; was introduced in RGBDS 0.4.0, but the -MG etc flags were also
	; introduced in that version.)
	ds $150 - @, 0


section "Entry point", rom0

EntryPoint:
	call lcd_off

	; Palettes!
	ld a, %11_10_01_00
	ld hl, rBGP
	ld [hl+], a
	ld a, %10_01_00_11
	ld [hl+], a
	ld a, %11_00_01_11
	ld [hl+], a

	call input_init
	call vblank_init
	ld a, LCDCF_OBJON | LCDCF_BGON
	ld [hLCDC], a
	ei

	jp main


lcd_off::
:
	ldh a, [rLY]
	cp 144
	jr c, :-
	ldh a, [rLCDC]
	res LCDCB_ON, a
	ldh [rLCDC], a
	ret


lcd_on::
	ldh a, [hLCDC]
	set LCDCB_ON, a
	ldh [hLCDC], a
	ldh [rLCDC], a
	ret
