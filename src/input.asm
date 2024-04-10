
def rcP1 equ $00
def P1F_READ_DIR equ 1 << 5
def P1F_READ_BTN equ 1 << 4
def P1F_RESET equ P1F_READ_DIR | P1F_READ_BTN


section "hKeys", hram
hKeys:: db
hKeysPressed:: db
hKeysReleased:: db


section "input", rom0

input_init::
	ld hl, hKeys
	xor a
	ld [hl+], a
	ld [hl+], a
	ld [hl+], a
	ret


input_update::
.read:
	ld c, rcP1
	ld a, P1F_READ_DIR :: ldh [c], a
	ld hl, hKeys
	ld d, [hl]
	ldh a, [c]
	ld b, a
	ld a, P1F_READ_BTN :: ldh [c], a
	ld a, b
	or $F0
	swap a
	ld b, a
	ldh a, [c]
	or $F0
	xor b
	ld b, a
	ld a, P1F_RESET :: ldh [c], a
.update:
	ld a, b
	ld [hl+], a
	xor d
	ld c, a
	and b
	ld [hl+], a
	ld a, b
	cpl
	and c
	ld [hl], a
	ret




;
; vim:ft=rgbasm
