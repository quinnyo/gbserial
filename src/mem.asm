
section "mem", rom0

_memcpy_loop:
	ld a, [de]
	inc de
	ld [hl+], a
	dec bc
; @param BC: len
; @param DE: src
; @param HL: dest
memcpy::
	ld a, c
	or b
	jr nz, _memcpy_loop
	ret
