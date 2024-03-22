
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


; Copy a contiguous block of bytes from `src` to `dest`.
; The block ends at the first instance of `terminator` read from the `src` region.
; @NOTE: Does **not** copy the terminator byte.
; @param HL: dest
; @param DE: src
; @param C: terminator
; @return DE: last source address read
; @return HL: end of written block
; @mut: AF, DE, HL
memcpy_terminated::
:
	ld a, [de]
	cp c
	ret z
	inc de
	ld [hl+], a
	jr :-
