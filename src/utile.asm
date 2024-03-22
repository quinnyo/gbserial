
section "utile", rom0

; print byte value as two hexadecimal digits
; @param B: value
; @param HL: &dest
; @mut: AF, HL
utile_print_h8::
	ld a, b
	swap a
	and $0F
	ld [hl+], a

	; FALLTHROUGH

; print low half of a byte as a hexadecimal digit
; @param B: value
; @param HL: &dest
; @mut: AF, HL
utile_print_h4::
	ld a, b
	and $0F
	ld [hl+], a
	ret
