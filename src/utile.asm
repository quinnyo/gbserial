
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


; Print a byte as one or two hexadecimal digits.
; If the high nibble is zero, it is omitted.
; @param B: value
; @param HL: &dest
; @mut: AF, HL
utile_print_hmin::
	ld a, b
	and $F0
	jr z, utile_print_h4
	jr utile_print_h8
