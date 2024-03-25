
include "hw.inc"
include "acharmap.inc"
include "serial.inc"


; LoadBitSwitch Q, u, R, c0, c1
; Load Q with c1 if R.u is set, or c0 if not.
macro LoadBitSwitch
	bit \2, \3
	ld \1, \4
	jr z, :+
	ld \1, \5
:
endm

macro StrDB
	db \#
	db $FF
endm


section "fmt_debug_info_data", rom0
strSerio: StrDB "ser"
strError: StrDB "err"
strOk:    StrDB " ok"

section "fmt_debug_info", rom0
; @param B: value
; @param HL: &dest
; @mut: AF, BC, DE, HL
fmt_serio_status::
	ld c, $FF ; str terminator
	ld de, strSerio
	call memcpy_terminated

	; LoadBitSwitch a, SERIOB_TXRX, b, "R", "T"
	; ld [hl+], a
	; ld a, "x"
	; ld [hl+], a
	ld a, " "
	ld [hl+], a
	LoadBitSwitch a, SERIOB_WORKING, b, ".", "W"
	ld [hl+], a
	LoadBitSwitch a, SERIOB_SYNC, b, "-", "S"
	ld [hl+], a
	LoadBitSwitch de, SERIOB_ERROR, b, strOk, strError
	ld c, $FF ; str terminator
	call memcpy_terminated
	ld a, " "
	ld [hl+], a

	LoadBitSwitch c, SERIOB_TXRX, b, "<", ">"

	ld a, "r"
	ld [hl+], a
	ldh a, [_serio_rx]
	ld b, a
	call utile_print_h8

	ld a, c
	ld [hl+], a

	ld a, "t"
	ld [hl+], a
	ldh a, [_serio_tx]
	ld b, a
	call utile_print_h8
	ret


; @param B: value
; @param HL: &dest
; @mut: AF, HL
fmt_SB::
	ld a, "S"
	ld [hl+], a
	ld a, "B"
	ld [hl+], a
	ld a, "\{"
	ld [hl+], a
	call utile_print_h8
	ld a, "}"
	ld [hl+], a
	ret


; @param B: value
; @param HL: &dest
; @mut: AF, HL
fmt_SC::
	ld a, "S"
	ld [hl+], a
	ld a, "C"
	ld [hl+], a
	ld a, "\{"
	ld [hl+], a
	LoadBitSwitch a, 7, b, "0", "1"
	ld [hl+], a
	ld a, "/"
	ld [hl+], a
	LoadBitSwitch a, 0, b, "0", "C"
	ld [hl+], a
	ld a, "}"
	ld [hl+], a
	ret
