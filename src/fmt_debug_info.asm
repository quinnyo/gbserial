
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
strIdle: StrDB "idle"
strWorking: StrDB "work"
strDone: StrDB "done"
strTimeout: StrDB "T-O!"
strUnknown: StrDB "??"


section "fmt_debug_info", rom0
; @param A: value
; @param HL: &dest
; @mut: AF, BC, DE, HL
fmt_serio_status::
	ld c, $FF ; str terminator
	ld de, strIdle
	cp SERIO_IDLE
	jp z, memcpy_terminated

	ld de, strWorking
	cp SERIO_WORKING
	jp z, memcpy_terminated

	ld de, strDone
	cp SERIO_DONE
	jp z, memcpy_terminated

	ld de, strTimeout
	cp SERIO_TIMEOUT
	jp z, memcpy_terminated

	call utile_print_h8
	ld de, strUnknown
	jp memcpy_terminated


; @param B: Tx value
; @param C: Rx value
fmt_txrx::
	ld a, "t"
	ld [hl+], a
	call utile_print_h8
	ld a, " "
	ld [hl+], a
	ld a, "r"
	ld [hl+], a
	ld b, c
	jp utile_print_h8


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
	ld a, ","
	ld [hl+], a
	LoadBitSwitch a, 0, b, "0", "C"
	ld [hl+], a
	ld a, "}"
	ld [hl+], a
	ret
