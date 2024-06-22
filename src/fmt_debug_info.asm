
include "hw.inc"
include "acharmap.inc"
;include "serial.inc"
;include "packet.inc"


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
strDown: StrDB "down"
strIdle: StrDB "idle"
strActive: StrDB "actv"
strDone: StrDB "done"
strTimeout: StrDB "T-O!"


strHshkConnected: StrDB "Conn OK"
strHshkWorking:   StrDB "Working"
strHshkInit:      StrDB "-------"
strHshkAborted:   StrDB "Aborted"


strPkstNull:    StrDB "NUL"
strPkstPrep:    StrDB "PRE"
strPkstStopped: StrDB "STP"
strPkstReady:   StrDB "RDY"
strPkstXfer:    StrDB "XFR"
strPkstCheck:   StrDB "CHK"
strPkstOk:      StrDB "OK!"
strPkstError:   StrDB "ERR"

strPktCheckOk:   StrDB "Chk^yes^"
strPktCheckFail: StrDB "Chk^no^"

section "fmt_debug_info", rom0

; @param A: value
; @param HL: &dest
; @mut: AF, BC, DE, HL
fmt_sio_config::
	bit SCB_SOURCE, a
	jr nz, .intclk
.extclk
	ld a, "e"
	ld [hl+], a
	ld a, "x"
	ld [hl+], a
	jr .end
.intclk
	ld a, "i"
	ld [hl+], a
	ld a, "n"
	ld [hl+], a
.end
	ld a, "c"
	ld [hl+], a
	ld a, "l"
	ld [hl+], a
	ld a, "k"
	ld [hl+], a
	ret


; @param A: value
; @param HL: &dest
; @mut: AF, BC, DE, HL
fmt_sio_state::
	; { i_dle, S_tarted, ^yes^_ompleted, ^no^_ailed }
	cp SIO_IDLE
	jr z, .idle
	cp SIO_FAILED
	jr z, .failed
	cp SIO_DONE
	jr z, .done
	cp SIO_XFER_STARTED
	jr z, .started
.unknown
	ld b, a
	jp utile_print_h8
.idle
	ld a, "I"
	ld [hl+], a
	jr .end
.failed
	ld a, "^no^"
	ld [hl+], a
	jr .end
.done
	ld a, "^yes^"
	ld [hl+], a
	jr .end
.started
	ld a, ">"
	ld [hl+], a
	jr .end
.end
	ld a, " "
	ld [hl+], a
	ret


; @param B: Tx value
; @param C: Rx value
fmt_txrx::
	ld a, "^tx^"
	ld [hl+], a
	call utile_print_h8
	ld a, " "
	ld [hl+], a
	ld a, "^rx^"
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
	jp print_bracketed_h8


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


; @param A: value
; @param HL: &dest
; @mut: AF, BC, DE, HL
fmt_hshk_status::
	ret


; @param A: value
; @param HL: &dest
; @mut: AF, BC, DE, HL
fmt_hshk_count::
	ld b, a
	ld a, "(" :: ld [hl+], a
	call utile_print_hmin
	ld a, ")" :: ld [hl+], a
	ret


; @param A: value
; @param HL: &dest
; @mut: AF, HL
fmt_packet_state::
	ret


; @param A: value
; @param HL: &dest
; @mut: AF, B, HL
fmt_packet_check::
	ret


print_bracketed_h8:
	ld a, "\{"
	ld [hl+], a
	call utile_print_h8
	ld a, "}"
	ld [hl+], a
	ret

;
; vim:ft=rgbasm
