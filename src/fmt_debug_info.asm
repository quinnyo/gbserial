
include "hw.inc"
include "acharmap.inc"
include "serial.inc"
include "packet.inc"


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
strQueue: StrDB " Q:"
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
fmt_serio_status::
	ld c, $FF ; str terminator
	ld de, strDown :: bit SIOSTB_ENABLE, a :: jp z, memcpy_terminated

	ld b, a ; safe keeping
	call fmt_serio_xfer_status

	ld de, strQueue
	call memcpy_terminated
	ld a, "0"
	bit SIOSTB_QUEUE, b
	jr z, :+
	ld a, "1"
:
	ld [hl+], a
	ld a, " "
	ld a, b
	ret


; @param A: value
; @param HL: &dest
; @mut: AF, BC, DE, HL
fmt_serio_xfer_status::
	ld c, $FF ; str terminator
	and SIOSTF_XFER_STATUS
	ld de, strIdle :: cp SIOSTF_XFER_IDLE :: jp z, memcpy_terminated
	ld de, strActive :: cp SIOSTF_XFER_ACTIVE :: jp z, memcpy_terminated
	ld de, strDone :: cp SIOSTF_XFER_DONE :: jp z, memcpy_terminated
	ld de, strTimeout :: cp SIOSTF_XFER_TIMEOUT :: jp z, memcpy_terminated
	jp print_bracketed_h8


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
	ld c, $FF
	ld de, strHshkConnected :: cp HSHK_CONNECTED :: jp z, memcpy_terminated
	ld de, strHshkWorking   :: cp HSHK_WORKING   :: jp z, memcpy_terminated
	ld de, strHshkInit      :: cp HSHK_INIT      :: jp z, memcpy_terminated
	ld de, strHshkAborted   :: cp HSHK_ABORTED   :: jp z, memcpy_terminated
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
	ld c, $FF
	ld de, strPkstNull    :: cp _PKST_NULL    :: jp z, memcpy_terminated
	ld de, strPkstPrep    :: cp _PKST_PREP    :: jp z, memcpy_terminated
	ld de, strPkstStopped :: cp _PKST_STOPPED :: jp z, memcpy_terminated
	ld de, strPkstReady   :: cp _PKST_READY   :: jp z, memcpy_terminated
	ld de, strPkstXfer    :: cp _PKST_XFER    :: jp z, memcpy_terminated
	ld de, strPkstCheck   :: cp _PKST_CHECK   :: jp z, memcpy_terminated
	ld de, strPkstOk      :: cp _PKST_OK      :: jp z, memcpy_terminated
	ld de, strPkstError   :: cp _PKST_ERROR   :: jp z, memcpy_terminated
	ret


; @param A: value
; @param HL: &dest
; @mut: AF, B, HL
fmt_packet_check::
	ld b, "^yes^" :: cp PKT_CHECK_OK   :: jr z, .write
	ld b, "^no^"  :: cp PKT_CHECK_FAIL :: jr z, .write
	ld b, "?"
.write
	ld a, b
	ld [hl+], a
	ret


print_bracketed_h8:
	ld a, "\{"
	ld [hl+], a
	call utile_print_h8
	ld a, "}"
	ld [hl+], a
	ret
