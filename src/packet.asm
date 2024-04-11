
include "hw.inc"
include "serial.inc"
include "packet.inc"


/*
PACKET OF NOTES
- connection (and who is HOST) is already established before starting packet stuff
	- hands have been shook
	- HOST is in charge
	- GUEST is supposed to do what it's told
- oh, 'connection'? 'packet'? basically TCP then?!
	- No.
	- (Ha, ha, ha. I am a dragonfly. )

PACKET NOTES
- wow, packets are multi-byte!
- packet is always the same size (for now at least)
- packets come with a checksum, enabling some ability to detect errors oh my
- each peer sends the entire packet buffer every time
	- this will add some overhead, but
	- it also makes the code easier to write
	- and possibly other benefits for e.g. error detection
- packet transfer is a "simultaneous" trade, like GB serial:
	- i.e. both peers send their `PACKET[i]` byte in the same GB serial transfer

PACKET PEER
- each device and the thing running on it is confusingly referred to as a peer
- peers are state machine type deals
- transfer starts when both peers have sent "READY" to each other
- high level state flow:
	- PREP: prepare outgoing packet
	- READY: ready to send
	- XFER: actual packet transfer
	- CHECK: perform error detection and share results
	- RESULT: transfer complete, handle errors, etc.
*/


; NextOffsetAddr R16, &OFFSET, &START
; load address at START + OFFSET to R16, increment OFFSET
; @mut: AF
macro NextOffsetAddr
	assert _NARG == 3
	if strcmp(strlwr("\1"), "hl") == 0
		ld hl, (\2)
		ld a, [hl]
		inc [hl]
	else
		ld a, [\2]
		inc a
		ld [\2], a
		dec a
	endc
	ld \1, (\3)
	add low(\1)
	ld low(\1), a
	adc high(\1)
	sub low(\1)
	ld high(\1), a
endm


section "A/PACKET/STATE", wram0
wPacketStateLoc::
_pkst_loc: db ; Local %PACKETPEER% state
wPacketStateRem::
_pkst_rem: db ; Current (last received) state of remote.
wPacketXferRemaining::
_xfer_remaining: db ; number of byte transfers remaining in packet transfer
_tx_index: db ; offset to next byte of the tx packet buffer
_rx_index: db ; offset to next byte of the rx packet buffer

_retries: db

wPacketCheckLoc::
_check_loc: db
wPacketCheckRem::
_check_rem: db

_packet_tx::
	.check:: ds PKT_CHECK_SZ
	.data:: ds PKT_DATA_SZ

_packet_rx::
	.check:: ds PKT_CHECK_SZ
	.data:: ds PKT_DATA_SZ


section "A/PACKET/IMPL", rom0
packet_init::
	ld a, _PKST_PREP
	ld [_pkst_loc], a
	ld [_pkst_rem], a
	xor a
	ld [_xfer_remaining], a
	ld [_tx_index], a
	ld [_rx_index], a
	ld [_retries], a
	ld [_check_loc], a
	ld [_check_rem], a
	ld hl, _packet_tx
	ld c, PKT_SZ
:
	ld [hl+], a
	dec c
	jr nz, :-

	ld hl, _packet_rx
	ld c, PKT_SZ
:
	ld [_packet_rx], a
	dec c
	jr nz, :-

	ld a, low(_pkt_on_xfer_end) :: ld [wSerioOnXferEnd + 0], a
	ld a, high(_pkt_on_xfer_end) :: ld [wSerioOnXferEnd + 1], a
	ret


packet_stop::
	ld a, $FF
	ld [wSerioOnXferEnd + 0], a
	ld [wSerioOnXferEnd + 1], a
	ld a, _PKST_STOPPED
	ld [_pkst_loc], a
	ret


; Calculate checksum
;;;;;;;;;;; and if not STOPPED, move to READY state.
; Call this when finished writing TX packet data.
; @mut: AF, C, HL
packet_tx_finalise::
	; TODO: check that stateus is PREP ?

assert PKT_CHECK_SZ == 1, "assuming checksum size here"
	xor a
	ld [_packet_tx.check], a
	ld hl, _packet_tx
	ld c, PKT_SZ
	call Checksum8
	ld [_packet_tx.check], a

	ld a, [_pkst_loc]
	cp _PKST_STOPPED
	ret z

	; FALLTHROUGH

; Start packet transmission
; @mut: AF, C, HL
packet_tx_begin::
	ld a, PACKET_RETRY_COUNT_DEFAULT
	ld [_retries], a
_ready:
	ld a, _PKST_READY
	ld [_pkst_loc], a
	call serio_transmit
	jp serio_continue


; serio xfer callback
_pkt_on_xfer_end:
	ld a, [_pkst_loc]
	cp _PKST_NULL :: ret z
	cp _PKST_PREP :: ret z
	cp _PKST_STOPPED :: ret z

	; [A]: if transfer active, wait
	; [B]: if transfer active, update timeout & wait

	; if transfer active, nothing to do
	ldh a, [hSerStatus]
	bit SIOSTB_XFER_ACTIVE, a
	ret nz

	; timeout error
	bit SIOSTB_XFER_TIMEOUT, a
	jr z, :+
	; TODO: set some error flag?
	jp packet_stop
:

	; transfer ended, do next
	call _do_pkst_next

	; send status if not sending anything else
	ldh a, [hSerStatus]
	bit SIOSTB_QUEUE, a
	ret nz
	ld a, [_pkst_loc]
	call serio_transmit
	jp serio_continue


_do_pkst_next:
;	handle received data, set next transfer, etc.
;	all based on current state (pkst)
;	If nothing is set to transmit from this function, pkst will be sent
	ld a, [_pkst_loc]
	cp _PKST_PREP :: jr z, .prep
	cp _PKST_READY :: jr z, .ready
	cp _PKST_XFER :: jr z, .xfer_proc
	cp _PKST_CHECK :: jr z, .check
	bit PKSTB_MAJ_RESULT, a :: jr nz, .result

.error_bad_pkst:
	di :: ld b, b
	jr .error_bad_pkst

.prep:
	ldh a, [hSerRx]
	ld [_pkst_rem], a
	; lazy
	; just send stateus
	ret
.ready:
	; NOTE: we shouldn't be in pkst READY without sending READY once.
	ldh a, [hSerRx]
	ld [_pkst_rem], a
	cp _PKST_READY
	ret nz

	; FALLTHROUGH
.xfer_start:
	ld a, _PKST_XFER
	ld [_pkst_loc], a
	ld [_pkst_rem], a
	call _reset_xfer
	; NOTE: we send the first byte here without receiving one, because the
	;       current RX value is not part of the packet.
	jr _tx_next_byte

.xfer_proc:
	; receive [i-1]
	NextOffsetAddr hl, _rx_index, _packet_rx
	ldh a, [hSerRx]
	ld [hl], a

	ld a, [_xfer_remaining]
	dec a
	ld [_xfer_remaining], a
	jr nz, _tx_next_byte

	ld a, _PKST_CHECK
	ld [_pkst_loc], a
	ld [_pkst_rem], a

	ld hl, _packet_rx
	ld a, [_rx_index]
	ld c, a
	call Checksum8

	; send result
	ld c, PKT_CHECK_OK
	; checksum should be zero (because the data includes the precalculated checksum from remote)
	and a
	jr z, :+
	ld c, PKT_CHECK_FAIL
:
	ld a, c
	ld [_check_loc], a
	call serio_transmit
	jp serio_continue
.check:
	ldh a, [hSerRx]
	ld [_check_rem], a
	cp PKT_CHECK_OK
	jr nz, _retry
	ld a, [_check_loc]
	cp PKT_CHECK_OK
	jr nz, _retry
	ld a, _PKST_OK
	ld [_pkst_loc], a
	ret
.result:
	ldh a, [hSerRx]
	ld [_pkst_rem], a
	ret


_retry:
	; do retry limit
	ld a, [_retries]
	and a
	jr nz, :+
	ld a, _PKST_ERROR
	ld [_pkst_loc], a
	ret
:
	dec a
	ld [_retries], a
	jp _ready


_tx_next_byte:
	NextOffsetAddr hl, _tx_index, _packet_tx
	ld a, [hl]
	call serio_transmit
	jp serio_continue


_reset_xfer:
	ld a, PKT_SZ
	ld [_xfer_remaining], a
	xor a
	ld [_tx_index], a
	ld [_rx_index], a
	ld [_check_loc], a
	ld [_check_rem], a
	ld hl, _packet_rx
	ld c, PKT_SZ
:
	ld [_packet_rx], a
	dec c
	jr nz, :-
	ret


;
; vim:ft=rgbasm
