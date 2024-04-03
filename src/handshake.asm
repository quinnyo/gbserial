
include "hw.inc"
include "serial.inc"

/*
CONNECTION HANDSHAKE CODES
- The `A` and `B` codes are sent by the host and guest, respectively.
*/

def HSHK_NULL       equ $FF

def HSHK_ANNOUNCE_A equ %01000010 ; $42
def HSHK_CONFIRM_A  equ %10000100 ; $84
def HSHK_CODE2_A    equ %00001001 ; $09
def HSHK_CODE3_A    equ %00010010 ; $12
def HSHK_CODE4_A    equ %00100100 ; $24
def HSHK_CODE5_A    equ %01001000 ; $48
def HSHK_CODE6_A    equ %10010000 ; $90
def HSHK_CODE7_A    equ %00100001 ; $21

def HSHK_ANNOUNCE_B equ %10111101 ; $BD
def HSHK_CONFIRM_B  equ %01111011 ; $7B
def HSHK_CODE2_B    equ %11110110 ; $F6
def HSHK_CODE3_B    equ %11101101 ; $ED
def HSHK_CODE4_B    equ %11011011 ; $DB
def HSHK_CODE5_B    equ %10110111 ; $B7
def HSHK_CODE6_B    equ %01101111 ; $6F
def HSHK_CODE7_B    equ %11011110 ; $DE

def HSHK_COUNT_REQUIRED  equ 3
def HSHK_HOST_DELAY      equ 1
def HSHK_RETRY_DELAY     equ 30
def HSHK_RETRIES_DEFAULT equ 3

def HSHK_CONNECTED      equ $00
def HSHK_WORKING        equ $01
def HSHK_INIT           equ $80
def HSHK_ABORTED        equ $FF
export HSHK_CONNECTED, HSHK_WORKING, HSHK_INIT, HSHK_ABORTED


section "wHandshake", wram0
wHshkState:: db
wHshkCount:: db
wHshkLastTx: db
wHshkLastRx: db
wHshkExpect: db
wHshkRetries:: db


section "handshake", rom0
handshake_init::
	call serio_init

	ld a, HSHK_INIT
	ld [wHshkState], a
	ld a, HSHK_COUNT_REQUIRED
	ld [wHshkCount], a
	ld a, HSHK_NULL
	ld [wHshkLastTx], a
	ld [wHshkLastRx], a
	ld a, HSHK_RETRIES_DEFAULT
	ld [wHshkRetries], a
	ld a, low(handshake_rx) :: ld [wSerioRxHandler + 0], a
	ld a, high(handshake_rx) :: ld [wSerioRxHandler + 1], a
	ret


; enable serio, start handshake sequence
; @mut: AF, HL
handshake_start::
	call serio_enable

	ld a, HSHK_INIT
	ld [wHshkState], a
	ld a, HSHK_COUNT_REQUIRED
	ld [wHshkCount], a
	ld a, HSHK_NULL
	ld [wHshkLastTx], a
	ld [wHshkLastRx], a
	ld a, HSHK_RETRIES_DEFAULT
	ld [wHshkRetries], a
	ld a, low(handshake_rx) :: ld [wSerioRxHandler + 0], a
	ld a, high(handshake_rx) :: ld [wSerioRxHandler + 1], a

	jr _tx_first


handshake_host::
	ld a, [wSerConfig]
	or SERIO_CFGF_HOST
	ld [wSerConfig], a
	jr handshake_start


handshake_join::
	ld a, [wSerConfig]
	res SERIO_CFGB_ROLE, a
	ld [wSerConfig], a
	jr handshake_start


; stop, disconnect, disable serio
; @mut: AF, HL
handshake_abort::
	ld a, HSHK_ABORTED
	ld [wHshkState], a
	jp serio_disable


handshake_tick::
	call serio_tick
	ret


; Serio RX handler for handshake
handshake_rx::
	; call serio_continue
	GetTransferStatus
	cp SIOSTF_XFER_DONE
	jr nz, _tx_retry ; if transfer ended unsuccessfully

	; check if we rx'd correct response
	ld a, [wHshkExpect]
	ld b, a
	ldh a, [hSerRx]
	cp b
	jr nz, _tx_retry
	ld [wHshkLastRx], a

	; rx'd appropriate code, update counter
	ld a, [wHshkCount]
	and a
	jr nz, _tx_next
	; count already zero?
	ld a, HSHK_CONNECTED
	ld [wHshkState], a
	ret


; @param A: current hshk count
_tx_next:
	dec a
	ld [wHshkCount], a
	ld a, [wHshkLastTx]
	rlca

	; FALLTHROUGH

; transmit hshk code in A and set expected rx code
; @param A: tx code
; @mut: AF, BC
_tx:
	ld b, a
	ld [wHshkLastTx], a
	cpl ; A <--/--> B
	ld [wHshkExpect], a
	call serio_continue
	ld a, b
	jp serio_transmit


_tx_retry:
	; do retry limit
	ld a, [wHshkRetries]
	and a
	jr z, handshake_abort
	dec a
	ld [wHshkRetries], a
	; set delay
	ld a, HSHK_RETRY_DELAY
	ldh [hSerDelay], a

	; FALLTHROUGH

; transmit announce code as A if host, or B if not
_tx_first:
	ld a, HSHK_COUNT_REQUIRED
	ld [wHshkCount], a ; no progress
	ld a, [wSerConfig]
	and SERIO_CFGF_HOST
	ld a, HSHK_ANNOUNCE_A
	jr nz, _tx
	cpl ; A --> B
	jr _tx


/*

START
next = first = 001
expect = ~next
A: 001 --> B
A <-- B: 110
(got inverted message == correct)
	next = last << 1
	expect = ~next
	A: 010 --> B
	A <-- B: 101
	(got expect)
(got something else)

*/