INCLUDE "hardware.inc"


DEF HANDSHAKE_INIT EQU $01
DEF HANDSHAKE_PASS EQU $00

; Code sent by clock provider
DEF HANDSHAKE_PING EQU $88
; Code sent by externally clocked device
DEF HANDSHAKE_PONG EQU $77


SECTION "Handshake State", WRAM0
wHandshakeState:: db
wHandshakeExpect: db


SECTION "Handshake Impl", ROM0
; Begin handshake as the default externally clocked device.
HandshakeInit::
	call SioAbort
	ld a, 0
	ldh [rSC], a
	ld a, HANDSHAKE_INIT
	ld [wHandshakeState], a
	ld a, HANDSHAKE_PING
	ld [wHandshakeExpect], a
	ld a, HANDSHAKE_PONG
	ld [wSioBufferTx], a
	ld a, 1
	jp SioTransferStart


; Begin handshake as the clock provider -- internally clocked device.
HandshakeAsClockProvider:
	call SioAbort
	ld a, SCF_SOURCE
	ldh [rSC], a
	ld a, HANDSHAKE_INIT
	ld [wHandshakeState], a
	ld a, HANDSHAKE_PONG
	ld [wHandshakeExpect], a
	ld a, HANDSHAKE_PING
	ld [wSioBufferTx], a
	ld a, 1
	jp SioTransferStart


HandshakeUpdate::
	ld a, [wHandshakeState]
	cp a, HANDSHAKE_PASS
	ret z ; nothing to do.
	; press START: reset handshake as clock provider
	ldh a, [hKeysPressed]
	bit PADB_START, a
	jr nz, HandshakeAsClockProvider
	; Check if transfer has completed.
	ld a, [wSioState]
	cp a, SIO_DONE
	jr z, .TransferDone
	; wait for active transfer to end
	cp a, SIO_ACTIVE
	ret z
	jr HandshakeInit

.TransferDone:
	; flush sio status
	ld a, SIO_IDLE
	ld [wSioState], a
	; Check received value
	ld a, [wHandshakeExpect]
	ld b, a
	ld a, [wSioBufferRx]
	cp a, b
	ret nz
	ld a, HANDSHAKE_PASS
	ld [wHandshakeState], a
	ld a, 0
	ld [wHandshakeExpect], a
	ret
