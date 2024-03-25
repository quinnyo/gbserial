
include "hw.inc"
include "serial.inc"


section "irq/serial", rom0[$58]

irq_serial:
	push af
	push bc
	call _serio_complete_transfer
	pop bc
	pop af
	reti


section "hSerial", hram
; The next value to transmit
_serio_tx:: db
; Holds the most recently received byte.
_serio_rx:: db
; Serial link status/flags
_serio_status:: db
; Externally clocked transfer timeout duration
hSerialTransferTimeout:: db
; timeout ticks remaining
hTimeoutTicks:: db


section "serio", rom0

serio_init::
	ld hl, rIF
	res IEB_SERIAL, [hl]
	ld l, h ; $FF0F --> $FFFF
	set IEB_SERIAL, [hl]

	ld a, $AA
	ldh [_serio_tx], a
	ld a, $99
	ldh [_serio_rx], a
	ld a, SERIO_IDLE
	ldh [_serio_status], a
	xor a
	ldh [hSerialTransferTimeout], a
	ldh [hTimeoutTicks], a
	ret


serio_tick::
	ldh a, [_serio_status]
	cp SERIO_WORKING
	ret nz
	ldh a, [hTimeoutTicks]
	and a
	ret z ; timeout not enabled
	dec a
	ldh [hTimeoutTicks], a
	ret nz
	; timed out
	ld a, SERIO_TIMEOUT
	jr serio_stop


; Start transfer as host (internal clock).
serio_host_start::
	ld a, SERIO_WORKING
	ldh [_serio_status], a
	; no timeout for clock source
	xor a
	ldh [hTimeoutTicks], a
	; Set internal clock source, without starting the transfer.
	ld a, $01
	ldh [rSC], a
	ldh a, [_serio_tx]
	ldh [rSB], a
	; Now start the transfer
	ld a, $81
	ldh [rSC], a
	ret


; Start transfer as guest (external clock).
serio_guest_start::
	ld a, SERIO_WORKING
	ldh [_serio_status], a
	; Use timeout when using external clock source
	ldh a, [hSerialTransferTimeout]
	ldh [hTimeoutTicks], a
	xor a
	ldh [rSC], a
	ldh a, [_serio_tx]
	ldh [rSB], a
	ld a, $80
	ldh [rSC], a
	ret


; @param A: new status code
serio_stop::
	ldh [_serio_status], a
	xor a
	ldh [rSC], a
	ldh [hTimeoutTicks], a
	ret


; @mut: AF, B
_serio_complete_transfer:
	; check that we were expecting a transfer
	ldh a, [_serio_status]
	cp SERIO_WORKING
	ret nz ; @TODO: should this be an error condition?

	; store the received value
	ldh a, [rSB]
	ldh [_serio_rx], a

	ld a, SERIO_DONE
	jr serio_stop
