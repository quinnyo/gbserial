
include "hw.inc"
include "serial.inc"


def SERIOCTL_READY equ $81
def SERIOCTL_RX_ACK equ $88


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


section "serio", rom0

serio_init::
	ld hl, rIF
	res IEB_SERIAL, [hl]
	ld l, h ; $FF0F --> $FFFF
	set IEB_SERIAL, [hl]

	ld a, SERIOCTL_READY
	ld [_serio_tx], a
	ld a, $FF
	ld [_serio_rx], a
	ld a, SERIOF_IDLE
	ld [_serio_status], a
	ret


; Start a transfer (as the clock source) to send the value in `[_serio_tx]`.
serio_tx::
	ld a, SERIOF_TX | SERIOF_WORKING | SERIOF_SYNC
	ldh [_serio_status], a
	; Set internal clock source, without starting the transfer.
	ld a, $01
	ldh [rSC], a
	ldh a, [_serio_tx]
	ldh [rSB], a
	; Now start the transfer
	ld a, $81
	ldh [rSC], a
	ret


; Enter receive mode -- await transfer from other device.
serio_rx::
	ld a, SERIOF_WORKING | SERIOF_SYNC
	ldh [_serio_status], a
	xor a
	ldh [rSC], a
	; respond with ACK code so remote knows we were actively receiving...
	ld a, SERIOCTL_RX_ACK
	ldh [rSB], a
	ld a, $80
	ldh [rSC], a
	ret


; @mut: AF, B
_serio_complete_transfer:
	; check that we were expecting a transfer
	ldh a, [_serio_status]
	bit SERIOB_WORKING, a
	ret z ; @TODO: should this be an error condition?

	ld b, a ; keep status in B and write it once at the end
	res SERIOB_WORKING, b

	; store the received value
	ldh a, [rSB]
	ldh [_serio_rx], a

	; were we the sender?
	bit SERIOB_TXRX, b
	jr z, .ok ; receiver, nothing else to do
	ldh a, [_serio_rx]
	cp SERIOCTL_RX_ACK
	jr z, .ok
	; incorrect response, set ERROR flag
	set SERIOB_ERROR, b
.ok
	ld a, b
	ldh [_serio_status], a
	ret
