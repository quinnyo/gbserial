
include "hw.inc"
include "serial.inc"


section "irq/serial", rom0[$58]

irq_serial:
	push af
	push bc
	call _serio_serial_irq_handler
	pop bc
	pop af
	reti


section "hSerial", hram
; The next value to transmit
hSerTx:: db
; Holds the most recently received byte.
hSerRx:: db
; Serial link status/flags
hSerStatus:: db
; timeout ticks remaining
hTimeoutTicks:: db

; ticks to wait before starting the next transfer
hSerDelay::
hDelay: db


section "wSerial", wram0
wSerConfig:: db
; Externally clocked transfer timeout duration
wSerTransferTimeout:: db
; Transfer end event callback address. Set to $FFFF to disable.
wSerioOnXferEnd:: dw
; Post-transfer delay duration. Host only.
wSerHostXferEndDelay:: db


section "Serio", rom0

; @mut: AF, HL
serio_init::
	ld a, $FF :: ld [wSerioOnXferEnd + 0], a :: ld [wSerioOnXferEnd + 1], a
	ld a, SERIO_TIMEOUT_TICKS_DEFAULT :: ld [wSerTransferTimeout], a
	ld a, SERIO_CFGF_DEFAULT :: ld [wSerConfig], a
	ld a, $AA :: ldh [hSerTx], a
	ld a, $99 :: ldh [hSerRx], a
	xor a :: ldh [hTimeoutTicks], a :: ldh [hDelay], a
	ld a, SIOSTF_DEFAULT :: ldh [hSerStatus], a
	ld a, SERIO_HOST_XFER_END_DELAY_DEFAULT :: ld [wSerHostXferEndDelay], a
	ret


; @mut: AF, HL
serio_enable::
	ldh a, [hSerStatus]
	bit SIOSTB_ENABLE, a
	ret nz
	ld a, SIOSTF_ENABLED :: ldh [hSerStatus], a
	xor a :: ldh [rSC], a :: ldh [rSB], a
	jp serio_interrupt_enable


; @mut: AF, HL
serio_disable::
	xor a :: ldh [rSC], a :: ldh [rSB], a
	call serio_interrupt_disable
	; ld hl, wSerConfig :: res SERIO_CFGB_ENABLE, [hl]
	ld a, SIOSTF_DEFAULT :: ldh [hSerStatus], a
	xor a :: ldh [hTimeoutTicks], a :: ldh [hDelay], a
	ret


; Reset to IDLE, allowing serio to perform the next transfer.
; @mut: AF
serio_continue::
	SetTransferStatus SIOSTF_XFER_IDLE
	ret


; @param A: value
; @mut: AF
serio_transmit::
	ldh [hSerTx], a
	ldh a, [hSerStatus]
	set SIOSTB_QUEUE, a
	ldh [hSerStatus], a
	ret


serio_tick::
	ldh a, [hSerStatus]
	bit SIOSTB_ENABLE, a
	ret z

	; if transfer ended, do callback
	bit SIOSTB_XFER_ENDED, a
	jp nz, _on_xfer_end

	IsTransferActive
	jr nz, _do_xfer_queue ; transfer not active, update queue

	; transfer is active, do timeout on guest
	IsExternalClock
	ret nz ; active transfer on the host, nothing to do
	call _do_timeout
	ret


; If the delay time is non-zero, tick the delay timer.
; If the delay time is zero, process the queue.
_do_xfer_queue:
	ldh a, [hDelay]
	and a
	jr nz, .delay_tick ; delay > 0

	; update SC from config
	ld a, [wSerConfig]
assert SCF_SOURCE == SERIO_CFGF_HOST
	and SCF_SOURCE
	ldh [rSC], a
	ld b, a
	; restart timeout duration for GUEST
	bit SCB_SOURCE, b
	jr nz, :+
	ld a, [wSerTransferTimeout]
	ldh [hTimeoutTicks], a
:

	; check QUEUE flag
	ldh a, [hSerStatus]
	bit SIOSTB_QUEUE, a
	ret z ; no QUEUE
	ld a, SIOSTF_XFER_ACTIVE | SIOSTF_ENABLED
	ldh [hSerStatus], a
	; start transfer
	ldh a, [hSerTx]
	ldh [rSB], a
	ld a, $80
	or b
	ldh [rSC], a
	ret

.delay_tick:
	dec a
	ldh [hDelay], a
	ret


; Updates (ticks) the transfer time-out timer. Assumes that there is an active transfer.
; If the timer expires, the transfer is ended with the TIMEOUT status/flag.
_do_timeout:
	ldh a, [hTimeoutTicks]
	and a
	ret z ; timer == 0, wait forever
	dec a
	ldh [hTimeoutTicks], a
	ret nz
	; timed out -- end transfer with TIMEOUT status
	ldh a, [hSerStatus]
	and $FF ^ SIOSTF_XFER_STATUS
	or SIOSTF_XFER_TIMEOUT
	ldh [hSerStatus], a
	ret


_on_xfer_end:
	; Delay HOST after every transfer to mitigate phase/sync errors.
	IsExternalClock
	jr z, :+
	ld a, [wSerHostXferEndDelay]
	ldh [hDelay], a
:
	; invoke transfer end callback handler
	ld hl, wSerioOnXferEnd
	ld a, [hl+]
	ld h, [hl]
	ld l, a
	and h
	cp $FF ; handler == $FFFF --> disabled
	ret z
	jp hl


; @mut: AF
_serio_serial_irq_handler:
	; check that we were expecting a transfer
	IsTransferActive
	ret nz

	; store the received value
	ldh a, [rSB]
	ldh [hSerRx], a

	SetTransferStatus SIOSTF_XFER_DONE
	ret


section "Serio Utility Funcs", rom0

; @mut: HL
serio_interrupt_enable:
; 3 + 4 + 1 + 4 = 12
	ld hl, rIF
	res IEB_SERIAL, [hl]
	ld l, h ; $FF0F --> $FFFF
	set IEB_SERIAL, [hl]
	ret


; @mut: HL
serio_interrupt_disable:
; 3 + 4 + 3 + 4 = 14
	ld hl, rIE
	res IEB_SERIAL, [hl]
	ld hl, rIF
	res IEB_SERIAL, [hl]
	ret

;
; vim:ft=rgbasm
