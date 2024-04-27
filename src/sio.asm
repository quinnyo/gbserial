INCLUDE "hw.inc"

; Duration of timeout period in ticks. (for externally clocked device)
DEF SIO_TIMEOUT_TICKS EQU 240
; Duration of 'catchup' delay period in ticks. (for internally clocked device)
DEF SIO_CATCHUP_TICKS EQU 1

DEF SIO_CONFIG_INTCLK  EQU SCF_SOURCE
DEF SIO_CONFIG_DEFAULT EQU 0
EXPORT SIO_CONFIG_INTCLK

; SioStatus transfer state enum
RSRESET
DEF SIO_IDLE           RB 1
DEF SIO_XFER_START     RB 1
DEF SIO_XFER_STARTED   RB 1
DEF SIO_XFER_COMPLETED RB 1
DEF SIO_XFER_FAILED    RB 1
EXPORT SIO_IDLE, SIO_XFER_START, SIO_XFER_STARTED, SIO_XFER_COMPLETED, SIO_XFER_FAILED


SECTION "SioCore State", WRAM0
; Sio config flags
wSioConfig:: db
; Sio state machine current state
wSioState:: db
; Source address of next value to transmit
wSioTxPtr:: dw
; Destination address of next received value
wSioRxPtr:: dw
; Number of transfers to perform (bytes to transfer)
wSioCount:: db

; Timer state (as ticks remaining, expires at zero) for timeouts and delays.
wSioTimer::
wTimer: db


SECTION "SioCore Impl", ROM0

SioInit::
	ld a, SIO_CONFIG_DEFAULT
	ld [wSioConfig], a
	ld a, SIO_IDLE
	ld [wSioState], a
	xor a
	ld [wTimer], a
	ld a, $FF ; using FFFF as 'null'/unset
	ld [wSioTxPtr + 0], a
	ld [wSioTxPtr + 1], a
	ld [wSioRxPtr + 0], a
	ld [wSioRxPtr + 1], a
	xor a
	ld [wSioCount], a

	; enable serial interrupt
	ldh a, [rIE]
	or IEF_SERIAL
	ldh [rIE], a
	ret


SioTick::
	ld a, [wSioState]
	cp SIO_XFER_START
	jr z, SioProcessQueue
	cp SIO_XFER_STARTED
	jr z, .xfer_started
	cp SIO_XFER_COMPLETED
	jr z, SioProcessQueue
	; treat anything else as failed/error and do nothing
	ret
.xfer_started:
	; update timeout on external clock
	ldh a, [rSC]
	and SCF_SOURCE
	ret nz
	ld a, [wTimer]
	and a
	ret z ; timer == 0, timeout disabled
	dec a
	ld [wTimer], a
	jr z, SioAbortTransfer
	ret


SioProcessQueue:
	; if SioCount > 0: start next transfer
	ld a, [wSioCount]
	and a
	ret z

	; FALLTHROUGH

SioStartNextTransfer:
	; delay on internal clock device
	ldh a, [rSC]
	bit SCB_SOURCE, a
	jr z, .no_delay
		ld a, [wTimer]
		and a
		jr z, .no_delay
			dec a
			ld [wTimer], a
			ret
.no_delay

	; get next value and start transfer
	ld hl, wSioTxPtr
	ld a, [hl+]
	ld h, [hl]
	ld l, a
	ld b, [hl]
	call SioStartTransfer

	ret


; Start a serial port transfer immediately.
; @param B: data byte to send
; @mut: AF
SioStartTransfer::
	; set the clock source (do this first & separately from starting the transfer!)
	ld a, [wSioConfig]
	and SCF_SOURCE ; the sio config byte uses the same bit for the clock source
	ldh [rSC], a
	; load the value to send
	ld a, b
	ldh [rSB], a
	; start the transfer
	ldh a, [rSC]
	or SCF_START
	ldh [rSC], a

	; reset timeout (on externally clocked device)
	bit SCB_SOURCE, a
	jr nz, :+
	ld a, SIO_TIMEOUT_TICKS
	ld [wTimer], a
:
	ld a, SIO_XFER_STARTED
	ld [wSioState], a
	ret


;
SioAbortTransfer::
	ld a, SIO_XFER_FAILED
	ld [wSioState], a
	ldh a, [rSC]
	res SCB_START, a
	ldh [rSC], a
	ret


SioSerialInterruptHandler:
	push af
	push hl

	; check that we were expecting a transfer
	ld a, [wSioState]
	cp SIO_XFER_STARTED
	ret nz
	; TODO: handle transfer ending during other states as error/s?

	; store the received value
	ld hl, wSioRxPtr
	ld a, [hl+]
	ld h, [hl]
	ld l, a
	ldh a, [rSB]
	ld [hl+], a
	; store the updated Rx pointer
	ld a, l
	ld [wSioRxPtr + 0], a
	ld a, h
	ld [wSioRxPtr + 1], a

	; update the Tx pointer
	ld hl, wSioTxPtr
	inc [hl] ; inc low byte
	jr nz, :+
	; inc high byte if overflow
	inc hl
	inc [hl]
:

	; update transfer count
	ld hl, wSioCount
	dec [hl]

	; set transfer state to 'completed'
	ld a, SIO_XFER_COMPLETED
	ld [wSioState], a

	ldh a, [rSC]
	and SCF_SOURCE
	jr z, :+
	; reset delay timer on internal clock
	ld a, SIO_CATCHUP_TICKS
	ld [wTimer], a
:

	pop hl
	pop af
	reti


section "Serial Interrupt", rom0[$58]
SerialInterrupt:
	jp SioSerialInterruptHandler

