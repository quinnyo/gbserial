; ::::::::::::::::::::::::::::::::::::::
; ::                                  ::
; ::                       ______.    ::
; ::       _              |````` ||   ::
; ::     _/ \__@_         |[- - ]||   ::
; ::    /        `--<[|]= |[ m  ]||   ::
; ::    \      .______    | ```` ||   ::
; ::    /     !| `````|   | +  oo||   ::
; ::   (      ||[ ^u^]|   |  .. #||   ::
; ::    `-<[|]=|[    ]|   `______//   ::
; ::          || ```` |               ::
; ::          || +  oo|               ::
; ::          ||  .. #|               ::
; ::          !|______/               ::
; ::                                  ::
; ::                                  ::
; ::::::::::::::::::::::::::::::::::::::

INCLUDE "hardware.inc"

; Duration of timeout period in ticks. (for externally clocked device)
DEF SIO_TIMEOUT_TICKS EQU 60

; Catchup delay duration
DEF SIO_CATCHUP_SLEEP_DURATION EQU 100

DEF SIO_CONFIG_INTCLK   EQU SCF_SOURCE
DEF SIO_CONFIG_RESERVED EQU $02
DEF SIO_CONFIG_DEFAULT  EQU $00
EXPORT SIO_CONFIG_INTCLK

; SioStatus transfer state enum
RSRESET
DEF SIO_IDLE           RB 1
DEF SIO_FAILED         RB 1
DEF SIO_DONE           RB 1
DEF SIO_BUSY           RB 0
DEF SIO_XFER_STARTED   RB 1
EXPORT SIO_IDLE, SIO_FAILED, SIO_DONE
EXPORT SIO_BUSY, SIO_XFER_STARTED

DEF SIO_BUFFER_SIZE EQU 32


SECTION "SioBufferRx", WRAM0, ALIGN[8]
wSioBufferRx:: ds SIO_BUFFER_SIZE


SECTION "SioBufferTx", WRAM0, ALIGN[8]
wSioBufferTx:: ds SIO_BUFFER_SIZE


SECTION "SioCore State", WRAM0
; Sio config flags
wSioConfig:: db
; Sio state machine current state
wSioState:: db
; Number of transfers to perform (bytes to transfer)
wSioCount:: db
wSioBufferOffset:: db
; Timer state (as ticks remaining, expires at zero) for timeouts and delays.
; HACK: this is only "public" (::) for access by debug display code.
wSioTimer:: db


SECTION "Sio Serial Interrupt", ROM0[$58]
SerialInterrupt:
	jp SioInterruptHandler


SECTION "SioCore Impl", ROM0
; Initialise/reset Sio to the ready to use 'IDLE' state.
; NOTE: Enables the serial interrupt.
; @mut: AF, [IE]
SioInit::
	ld a, SIO_CONFIG_DEFAULT
	ld [wSioConfig], a
	ld a, SIO_IDLE
	ld [wSioState], a
	ld a, 0
	ld [wSioTimer], a
	ld a, 0
	ld [wSioCount], a
	ld [wSioBufferOffset], a

	; enable serial interrupt
	ldh a, [rIE]
	or a, IEF_SERIAL
	ldh [rIE], a
	ret


; @mut: AF, HL
SioTick::
	ld a, [wSioState]
	cp a, SIO_XFER_STARTED
	jr z, .xfer_started
	; anything else: do nothing
	ret
.xfer_started:
	ld a, [wSioCount]
	and a, a
	jr nz, :+
	ld a, SIO_DONE
	ld [wSioState], a
	ret
:
	; update timeout on external clock
	ldh a, [rSC]
	and a, SCF_SOURCE
	ret nz
	ld a, [wSioTimer]
	and a, a
	ret z ; timer == 0, timeout disabled
	dec a
	ld [wSioTimer], a
	jr z, SioAbort
	ret


; Abort the ongoing transfer (if any) and enter the FAILED state.
; @mut: AF
SioAbort::
	ld a, SIO_FAILED
	ld [wSioState], a
	ldh a, [rSC]
	res SCB_START, a
	ldh [rSC], a
	ret


SioInterruptHandler:
	push af
	push hl

	; check that we were expecting a transfer (to end)
	ld hl, wSioCount
	ld a, [hl]
	and a
	jr z, .end
	dec [hl]
	; Get buffer pointer offset (low byte)
	ld a, [wSioBufferOffset]
	ld l, a
	; Get received value
	ld h, HIGH(wSioBufferRx)
	ldh a, [rSB]
	; NOTE: incrementing L here
	ld [hl+], a
	; Store updated buffer offset
	ld a, l
	ld [wSioBufferOffset], a
	; If completing the last transfer, don't start another one
	; NOTE: We are checking the zero flag as set by `dec [hl]` up above!
	jr z, .end
	; Next value to send
	ld h, HIGH(wSioBufferTx)
	ld a, [hl]
	ldh [rSB], a
	; If internal clock source, do catchup delay
	ldh a, [rSC]
	and a, SCF_SOURCE
	; NOTE: preserve `A` to be used after the loop
	jr z, .start_xfer
	ld l, SIO_CATCHUP_SLEEP_DURATION
.catchup_sleep_loop:
	nop
	nop
	dec l
	jr nz, .catchup_sleep_loop
.start_xfer:
	or a, SCF_START
	ldh [rSC], a

.end:
	ld a, SIO_TIMEOUT_TICKS
	ld [wSioTimer], a
	pop hl
	pop af
	reti


; Start a multibyte serial transfer. Copies the payload.
; @param C: length of payload data
; @param DE: &payload
; @mut: AF, C, DE, HL
SioTransferStart::
	; Check payload data will fit in our buffer
	ld a, SIO_BUFFER_SIZE
	cp c
	; TODO: panic!?
	ret c

	ld a, c
	ld [wSioCount], a
	ld a, 0
	ld [wSioBufferOffset], a

	ld hl, wSioBufferTx
:
	ld a, [de] :: inc de
	ld [hl+], a
	dec c
	jr nz, :-

	; set the clock source (do this first & separately from starting the transfer!)
	ld a, [wSioConfig]
	and a, SCF_SOURCE ; the sio config byte uses the same bit for the clock source
	ldh [rSC], a
	; reset timeout
	ld a, SIO_TIMEOUT_TICKS
	ld [wSioTimer], a
	; send first byte
	ld a, [wSioBufferTx]
	ldh [rSB], a
	; start the transfer
	ldh a, [rSC]
	or a, SCF_START
	ldh [rSC], a
	ld a, SIO_XFER_STARTED
	ld [wSioState], a
	ret
