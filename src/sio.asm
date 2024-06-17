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
DEF SIO_TIMEOUT_TICKS EQU 240
; Duration of 'catchup' delay period in ticks. (for internally clocked device)
DEF SIO_CATCHUP_TICKS EQU 1
IF DEF(_SIO_SHORTCUT)
; Catchup delay duration when using the Sleep function.
DEF SIO_CATCHUP_SLEEP_DURATION EQU 10
ENDC

DEF SIO_CONFIG_INTCLK   EQU SCF_SOURCE
DEF SIO_CONFIG_RESERVED EQU $02
DEF SIO_CONFIG_ADVANCE  EQU $04
DEF SIO_CONFIG_DEFAULT  EQU SIO_CONFIG_ADVANCE
EXPORT SIO_CONFIG_INTCLK, SIO_CONFIG_ADVANCE

; SioStatus transfer state enum
RSRESET
DEF SIO_IDLE           RB 1
DEF SIO_XFER_COMPLETED RB 1
DEF SIO_XFER_FAILED    RB 1
DEF SIO_BUSY           RB 0
DEF SIO_XFER_START     RB 1
DEF SIO_XFER_STARTED   RB 1
EXPORT SIO_IDLE, SIO_XFER_START, SIO_XFER_STARTED, SIO_XFER_COMPLETED, SIO_XFER_FAILED, SIO_BUSY


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
; HACK: this is only "public" (::) for access by debug display code.
wSioTimer:: db

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
	ld a, $FF ; using FFFF as 'null'/unset
	ld [wSioTxPtr + 0], a
	ld [wSioTxPtr + 1], a
	ld [wSioRxPtr + 0], a
	ld [wSioRxPtr + 1], a
	ld a, 0
	ld [wSioCount], a

	; enable serial interrupt
	ldh a, [rIE]
	or a, IEF_SERIAL
	ldh [rIE], a
	ret


; @mut: AF, HL
SioTick::
	ld a, [wSioState]
	cp a, SIO_XFER_START
	jr z, .process_queue
	cp a, SIO_XFER_STARTED
	jr z, .xfer_started
	cp a, SIO_XFER_COMPLETED
	jr z, .process_queue
	; treat anything else as failed/error and do nothing
	ret
.xfer_started:
	; update timeout on external clock
	ldh a, [rSC]
	and a, SCF_SOURCE
	ret nz
	ld a, [wSioTimer]
	and a, a
	ret z ; timer == 0, timeout disabled
	dec a
	ld [wSioTimer], a
	jr z, SioAbortTransfer
	ret
.process_queue:
	; if SioCount > 0: start next transfer
	ld a, [wSioCount]
	and a, a
	ret z
IF !DEF(_SIO_SHORTCUT)
	; if this device is the clock source (internal clock), do catchup delay
	ldh a, [rSC]
	bit SCB_SOURCE, a
	jr z, .start_next
		ld a, [wSioTimer]
		and a, a
		jr z, .start_next
			dec a
			ld [wSioTimer], a
			ret
ENDC
.start_next:
	; read the Tx pointer (points to the next value to send)
	ld hl, wSioTxPtr
	ld a, [hl+]
	ld h, [hl]
	ld l, a
	; set the clock source (do this first & separately from starting the transfer!)
	ld a, [wSioConfig]
	and a, SCF_SOURCE ; the sio config byte uses the same bit for the clock source
	ldh [rSC], a
	; load the value to send
	ld a, [hl]
	ldh [rSB], a
	; start the transfer
	ldh a, [rSC]
	or a, SCF_START
	ldh [rSC], a

	; reset timeout (on externally clocked device)
	bit SCB_SOURCE, a
	jr nz, :+
	ld a, SIO_TIMEOUT_TICKS
	ld [wSioTimer], a
:
	ld a, SIO_XFER_STARTED
	ld [wSioState], a
	ret


; Abort the ongoing transfer (if any) and enter the FAILED state.
; @mut: AF
SioAbortTransfer::
	ld a, SIO_XFER_FAILED
	ld [wSioState], a
	ldh a, [rSC]
	res SCB_START, a
	ldh [rSC], a
	ret


; Complete the current transfer and read the received value. This function is
; to be called once per (byte) transfer, after the port deactivates itself.
; @mut: AF, HL
SioCompleteTransfer::
	; check that we were expecting a transfer (to end)
	ld a, [wSioState]
	cp a, SIO_XFER_STARTED
	ret nz

	; store the received value
	ld hl, wSioRxPtr
	ld a, [hl+]
	ld h, [hl]
	ld l, a
	ldh a, [rSB]
	ld [hl+], a
	; Check if data pointer advance enabled
	ld a, [wSioConfig]
	and a, SIO_CONFIG_ADVANCE
	jr z, .no_advance
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
.no_advance:
	; update transfer count
	ld hl, wSioCount
	dec [hl]

	; set transfer state to 'completed'
	ld a, SIO_XFER_COMPLETED
	ld [wSioState], a

	ldh a, [rSC]
	and a, SCF_SOURCE
IF DEF(_SIO_SHORTCUT)
	jr z, :+
	ld a, SIO_CATCHUP_SLEEP_DURATION
	call Sleep
:
	jp SioTick
ELSE
	jr z, :+
	; reset delay timer on internal clock
	ld a, SIO_CATCHUP_TICKS
	ld [wSioTimer], a
:
	ret
ENDC


IF DEF(_SIO_SHORTCUT)
; | duration | T-states | M-states |
; |----------|----------|----------|
; |        0 |       24 |        6 |
; |      x>0 |  x*24+32 |    x*6+5 |
; |       20 |      512 |      125 |
; |      127 |     3080 |      767 |
; |      255 |     6152 |     1535 |
; @param A: duration
Sleep:
	; (and + ret z + nop + nop) + (jr 0 - jr 1) + (ret) + duration * (nop + nop + dec a + jr 1)
	; = 20 + -4 + 16 + duration * (4 + 12 + 4 + 4)
	; = 32 + duration * 24
	and a
	ret z
	nop
	nop
:
	nop
	nop
	dec a
	jr nz, :-
	ret
ENDC


SECTION "Sio Serial Interrupt", ROM0[$58]
; NOTE: This implementation of the serial interrupt is for demonstration purposes.
SerialInterrupt:
	; Use the stack to preserve the registers used by SioCompleteTransfer.
	push af
	push hl
	call SioCompleteTransfer
	pop hl
	pop af
	reti
