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

; Not doing anything, nothing to report
DEF SIO_IDLE      EQU $00
; A transfer is in progress
DEF SIO_ACTIVE    EQU $80
; Transfer completed without error
DEF SIO_DONE      EQU $01
; Transfer ended forcefully
DEF SIO_ABORTED   EQU $02
; Transfer terminated after no response time out period (external clock only)
DEF SIO_TIMED_OUT EQU $03
EXPORT SIO_IDLE, SIO_ACTIVE, SIO_DONE, SIO_ABORTED, SIO_TIMED_OUT
; Mask: all result statuses
DEF SIOF_RESULT   EQU $03
EXPORT SIOF_RESULT

DEF SIO_BUFFER_SIZE EQU 32
EXPORT SIO_BUFFER_SIZE


; PACKET

DEF SIO_PACKET_HEAD_SIZE EQU 2
DEF SIO_PACKET_DATA_SIZE EQU SIO_BUFFER_SIZE - SIO_PACKET_HEAD_SIZE
EXPORT SIO_PACKET_HEAD_SIZE, SIO_PACKET_DATA_SIZE

DEF SIO_PACKET_START EQU $70
DEF SIO_PACKET_END EQU $7F


SECTION "SioBufferRx", WRAM0, ALIGN[8]
wSioBufferRx:: ds SIO_BUFFER_SIZE


SECTION "SioBufferTx", WRAM0, ALIGN[8]
wSioBufferTx:: ds SIO_BUFFER_SIZE


SECTION "SioCore State", WRAM0
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
	push af
	push hl
	call SioPortEnd
	pop hl
	pop af
	reti


SECTION "SioCore Impl", ROM0
; Initialise/reset Sio to the ready to use 'IDLE' state.
; NOTE: Enables the serial interrupt.
; @mut: AF, [IE]
SioInit::
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
	cp a, SIO_ACTIVE
	ret nz
	; update timeout on external clock
	ldh a, [rSC]
	and a, SCF_SOURCE
	ret nz
	ld a, [wSioTimer]
	and a, a
	ret z ; timer == 0, timeout disabled
	dec a
	ld [wSioTimer], a
	ret nz
	ld a, SIO_TIMED_OUT
	ld [wSioState], a
	jp SioPortStop


; Abort the ongoing transfer (if any) and enter the ABORTED result state.
; @mut: AF
SioAbort::
	ld a, SIO_ABORTED
	ld [wSioState], a
	jp SioPortStop


; Start a transfer with a configurable length.
; @param A: transfer count
; @mut: AF, L
SioTransferStart::
	and a, a
	ret z
	ld [wSioCount], a
	ld a, 0
	ld [wSioBufferOffset], a
	jr SioTransferCommit


; Start transfer with current count and buffer position.
; WARNING: Assumes wSioCount and wSioBufferOffset are set to valid values.
; @mut: AF, L
SioTransferCommit:
	; send first byte
	ld a, [wSioBufferTx]
	ldh [rSB], a
	ld a, SIO_ACTIVE
	ld [wSioState], a
	jr SioPortStart


; Enable the serial port, starting a transfer.
; If internal clock is enabled, performs catchup delay before enabling the port.
; Resets the transfer timeout timer.
; @mut: AF, L
SioPortStart:
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
	; reset timeout
	ld a, SIO_TIMEOUT_TICKS
	ld [wSioTimer], a
	ret


; Collects the received value and starts the next transfer, if there is any.
; To be called after the serial port deactivates itself / serial interrupt.
; @mut: AF, HL
SioPortEnd:
	; check that we were expecting a transfer (to end)
	ld hl, wSioCount
	ld a, [hl]
	and a
	ret z

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
	jr nz, .next
	ld a, SIO_DONE
	ld [wSioState], a
	ret
.next:
	; Next value to send
	ld h, HIGH(wSioBufferTx)
	ld a, [hl]
	ldh [rSB], a
	jr SioPortStart


; @mut: AF
SioPortStop:
	ldh a, [rSC]
	res SCB_START, a
	ldh [rSC], a
	ret


SECTION "SioPacket Impl", ROM0
; Initialise the Tx buffer as a packet, ready for data.
; Returns a pointer to the packet data section.
; @return HL: packet data pointer
; @mut: AF, C, HL
SioPacketTxPrepare::
	ld hl, wSioBufferTx
	; packet always starts with constant ID
	ld a, SIO_PACKET_START
	ld [hl+], a
	; checksum = 0 for initial calculation
	ld a, 0
	ld [hl+], a
	; clear packet data
	ld a, SIO_PACKET_END
	ld c, SIO_PACKET_DATA_SIZE
:
	ld [hl+], a
	dec c
	jr nz, :-
	ld hl, wSioBufferTx + SIO_PACKET_HEAD_SIZE
	ret


; @mut: AF, C, HL
SioPacketTxFinalise::
	ld hl, wSioBufferTx
	call SioPacketChecksum
	ld [wSioBufferTx + 1], a
	ret


; @mut: AF, L
SioPacketTransferStart::
	ld a, SIO_BUFFER_SIZE
	jp SioTransferStart


; @return F.Z: if check OK
; @mut: AF, C, HL
SioPacketRxCheck::
	ld hl, wSioBufferRx
	; expect constant
	ld a, [hl]
	cp a, SIO_PACKET_START
	ret nz

	; check the sum
	call SioPacketChecksum
	and a, a
	ret ; F.Z already set (or not)


; Calculate a simple 1 byte checksum of a Sio data buffer.
; sum(buffer + sum(buffer + 0)) == 0
; @param HL: &buffer
; @return A: sum
; @mut: AF, C, HL
SioPacketChecksum:
	ld c, SIO_BUFFER_SIZE
	ld a, c
:
	sub [hl]
	inc hl
	dec c
	jr nz, :-
	ret
