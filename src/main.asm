
include "hw.inc"
include "acharmap.inc"


section "main", rom0

main::
	call load_tiles
	call load_afont

	xor a
	ld hl, startof("wMainDisplay")
	ld c, sizeof("wMainDisplay")
:
	ld [hl+], a
	dec c
	jr nz, :-

; bg thing
	ldh a, [rDIV]
	ld d, a
	ld hl, BGMAP
	ld b, $20
.iter_rows
	ld c, $20
.iter_cols
	ldh a, [rDIV]
	add d
	ld d, a
	and $03
	add $80
	ld [hl+], a
	dec c
	jr nz, .iter_cols
	dec b
	jr nz, .iter_rows

	call clear_statln
	call lcd_on
	call serialdemo_init

.main_iter:
	call obuf_clear
	call input_update

	ldh a, [hKeysPressed]
	ld b, a
	call serialdemo_update

	call obuf_next
	ld a, [wTick]
	srl a
	and $1F
	add OAM_Y_OFS + 64
	ld [hl+], a
	ld a, [wTick]
	and $7F
	add OAM_X_OFS
	ld [hl+], a
	ld a, [wTick]
	and $3
	ld [hl+], a
	ld a, OAMF_PAL1
	ld [hl+], a

	; drawing status waits for vsync so put it after everything else.
	call display_flush

	jr .vsync
.vsync_halt:
	halt
	nop
.vsync:
	ldh a, [hVBlankDone]
	and a
	jr z, .vsync_halt
.vsync_done:
	xor a
	ldh [hVBlankDone], a
	jr .main_iter


include "main_display.inc"





;;;;;;;;;;;;;;;;;;;;;;;;
; serial demo
;;;;;;;;;;;;;;;;;;;;;;;;


rsreset
def SERIAL_STATE_INIT rb 1
def SERIAL_STATE_THING rb 1


section "wSerialdemo", wram0

wSerialState: db
wPktGen: db

_last_draw_essentials_sec: db

section "serialdemo", rom0

serialdemo_init:
	call SioInit
	call serial_clock_init

	ld a, SERIAL_STATE_INIT
	ld [wSerialState], a

	xor a
	ld [wPktGen], a
	ld a, $FF
	ld [_last_draw_essentials_sec], a

	jp timer_enable


; @param B: keys pressed
serialdemo_update:
	call _process_input
	call _state_update

	ld a, [_last_draw_essentials_sec] :: ld b, a
	ld a, [wSerialClock.seconds]
	cp b
	jr z, :+
		ld [_last_draw_essentials_sec], a
		xor a
		call display_statln_start
		push bc
		call draw_essentials
		pop bc
		call display_clear_to
:

	ld a, 1
	call display_statln_start
	push bc
	call draw_statln_sc_sb
	pop bc
	call display_clear_to

	ld a, 2
	call display_statln_start
	push bc
	call draw_sio_info
	pop bc
	call display_clear_to

	ret


; @param B: keys pressed
_process_input:
	bit PADB_START, b
	jr nz, _toggle_intclk
	ret


_reset:
	call display_clear
	ld a, SERIAL_STATE_INIT
	ld [wSerialState], a
	ret


_toggle_intclk:
	call display_clear
	ld a, [wSioConfig]
	xor SIO_CONFIG_INTCLK
	ld [wSioConfig], a
	ret


_start_thing:
	call SioTestInit
	ld a, SERIAL_STATE_THING
	ld [wSerialState], a
	ret


; @param B: keys pressed
_state_update:
	ld a, [wSerialState]
	cp SERIAL_STATE_INIT :: jr z, .update_init
	cp SERIAL_STATE_THING :: jp z, SioTestUpdate
	ret
.update_init:
	jr _start_thing
	ret


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DEF SIOTEST_PKT_DATA_LENGTH EQU 16
DEF SIOTEST_PKT_HEADER_LENGTH EQU 2
DEF SIOTEST_PKT_TOTAL_LENGTH EQU SIOTEST_PKT_DATA_LENGTH + SIOTEST_PKT_HEADER_LENGTH
DEF SIOTEST_PKT_START EQU $77
DEF SIOTEST_DELAY_EXT EQU 1
DEF SIOTEST_DELAY_INT EQU 2


SECTION "SerialDemo/SioTest State", WRAM0

wSioTestFlippy: db
wSioTestStartDelay: db

wSioTestBufferRx: ; ds SIOTEST_PKT_TOTAL_LENGTH
	.id: db
	.check: db
	.data: ds SIOTEST_PKT_DATA_LENGTH
	.end:
wSioTestBufferTx: ; ds SIOTEST_PKT_TOTAL_LENGTH
	.id: db
	.check: db
	.data: ds SIOTEST_PKT_DATA_LENGTH
	.end:

wSioTestResults: ds 16
	.end:
wSioTestResultsPtr: dw
wSioTestResultsCount: db


SECTION "SerialDemo/SioTest Impl", ROM0

SioTestDataA:
	db "A short message!"

SioTestDataB:
	db "Bees == cute fyi"


SioTestInit::
	xor a
	ld [wSioTestFlippy], a
	ld [wSioTestStartDelay], a
	ld c, wSioTestBufferRx.end - wSioTestBufferRx
	ld hl, wSioTestBufferRx
:
	ld [hl+], a
	dec c
	jr nz, :-
	ld c, wSioTestBufferTx.end - wSioTestBufferTx
	ld hl, wSioTestBufferTx
:
	ld [hl+], a
	dec c
	jr nz, :-
	ld hl, wSioTestResults
	ld c, wSioTestResults.end - wSioTestResults
	ld a, "-"
:
	ld [hl+], a
	dec c
	jr nz, :-
	ld a, low(wSioTestResults)
	ld [wSioTestResultsPtr + 0], a
	ld a, high(wSioTestResults)
	ld [wSioTestResultsPtr + 1], a
	ld a, wSioTestResults.end - wSioTestResults
	ld [wSioTestResultsCount], a
	ret


SioTestUpdate::
	call SioTestCheckStatus
	call SioTestStartAuto
	call SioTick
	call SioTestDraw
	ret


SioTestStartThing:
	; set Rx pointer
	ld de, wSioTestBufferRx
	ld hl, wSioRxPtr
	ld a, e
	ld [hl+], a
	ld [hl], d

	; set Tx pointer
	ld de, wSioTestBufferTx
	ld hl, wSioTxPtr
	ld a, e
	ld [hl+], a
	ld [hl], d

	; Copy test data to packet buffer
	ld de, SioTestDataA
	ld a, [wSioTestFlippy]
	ld b, a
	cpl
	ld [wSioTestFlippy], a

	ld a, [wSioConfig]
	xor b
	and SIO_CONFIG_INTCLK
	jr nz, :+
	ld de, SioTestDataB
:
	call SioTestPacketTxPrepare
	ld bc, SIOTEST_PKT_DATA_LENGTH
	call memcpy
	call SioTestPacketTxFinalise

	; set count and request start
	ld a, wSioTestBufferRx.end - wSioTestBufferRx
	ld [wSioCount], a
	ld a, SIO_XFER_START
	ld [wSioState], a
	ret


SioTestStartAuto:
	ld a, [wSioState]
	cp SIO_BUSY
	ret nc

	; tick delay timer, start thing at zero
	ld a, [wSioTestStartDelay]
	and a
	jr z, .go_forth
	dec a
	ld [wSioTestStartDelay], a
	ret
.go_forth
	; Reset delay timer
	ld b, SIOTEST_DELAY_INT
	ld a, [wSioConfig]
	and SIO_CONFIG_INTCLK
	jr nz, :+
	ld b, SIOTEST_DELAY_EXT
:
	ld a, b
	ld [wSioTestStartDelay], a
	jr SioTestStartThing


; Prepare Tx packet buffer for writing.
; @return HL: packet data pointer
; @mut: AF, C, HL
SioTestPacketTxPrepare:
	ld hl, wSioTestBufferTx
	; packet always starts with constant ID
	ld a, SIOTEST_PKT_START
	ld [hl+], a
	; checksum = 0 for initial calculation
	ld a, 0
	ld [hl+], a
	ld c, SIOTEST_PKT_DATA_LENGTH
:
	ld [hl+], a
	dec c
	jr nz, :-
	ld hl, wSioTestBufferTx.data
	ret


; @mut: AF, C, HL
SioTestPacketTxFinalise:
	ld hl, wSioTestBufferTx
	ld c, wSioTestBufferTx.end - wSioTestBufferTx
	call Checksum8
	ld [wSioTestBufferTx.check], a
	ret


; @return F.Z: if check OK
; @mut: AF, C, HL
SioTestPacketRxCheck:
	; expect constant
	ld a, [wSioTestBufferRx.id]
	cp a, SIOTEST_PKT_START
	ret nz

	; check the sum
	ld hl, wSioTestBufferRx
	ld c, wSioTestBufferRx.end - wSioTestBufferRx
	call Checksum8
	and a, a
	ret ; F.Z already set (or not)


SioTestCheckStatus:
	ld b, "^no^"
	ld a, [wSioState]
	cp SIO_FAILED
	jr z, .result
	cp SIO_DONE
	ret nz ; no result yet
	ld b, "^yes^"
	call SioTestPacketRxCheck
	jr z, .result
	ld b, "!"
.result
	; Clear done/failed status
	ld a, SIO_IDLE
	ld [wSioState], a
	jr SioTestPushResult


; @param B: result
SioTestPushResult:
	ld hl, wSioTestResultsPtr
	ld a, [hl+]
	ld h, [hl]
	ld l, a

	ld a, [wSioTestResultsCount]
	and a, a
	jr nz, :+
	; Count == 0: go to start
	ld hl, wSioTestResults
	ld a, wSioTestResults.end - wSioTestResults
:
	dec a
	ld [wSioTestResultsCount], a

	ld a, b
	ld [hl+], a
	ld a, l
	ld [wSioTestResultsPtr + 0], a
	ld a, h
	ld [wSioTestResultsPtr + 1], a

	jr SioTestDrawResults


SioTestDraw:
	; draw Rx buffer
	ld a, 3
	call display_statln_start
	push bc
	ld a, "^rx^"
	ld [hl+], a
	ld a, " "
	ld [hl+], a
	ld a, "'"
	ld [hl+], a
	ld de, wSioTestBufferRx.data
	ld c, SIOTEST_PKT_DATA_LENGTH
.loop
	; stop early if we caught up to the Rx pointer
	ld a, [wSioRxPtr + 1]
	cp d
	jr nz, :+
	ld a, [wSioRxPtr + 0]
	cp e
	jr z, .loop_break
:
	ld a, [de]
	inc de
	ld [hl+], a
	dec c
	jr nz, .loop
.loop_break
	ld a, "'"
	ld [hl+], a
	pop bc
	call display_clear_to
	ret


SioTestDrawResults:
	ld a, 4
	call display_statln_start
	ld de, wSioTestResults
	ld a, [wSioTestResultsCount]
	ld c, a
	ld a, wSioTestResults.end - wSioTestResults
	sub c
	ld c, a
:
	ld a, [de]
	inc de
	ld [hl+], a
	dec c
	jr nz, :-
	ld a, " "
	ld [hl+], a
	ret


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

include "hw.inc"


section "irq/timer", rom0[$50]
irq_timer:
	jp irq_timer_handler


section "SerialDemo/TimerThing", rom0

timer_enable::
;	ld a, $FF ; 4096 /   1 = 4096 Hz
;	ld a, $FE ; 4096 /   2 = 2048 Hz
	ld a, $FC ; 4096 /   4 = 1024 Hz
;	ld a, $F8 ; 4096 /   8 =  512 Hz
;	ld a, $80 ; 4096 / 128 =   32 Hz
;	ld a, $00 ; 4096 / 256 =   16 Hz
	ldh [rTMA], a
	ld a, %100
	ldh [rTAC], a
	ldh a, [rIE]
	or IEF_TIMER
	ldh [rIE], a
	ret


irq_timer_handler:
	push af
	push hl

	; NOTE: this is hardcoded to "work" (as it does) with an interrupt frequency of 1024 Hz.
	ld hl, wSerialClock.ticks
	inc [hl] ; ticks++
	jr nz, :+
		ld a, 256 - SERIAL_CLOCK_TICKS :: ld [hl+], a
		inc [hl] ; periods++
		jr nz, :+
			ld a, 256 - SERIAL_CLOCK_PERIODS :: ld [hl+], a
			inc [hl] ; seconds++
			jr nz, :+
				ld a, 256 - SERIAL_CLOCK_SECONDS :: ld [hl+], a
				inc [hl] ; minutes++
:
	pop hl
	pop af
	reti


def SERIAL_CLOCK_TICKS   equ 256
def SERIAL_CLOCK_PERIODS equ 4
def SERIAL_CLOCK_SECONDS equ 60

section "wSerialClock", wram0
; Time keeping in serial time.
wSerialClock::
	.ticks:: db
	.periods:: db
	.seconds:: db
	.minutes:: db


section "SerialClock", rom0

serial_clock_init::
	ld a, 256 - SERIAL_CLOCK_TICKS :: ld [wSerialClock.ticks], a
	ld a, 256 - SERIAL_CLOCK_PERIODS :: ld [wSerialClock.periods], a
	ld a, 256 - SERIAL_CLOCK_SECONDS :: ld [wSerialClock.seconds], a
	xor a :: ld [wSerialClock.minutes], a
	ret


; @param HL: &dest
; @mut: AF, B, HL
draw_serial_time::
	ld a, [wSerialClock.minutes] :: ld b, a
	call utile_print_h8
	ld a, ":" :: ld [hl+], a
	ld a, [wSerialClock.seconds]
	sub 256 - SERIAL_CLOCK_SECONDS
	ld b, a
	jp utile_print_h8

;
; vim:ft=rgbasm
