include "hardware.inc"
include "acharmap.inc"

DEF SIOTEST_PKT_DATA_LENGTH EQU 16
DEF SIOTEST_DELAY_EXT EQU 1
DEF SIOTEST_DELAY_INT EQU 1
DEF SIOTEST_RESULTS_COUNT EQU 16


SECTION "SerialDemo/SioTest State", WRAM0

wSioTestFlippy: db
wSioTestStartDelay: db
wSioTestResults: ds SIOTEST_RESULTS_COUNT
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
	ld a, 60
	ld [wSioTestStartDelay], a
	ld hl, wSioTestResults
	ld c, SIOTEST_RESULTS_COUNT
	ld a, "-"
:
	ld [hl+], a
	dec c
	jr nz, :-
	ld a, low(wSioTestResults)
	ld [wSioTestResultsPtr + 0], a
	ld a, high(wSioTestResults)
	ld [wSioTestResultsPtr + 1], a
	ld a, SIOTEST_RESULTS_COUNT
	ld [wSioTestResultsCount], a
	call HandshakeInit
	ret


SioTestUpdate::
	call SioTick
	ld a, [wHandshakeState]
	and a, a
	jr nz, .handshake
	jr .proper

.handshake:
	call HandshakeUpdate
	ld a, 3
	call display_statln_start
	push bc
	ld a, "H" :: ld [hl+], a
	ld a, "S" :: ld [hl+], a
	ld a, [wTick]
	swap a
	and 3
	jr z, .dots_done
	ld c, a
	ld a, "."
:
	ld [hl+], a
	dec c
	jr nz, :-
.dots_done
	pop bc
	jp display_clear_to

.proper:
	call SioTestFlushStatus
	call SioTestStartAuto
	jp SioTestDraw


SioTestStartThing:
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
	call SioPacketTxPrepare
	ld bc, SIOTEST_PKT_DATA_LENGTH
	call memcpy
	call SioPacketTxFinalise
	jp SioPacketTransferStart


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


SioTestFlushStatus:
	ld a, [wSioState]
	ld b, "F"
	cp SIO_FAILED
	jr z, .result
	cp SIO_DONE
	ret nz
	call SioPacketRxCheck
	ld b, "^yes^"
	jr z, .result
	ld b, "^no^"
.result
	; Clear done/failed status
	ld a, SIO_IDLE
	ld [wSioState], a
	call SioTestPushResult
	jr SioTestDrawResults


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
	ld a, SIOTEST_RESULTS_COUNT
:
	dec a
	ld [wSioTestResultsCount], a

	ld a, b
	ld [hl+], a
	ld a, l
	ld [wSioTestResultsPtr + 0], a
	ld a, h
	ld [wSioTestResultsPtr + 1], a
	ret


SioTestDraw:
	; draw Rx buffer
	ld a, 3
	call display_statln_start
	push bc

	ld a, [wSioBufferOffset]
	ld b, a
	ld a, "^rx^"
	ld [hl+], a
	ld a, " "
	ld [hl+], a
	ld a, "'"
	ld [hl+], a
	ld de, wSioBufferRx + 2
	ld c, SIOTEST_PKT_DATA_LENGTH
.loop
	; stop early if we caught up to the Rx pointer
	ld a, e
	cp b
	jr nc, .loop_break
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
	ld b, SIOTEST_RESULTS_COUNT
	ld a, [wSioTestResultsCount]
	ld c, a
	ld a, b
	sub c
	ld c, a
	inc c ; one after last
.results_loop
	ld a, " "
	dec c
	jr z, :+
	ld a, [de] :: inc de
:
	ld [hl+], a
	dec b
	jr nz, .results_loop
	ret
