
include "hw.inc"
include "acharmap.inc"


section "wMain", wram0
_tick: db


section "main", rom0

main::
	call load_tiles
	call load_afont

	xor a
	ld [_tick], a
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

	ld hl, _tick
	inc [hl]

	ldh a, [hKeysPressed]
	ld b, a
	call serialdemo_update

	call obuf_next
	ld a, [_tick]
	srl a
	and $1F
	add OAM_Y_OFS + 64
	ld [hl+], a
	ld a, [_tick]
	and $7F
	add OAM_X_OFS
	ld [hl+], a
	ld a, [_tick]
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
def SERIAL_STATE_HANDSHAKE rb 1
def SERIAL_STATE_BUILD_PACKET rb 1
def SERIAL_STATE_PACKET rb 1
def SERIAL_STATE_BLASTER rb 1


section "wSerialdemo", wram0

wSerialState: db
wTick: dw
wPktGen: db

_last_draw_essentials_sec: db

section "serialdemo", rom0

serialdemo_init:
	call SioInit
	call serial_clock_init

	ld a, SERIAL_STATE_INIT
	ld [wSerialState], a

	xor a
	ld [wTick + 0], a
	ld [wTick + 1], a
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

	ld hl, wTick
	inc [hl]
	jr nz, :+
	inc hl
	inc [hl]
:
	ret


; @param B: keys pressed
_process_input:
	ldh a, [hKeys]
	ret
;	bit PADB_SELECT, a
;	jr nz, :+
;		bit PADB_B, b
;		jr nz, _reset
;		ret
;:
;	call .select
;	ld b, 0
;	ret
;.select
;	ld a, [wSioConfig]
;	and SIO_CONFIG_INTCLK
;	jr z, .guest
;	bit PADB_UP, b
;	jr nz, .next_xfer_end_delay
;	ret
;.guest
;	ret
;.next_xfer_end_delay
;	ld a, [wSerHostXferEndDelay]
;	inc a
;	cp 4
;	jr c, :+
;	ld a, 0
;:
;	ld [wSerHostXferEndDelay], a
;	ret


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


; _blaster_start:
; 	call display_clear
; 	ld a, SERIAL_STATE_BLASTER
; 	ld [wSerialState], a
; 	call serial_blaster_start
; 	ret


; @param B: keys pressed
_state_update:
	ld a, [wSerialState]
	cp SERIAL_STATE_INIT :: jr z, .update_init
	cp SERIAL_STATE_THING :: jp z, SioTestUpdate
	ret
.update_init:
	bit PADB_START, b
	jr nz, _toggle_intclk
	bit PADB_A, b
	jr nz, _start_thing
	ret


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DEF SIOTEST_BUFFER_SIZE EQU 32
DEF SIOTEST_XFER_COUNT EQU 16

SECTION "SerialDemo/SioTest State", WRAM0

wSioTestFlippy: db

wSioTestBufferRx: ds SIOTEST_BUFFER_SIZE

SECTION "SerialDemo/SioTest Impl", ROM0

SioTestDataA:
	db "A short message!"

SioTestDataB:
	db "Bees == cute fyi"


SioTestInit::
	xor a
	ld [wSioTestFlippy], a
	ld c, SIOTEST_BUFFER_SIZE
	ld hl, wSioTestBufferRx
:
	ld [hl+], a
	dec c
	jr nz, :-
	ret


SioTestUpdate::
	call SioTestDraw

	; Start transfer (press A && not transferring)
	ldh a, [hKeysPressed]
	bit PADB_A, a
	jr z, :+
	ld a, [wSioCount]
	and a
	jr z, SioTestStartThing
	ld a, [wSioState]
	cp SIO_XFER_FAILED
	jr z, SioTestStartThing
:
	ret


SioTestStartThing:
	; set Rx pointer and transfer count
	ld de, wSioTestBufferRx
	ld hl, wSioRxPtr
	ld a, e
	ld [hl+], a
	ld [hl], d

	; set Tx pointer
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
	ld hl, wSioTxPtr
	ld a, e
	ld [hl+], a
	ld [hl], d

	; set count last
	ld a, SIOTEST_XFER_COUNT
	ld [wSioCount], a
	ret


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
	ld de, wSioTestBufferRx
	ld c, SIOTEST_XFER_COUNT
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; SerialBlaster
if 0

def BLAST_HOST equ $FA
def BLAST_GUEST equ $81

def SERIAL_BLASTER_DIV_OK_DEFAULT    equ $80
def SERIAL_BLASTER_DIV_ERROR_DEFAULT equ $01

section "wSerialBlaster", wram0
; Number of expected values received (consecutively) to count as one OK.
_div_ok: db
_raw_ok: db
_count_ok: db
; Number of unexpected values received (consecutively) to count as one ERROR.
_div_error: db
_raw_error: db
_count_error: db
_count_timeout: db


section "SerialBlaster", rom0

serial_blaster_start::
	ld a, low(_on_xfer_end) :: ld [wSerioOnXferEnd + 0], a
	ld a, high(_on_xfer_end) :: ld [wSerioOnXferEnd + 1], a
	xor a
	ld [_count_ok], a
	ld [_count_error], a
	ld [_count_timeout], a
	ld a, SERIAL_BLASTER_DIV_OK_DEFAULT
	ld [_div_ok], a
	ld [_raw_ok], a
	ld a, SERIAL_BLASTER_DIV_ERROR_DEFAULT
	ld [_div_error], a
	ld [_raw_error], a
	jp _start_next_xfer


; @param B: keys pressed
serial_blaster_update::
	call _blaster_input

	ld a, 2
	call display_statln_start
	push bc
;	ld a, "B" :: ld [hl+], a
;	ld a, "l" :: ld [hl+], a
;	ld a, "a" :: ld [hl+], a
;	ld a, "s" :: ld [hl+], a
;	ld a, "t" :: ld [hl+], a
;	ld a, "e" :: ld [hl+], a
;	ld a, "r" :: ld [hl+], a
	ld a, "^yes^" :: ld [hl+], a
	ld a, [_div_ok] :: ld b, a :: call utile_print_h8
	ld a, ":" :: ld [hl+], a
	ld a, [_count_ok] :: ld b, a :: call utile_print_h8
	ld a, " " :: ld [hl+], a
	ld a, " " :: ld [hl+], a
	ld a, "^no^" :: ld [hl+], a
	ld a, [_div_error] :: ld b, a :: call utile_print_h8
	ld a, ":" :: ld [hl+], a
	ld a, [_count_error] :: ld b, a :: call utile_print_h8
	ld a, " " :: ld [hl+], a
	ld a, " " :: ld [hl+], a
	ld a, "T" :: ld [hl+], a
	ld a, [_count_timeout] :: ld b, a :: call utile_print_h8
	pop bc
	call display_clear_to

	ret


; @param B: keys pressed
_blaster_input:
	;    UP/DOWN: adjust (+/-) OK divider
	; RIGHT/LEFT: adjust (+/-) ERROR divider
	;      START: reset counts
	bit PADB_UP, b
	jr z, :+
	ld a, [_div_ok]
	rlca
	ld [_div_ok], a
:
	bit PADB_DOWN, b
	jr z, :+
	ld a, [_div_ok]
	rrca
	ld [_div_ok], a
:
	bit PADB_RIGHT, b
	jr z, :+
	ld a, [_div_error]
	rlca
	ld [_div_error], a
:
	bit PADB_LEFT, b
	jr z, :+
	ld a, [_div_error]
	rrca
	ld [_div_error], a
:
	bit PADB_START, b
	jr z, :+
	xor a
	ld [_count_ok], a
	ld [_count_error], a
	ld [_count_timeout], a
:
	ret


_on_xfer_end:
	ldh a, [hSerStatus]
	bit SIOSTB_XFER_TIMEOUT, a
	jr nz, _timeout
	ld b, BLAST_HOST
	ld a, [wSerConfig]
	and SERIO_CFGF_HOST
	jr z, :+
	ld b, BLAST_GUEST
:
	ldh a, [hSerRx]
	cp b
	jr z, _ok

_error:
	ld a, [_div_ok] :: ld [_raw_ok], a
	ld hl, _raw_error :: dec [hl]
	jr nz, _start_next_xfer
	ld a, [_div_error] :: ld [hl], a
	ld hl, _count_error :: inc [hl]
	jr _start_next_xfer
_timeout:
	ld a, [_div_ok] :: ld [_raw_ok], a
	ld a, [_div_error] :: ld [_raw_error], a
	ld hl, _count_timeout :: inc [hl]
	jr _start_next_xfer
_ok:
	ld a, [_div_error] :: ld [_raw_error], a
	ld hl, _raw_ok :: dec [hl]
	jr nz, _start_next_xfer
	ld a, [_div_ok] :: ld [hl], a
	ld hl, _count_ok :: inc [hl]
_start_next_xfer:
	ld b, BLAST_GUEST
	ld a, [wSerConfig]
	and SERIO_CFGF_HOST
	jr z, :+
	ld b, BLAST_HOST
:
	ld a, b
	call serio_transmit
	jp serio_continue

endc ; 0


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

include "hw.inc"


section "irq/timer", rom0[$50]
irq_timer:
	jp irq_timer_handler


section "SerialDemo/TimerThing", rom0

timer_enable::
;	ld a, $FF ; 4096 /   1 = 4096 Hz
;	ld a, $FE ; 4096 /   2 = 2048 Hz
;	ld a, $FC ; 4096 /   4 = 1024 Hz
;	ld a, $F8 ; 4096 /   8 =  512 Hz
;	ld a, $80 ; 4096 / 128 =   32 Hz
	ld a, $00 ; 4096 / 256 =   16 Hz
	ldh [rTMA], a
	ld a, %100
	ldh [rTAC], a
	ldh a, [rIE]
	or IEF_TIMER
	ldh [rIE], a
	ret


irq_timer_handler:
	push af
	push bc
	push de
	push hl
	call SioTick

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
	pop de
	pop bc
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
