
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
	and $07
	add OAM_Y_OFS + 16
	ld [hl+], a
	ld a, [_tick]
	and $7F
	add OAM_X_OFS + 24
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

include "serial.inc"
include "packet.inc"


rsreset
def SERIAL_STATE_INIT rb 1
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
	call serio_init
	call handshake_init
	call serial_clock_init

	ld a, SERIAL_STATE_HANDSHAKE
	ld [wSerialState], a

	xor a
	ld [wTick + 0], a
	ld [wTick + 1], a
	ld [wPktGen], a
	ld a, $FF
	ld [_last_draw_essentials_sec], a

	ld a, 255
	ld [wSerTransferTimeout], a
	call timer_enable

	ret


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
	call draw_statln_serio
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
	bit PADB_SELECT, a
	jr nz, :+
		bit PADB_B, b
		jr nz, _reset
		ret
:
	call .select
	ld b, 0
	ret
.select
	ld a, [wSerConfig]
	bit SERIO_CFGB_ROLE, a
	jr z, .guest
	bit PADB_UP, b
	jr nz, .next_xfer_end_delay
	ret
.guest
	ret
.next_xfer_end_delay
	ld a, [wSerHostXferEndDelay]
	inc a
	cp 4
	jr c, :+
	ld a, 0
:
	ld [wSerHostXferEndDelay], a
	ret


_reset:
	call packet_stop
	call handshake_abort
	call display_clear
	ld a, SERIAL_STATE_INIT
	ld [wSerialState], a
	ret


_join:
	call display_clear
	ld a, SERIAL_STATE_HANDSHAKE
	ld [wSerialState], a
	jp handshake_join


_host:
	call display_clear
	ld a, SERIAL_STATE_HANDSHAKE
	ld [wSerialState], a
	jp handshake_host


_packet_start:
	call display_clear
	ld a, SERIAL_STATE_PACKET
	ld [wSerialState], a
	jp packet_init


_blaster_start:
	call display_clear
	ld a, SERIAL_STATE_BLASTER
	ld [wSerialState], a
	call serial_blaster_start
	ret


; @param B: keys pressed
_state_update:
	ld a, [wSerialState]
	cp SERIAL_STATE_INIT :: jr z, .update_init
	cp SERIAL_STATE_HANDSHAKE :: jr z, .update_hshk
	cp SERIAL_STATE_PACKET :: jr z, .update_packet
	cp SERIAL_STATE_BLASTER :: jp z, serial_blaster_update
	ret
.update_init:
	bit PADB_A, b :: jr nz, _join
	bit PADB_START, b :: jr nz, _host
	ret
.update_hshk:
	call _process_input_hshk

	ld a, 2
	call display_statln_start
	push bc
	call draw_hshk
	pop bc
	call display_clear_to

	ret
.update_packet:
	call _process_input_packet

	ld a, 2
	call display_statln_start
	push bc
	call draw_packet_sys
	pop bc
	call display_clear_to

	ld a, 3
	call display_statln_start
	ld a, "^tx^" :: ld [hl+], a
	ld c, PKT_DATA_SZ
	ld de, _packet_tx
	call draw_packet_buffer

	ld a, 4
	call display_statln_start
	ld a, "^rx^" :: ld [hl+], a
	ld c, PKT_DATA_SZ
	ld de, _packet_rx
	call draw_packet_buffer

	ret


; @param B: keys pressed
_process_input_hshk:
	ld a, [wHshkState]
	cp HSHK_CONNECTED
	jr z, :+
		bit PADB_A, b :: jp nz, handshake_join
		bit PADB_START, b :: jp nz, handshake_host
		ret
:
	bit PADB_A, b :: jp nz, _packet_start
	bit PADB_RIGHT, b :: jp nz, _blaster_start
	ret


; @param B: keys pressed
_process_input_packet:
	; A: start packet send
	; L/R/U/D: build (and send) next packet
	ld a, [wPacketStateLoc]
	and PKST_MAJOR
	jr z, :+
	cp PKST_MAJ_RESULT :: ret nz
:

	bit PADB_A, b :: jp nz, packet_tx_begin
	bit PADB_LEFT, b :: jr nz, _build_packet_good
	bit PADB_DOWN, b :: jr nz, _build_packet_gen
	bit PADB_RIGHT, b :: jr nz, _build_packet_random
	ret


; known-good packet data
; @mut: AF, BC, DE, HL
_build_packet_good:
	ld de, rGoodData2
	ld a, [wSerConfig]
	bit SERIO_CFGB_ROLE, a
	jr z, :+
	ld de, rGoodData1
:
	ld bc, PKT_DATA_SZ
	ld hl, _packet_tx.data
	call memcpy
	jp packet_tx_finalise


rGoodData1: db $50, $51, $52, $53, $54, $55, $56, $57
rGoodData2: db $58, $59, $5A, $5B, $5C, $5D, $5E, $5F


; not very random packet data
; @mut: AF, C, DE, HL
_build_packet_random:
	ld hl, _packet_tx.data
	ld c, PKT_DATA_SZ
	ld a, [wTick + 0]
	ld e, a
	ld a, [wTick + 1]
	xor e
:
		ld [hl+], a
		inc a
		dec c
	jr nz, :-
	jp packet_tx_finalise


; simple pattern of repeating generation number
_build_packet_gen:
	ld hl, _packet_tx.data
	ld c, PKT_DATA_SZ
	ld a, [wPktGen]
:
		ld [hl+], a
		dec c
	jr nz, :-
	inc a
	ld [wPktGen], a
	jp packet_tx_finalise




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
	push bc
	push de
	push hl
	call serio_tick

	; NOTE: this is hardcoded to "work" (as it does) with an interrupt frequency of 1024 Hz.
	ld hl, wSerialClock.ticks
	inc [hl] ; ticks++
	jr nz, :+
		ld a, 256 - SERIAL_CLOCK_TICKS :: ld [hl+], a
		; xor a ; 256 ticks = 1 period
		; ld [hl+], a
		inc [hl] ; periods++
		jr nz, :+
			ld a, 256 - SERIAL_CLOCK_PERIODS :: ld [hl+], a
			; ld a, 256 - 4 ; 4 periods = 1 second
			; ld [hl+], a
			inc [hl] ; seconds++
			jr nz, :+
				ld a, 256 - SERIAL_CLOCK_SECONDS :: ld [hl+], a
				; ld a, 256 - 60 ; 60 seconds = 1 minute
				; ld [hl+], a
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
