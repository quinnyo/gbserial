
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
	call redraw
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
	call redraw

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


section "serialdemo", rom0

serialdemo_init:
	call serio_init
	call handshake_init

	ld a, SERIAL_STATE_HANDSHAKE
	ld [wSerialState], a

	xor a
	ld [wTick + 0], a
	ld [wTick + 1], a
	ld [wPktGen], a

	ret


; @param B: keys pressed
serialdemo_update:
	call _process_input
	call _state_update

	ld hl, wTick
	inc [hl]
	jr nz, :+
	inc hl
	inc [hl]
:
	ret


; @param B: keys pressed
_process_input:
	bit PADB_B, b
	jp z, :+
		call packet_stop
		call handshake_abort
		ld a, SERIAL_STATE_INIT
		ld [wSerialState], a
		ret
:
	ret


_join:
	ld a, SERIAL_STATE_HANDSHAKE
	ld [wSerialState], a
	jp handshake_join


_host:
	ld a, SERIAL_STATE_HANDSHAKE
	ld [wSerialState], a
	jp handshake_host


_packet_start:
	ld a, SERIAL_STATE_PACKET
	ld [wSerialState], a
	jp packet_init
	ret


_blaster_start:
	ld a, SERIAL_STATE_BLASTER
	ld [wSerialState], a
	ret


; @param B: keys pressed
_state_update:
	ld a, [wSerialState]
	cp SERIAL_STATE_INIT :: jr z, .update_init
	cp SERIAL_STATE_HANDSHAKE :: jr z, .update_hshk
	cp SERIAL_STATE_PACKET :: jr z, .update_packet
	cp SERIAL_STATE_BLASTER :: jr z, .update_blaster
	ret
.update_init:
	bit PADB_A, b :: jr nz, _join
	bit PADB_START, b :: jr nz, _host
	ret
.update_hshk:
	call _process_input_hshk
	call serio_tick
	ret
.update_packet:
	call _process_input_packet
	call serio_tick
	ret
.update_blaster:
	ldh a, [hSerStatus]
	bit SIOSTB_QUEUE, a
	jr nz, :+
	ldh a, [hSerTx]
	add b
	call serio_transmit
	call serio_continue
:
	call serio_tick
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
	bit PADB_SELECT, b :: jr nz, _blaster_start
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

