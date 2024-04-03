
include "hw.inc"
include "acharmap.inc"
include "serial.inc"


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
	call handshake_init

.main_iter:
	call obuf_clear
	call input_update

	ld hl, _tick
	inc [hl]

	ldh a, [hKeysPressed]
	ld b, a
	call _process_input

	call handshake_tick

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


; @param B: keys pressed
_process_input:
	ld a, [wHshkState]
	cp HSHK_CONNECTED
	jr z, :+
		bit PADB_A, b :: jp nz, handshake_join
		bit PADB_START, b :: jp nz, handshake_host
:
	bit PADB_B, b :: jp nz, handshake_abort
	ret


include "main_display.inc"
