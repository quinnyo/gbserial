
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

	call serio_init
	call clear_statln
	call redraw
	call lcd_on

.main_iter:
	call input_update

	ld hl, _tick
	inc [hl]

	; press A: start as host
	ldh a, [hKeysPressed]
	bit PADB_A, a
	call nz, do_host_transfer

	; press B: start as guest
	ldh a, [hKeysPressed]
	bit PADB_B, a
	call nz, do_guest_transfer

	call do_timeout_adjust

	call _serio_sync_poll
	; call _serio_sync_wait

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


do_host_transfer:
	ldh a, [hSerStatus]
	cp SERIO_IDLE
	ret nz
	ld a, [_tick]
	and $7F
	ldh [hSerTx], a
	ld d, a
	ld e, "T"
	call collection_append
	jp serio_host_start


do_guest_transfer:
	ldh a, [hSerStatus]
	cp SERIO_IDLE
	ret nz
	ld a, [_tick]
	and $7F
	ldh [hSerTx], a
	ld d, a
	ld e, "t"
	call collection_append
	jp serio_guest_start


do_timeout_adjust:
	ldh a, [hKeysPressed]
	ld b, a
	ld a, [wSerTransferTimeout]
	cp 240
	jr nc, :+
	bit PADB_UP, b
	jr z, :+
	add 8
:
	bit PADB_DOWN, b
	jr z, :+
	sub 8
	jr nc, :+
	xor a
:
	ld [wSerTransferTimeout], a
	ret


; Check if transfer is complete.
; @mut: AF, DE, HL
_serio_sync_poll:
	ldh a, [hSerStatus]
	cp SERIO_WORKING
	jp z, serio_tick ; still working, update timeout
	jr _process_transfer_result


; If there is a transfer pending, wait until it is complete.
_serio_sync_wait:
	jr .serio_sync
.serio_sync_halt:
	call serio_tick
	halt
	nop
	call redraw
.serio_sync:
	ldh a, [hSerStatus]
	cp SERIO_WORKING
	jr z, .serio_sync_halt
	jr _process_transfer_result


; @param A: status
; @mut: AF, DE, HL
_process_transfer_result:
	cp SERIO_DONE
	ret c

	ld e, a
	ldh a, [hSerRx]
	ld d, a
	ld a, SERIO_IDLE
	ldh [hSerStatus], a
	jp collection_append


include "main_display.inc"
