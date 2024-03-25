
include "hw.inc"
include "acharmap.inc"
include "serial.inc"


def BGMAP equ $9800
def STATLN_DEST equ BGMAP
def STATLN_BG_CHR equ " "
def STATLN_LINE_LEN equ 20
def STATLN_LINE_COUNT equ 2

def COLLITEM_SIZE equ 2
def COLLECTION_MAX_COUNT equ 8
def COLLDISP_DEST equ BGMAP + (STATLN_LINE_COUNT + 1) * 32
def COLLDISP_ITEM_WIDTH equ 4
def COLLDISP_COLUMNS equ 2


section "wMain", wram0
_tick: db

_statln: ds STATLN_LINE_COUNT * STATLN_LINE_LEN

_collection: ds COLLECTION_MAX_COUNT * COLLITEM_SIZE
_collection_count: db
_collection_display_buffer: ds COLLECTION_MAX_COUNT * COLLDISP_ITEM_WIDTH


section "main", rom0

main::
	call load_tiles
	call load_afont

	xor a
	ld hl, startof("wMain")
	ld c, sizeof("wMain")
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
	ldh a, [_serio_status]
	cp SERIO_IDLE
	ret nz
	ld a, [_tick]
	and $7F
	ldh [_serio_tx], a
	ld d, a
	ld e, "T"
	call log_event
	jp serio_host_start


do_guest_transfer:
	ldh a, [_serio_status]
	cp SERIO_IDLE
	ret nz
	ld a, [_tick]
	and $7F
	ldh [_serio_tx], a
	ld d, a
	ld e, "t"
	call log_event
	jp serio_guest_start


do_timeout_adjust:
	ldh a, [hKeysPressed]
	ld b, a
	ldh a, [hSerialTransferTimeout]
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
	ldh [hSerialTransferTimeout], a
	ret


; Check if transfer is complete.
; @mut: AF, DE, HL
_serio_sync_poll:
	ldh a, [_serio_status]
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
	ldh a, [_serio_status]
	cp SERIO_WORKING
	jr z, .serio_sync_halt
	jr _process_transfer_result


; @param A: status
; @mut: AF, DE, HL
_process_transfer_result:
	cp SERIO_DONE
	ret c

	ld e, a
	ldh a, [_serio_rx]
	ld d, a
	ld a, SERIO_IDLE
	ldh [_serio_status], a
	jr log_event


; @return A:
_get_collection_count:
	ld a, [_collection_count]
	cp COLLECTION_MAX_COUNT
	jr c, :+
	xor a
:
	ret


; @param E: event code
; @param D: event value
; @mut: AF, HL
log_event:
	call _get_collection_count
	assert COLLITEM_SIZE == 2
	inc a
	ld [_collection_count], a
	dec a
	add a ; mul 2
	ld hl, _collection
	add l
	ld l, a
	adc h
	sub l
	ld h, a

	ld a, d
	ld [hl+], a
	ld a, e
	ld [hl+], a
	ret


redraw:
	ld hl, _statln
	call draw_statln_serio
	ld hl, _statln + STATLN_LINE_LEN
	call draw_statln_sc_sb
	ld hl, _collection_display_buffer
	call draw_collection
	jp flush_display


clear_statln:
	ld hl, _statln
	ld c, STATLN_LINE_COUNT * STATLN_LINE_LEN
	ld a, STATLN_BG_CHR
:
	ld [hl+], a
	dec c
	jr nz, :-
	ret


; @param HL: &dest
draw_statln_serio:
	ldh a, [_serio_status]
	call fmt_serio_status
	ld a, STATLN_BG_CHR
	ld [hl+], a

	ldh a, [hTimeoutTicks]
	ld b, a
	call utile_print_h8
	ld a, "/"
	ld [hl+], a
	ldh a, [hSerialTransferTimeout]
	ld b, a
	call utile_print_h8
	ld a, STATLN_BG_CHR
	ld [hl+], a

	ldh a, [_serio_tx]
	ld b, a
	ldh a, [_serio_rx]
	ld c, a
	jp fmt_txrx


; @param HL: &dest
draw_statln_sc_sb:
	ldh a, [rSC]
	ld b, a
	call fmt_SC
	ld a, STATLN_BG_CHR
	ld [hl+], a
	ldh a, [rSB]
	ld b, a
	jp fmt_SB


; @param HL: &dest
draw_collection:
	ld de, _collection
	ld c, COLLECTION_MAX_COUNT
.fmt_loop
	ld a, [de]
	inc de
	ld b, a
	ld a, [de]
	inc de
	call draw_coll_item

	call _get_collection_count
	ld b, a
	ld a, COLLECTION_MAX_COUNT
	sub b
	ld b, " "
	cp c
	jr nz, :+
	ld b, "<"
:
	ld a, b
	ld [hl+], a
	dec c
	jr nz, .fmt_loop
	ret


; @param A: code
; @param B: value
draw_coll_item:
	cp SERIO_DONE
	jr nz, :+
	ld a, "r"
	ld [hl+], a
	jp utile_print_h8
:
	cp SERIO_TIMEOUT
	jr nz, :+
	ld a, "T"
	ld [hl+], a
	ld a, "/"
	ld [hl+], a
	ld a, "O"
	ld [hl+], a
	ret
:
	ld [hl+], a
	jp utile_print_h8


flush_display:
	; flush status lines
	ld hl, STATLN_DEST
	ld de, _statln
	ld b, STATLN_LINE_LEN
	call bgmap_buffer_flush
	ld hl, STATLN_DEST + 32
	ld de, _statln + STATLN_LINE_LEN
	ld b, STATLN_LINE_LEN
	call bgmap_buffer_flush

	; flush collection
	call _get_collection_count
	assert COLLDISP_ITEM_WIDTH == 4
	add a
	add a
	ld b, a

	ld hl, COLLDISP_DEST
	ld de, _collection_display_buffer
	ld b, COLLECTION_MAX_COUNT * COLLDISP_ITEM_WIDTH
	ld c, COLLDISP_COLUMNS * COLLDISP_ITEM_WIDTH
.flush_loop
	; wait for vmem
	ldh a, [rSTAT]
	and STATF_BUSY
	jr nz, .flush_loop
	ld a, [de]
	ld [hl+], a
	inc de
	dec b
	ret z ; RET

	dec c
	jr nz, :+
	; jump down a row
	ld a, 32 - COLLDISP_COLUMNS * COLLDISP_ITEM_WIDTH
	add l
	ld l, a
	adc h
	sub l
	ld h, a
	ld c, COLLDISP_COLUMNS * COLLDISP_ITEM_WIDTH
:
	jr .flush_loop


; @param HL: dest
; @param DE: src
; @param B: length
; @mut: AF, BC, DE, HL
bgmap_buffer_flush:
:
	; wait for vmem
	ldh a, [rSTAT]
	and STATF_BUSY
	jr nz, :-
	ld a, [de]
	ld [hl+], a
	inc de
	dec b
	jr nz, :-
	ret
