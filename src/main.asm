
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
def COLLDISP_COLUMNS equ 4
def COLLDISP_ROWS equ COLLECTION_MAX_COUNT / COLLDISP_COLUMNS

def COLL_EMPTY equ "."
def COLL_RX equ "r"
def COLL_TX equ "t"
def COLL_ERR_OFFSET equ $20
def COLL_RXERR equ "R" ; RX = RXERR + $20
def COLL_TXERR equ "T" ; TX = TXERR + $20



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

	; press A: TX
	ldh a, [hKeysPressed]
	bit PADB_A, a
	call nz, do_tx

	; press B: RX
	ldh a, [hKeysPressed]
	bit PADB_B, a
	call nz, do_rx


	; simplest(?) sync method -- just check if successful transfer every frame
	call _serio_sync_poll
	; ; serio sync - only wait until the first interrupt
	; ld c, 4 ; max periods
	; call _serio_sync_limitcount
	; call _serio_sync_forced

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


; @mut: AF, DE, HL
_serio_sync_poll:
	ld hl, _serio_status
	bit SERIOB_SYNC, [hl]
	ret z ; nothing to wait for
	bit SERIOB_WORKING, [hl]
	ret nz ; still working
	res SERIOB_SYNC, [hl]
	call _collect_rx
	ret


; If there is a transfer pending, wait until it is complete, or until a timer expires.
; @param C: maximum number of `HALT`s
; @mut: AF, DE, C, HL
_serio_sync_limitcount:
; @TODO: use the timer interrupt to break out of halt
	ld hl, _serio_status
	bit SERIOB_SYNC, [hl]
	ret z
	jr .serio_sync
.serio_sync_halt:
	halt
	nop
	; @TODO: determine which interrupt broke HALT
.serio_sync:
	bit SERIOB_WORKING, [hl]
	jr z, .work_complete
	; simple counter timeout
	dec c
	jr nz, .serio_sync_halt
.work_complete
	call _collect_rx
.done
	res SERIOB_SYNC, [hl]
	ret


; If there is a transfer pending, wait until it is complete.
_serio_sync_forced:
; IF TRANSFER PENDING, WAIT UNTIL TRANSFER IS COMPLETE -- LOCKSTEP.
; YOU COULD USE THIS TO SYNC MAIN LOOP INSTEAD OF SYNCING TO VBLANK?
	ld hl, _serio_status
	bit SERIOB_SYNC, [hl]
	ret z
	jr .serio_sync
.serio_sync_halt:
	halt
	nop
.serio_sync:
	bit SERIOB_WORKING, [hl]
	jr nz, .serio_sync_halt
	res SERIOB_SYNC, [hl]

	call _collect_rx
	ret


; @return A:
_get_collection_count:
	ld a, [_collection_count]
	cp COLLECTION_MAX_COUNT
	jr c, :+
	xor a
:
	ret


; grab received value and "collect" it
; @mut: AF, B, DE
_collect_rx:
	call _get_collection_count
	assert COLLITEM_SIZE == 2
	inc a
	ld [_collection_count], a
	dec a
	add a ; mul 2
	ld de, _collection
	add e
	ld e, a
	adc d
	sub e
	ld d, a

	ldh a, [_serio_status]
	bit SERIOB_TXRX, a
	jr z, :+
	ld b, COLL_TX
	ldh a, [_serio_tx]
	jr .write
:
	ld b, COLL_RX
	ldh a, [_serio_rx]
.write
	ld [de], a
	inc de

	ldh a, [_serio_status]
	bit SERIOB_ERROR, a
	jr z, :+
	ld a, b
	sub COLL_ERR_OFFSET
	ld b, a
:
	ld a, b
	ld [de], a
	inc de
	ret


do_tx:
	ld a, [_tick]
	and $7F
	ldh [_serio_tx], a
	call serio_tx
	ret


do_rx:
	call serio_rx
	ret


redraw:
	call draw_statln
	call draw_collection

	call flush_display
	ret


clear_statln:
	ld hl, _statln
	ld c, STATLN_LINE_COUNT * STATLN_LINE_LEN
	ld a, STATLN_BG_CHR
:
	ld [hl+], a
	dec c
	jr nz, :-
	ret


draw_statln:
	; serio
	ld hl, _statln
	ldh a, [_serio_status]
	ld b, a
	call fmt_serio_status

	; rSC, rSB
	ld hl, _statln + STATLN_LINE_LEN
	ldh a, [rSC]
	ld b, a
	call fmt_SC
	ld a, STATLN_BG_CHR
	ld [hl+], a
	ldh a, [rSB]
	ld b, a
	call fmt_SB

	ret


draw_collection:
	ld hl, _collection_display_buffer
	ld de, _collection
	ld c, COLLECTION_MAX_COUNT
.fmt_loop
	ld a, [de]
	inc de
	ld b, a
	ld a, [de]
	inc de
	ld [hl+], a
	call utile_print_h8

	; mark end
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
