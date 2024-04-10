
include "hw.inc"
include "acharmap.inc"
include "serial.inc"

def BGMAP equ $9800
def STATLN_DEST equ BGMAP
def STATLN_BG_CHR equ " "
def STATLN_LINE_LEN equ 20
def STATLN_LINE_COUNT equ 5
def COLLDISP_DEST equ BGMAP + (STATLN_LINE_COUNT + 1) * 32
def COLLDISP_ITEM_WIDTH equ 4
def COLLDISP_COLUMNS equ 2

def COLLITEM_SIZE equ 2
def COLLECTION_MAX_COUNT equ 16


section "wMainDisplay", wram0
_statln:
for I, STATLN_LINE_COUNT
	.line{u:I}: ds STATLN_LINE_LEN
endr
_collection_display_buffer: ds COLLECTION_MAX_COUNT * COLLDISP_ITEM_WIDTH

_collection:: ds COLLECTION_MAX_COUNT * COLLITEM_SIZE
_collection_count:: db


section "MainDisplay", rom0

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
collection_append::
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

	ld bc, _statln.line1
	ld d, STATLN_BG_CHR
	call _clear_to
	call draw_hshk

	ld bc, _statln.line2
	ld d, STATLN_BG_CHR
	call _clear_to
	call draw_packet_sys

	ld bc, _statln.line3
	ld d, STATLN_BG_CHR
	call _clear_to
	ld a, "^tx^" :: ld [hl+], a
	ld c, PKT_DATA_SZ
	ld de, _packet_tx
	call draw_packet_buffer

	ld bc, _statln.line4
	ld d, STATLN_BG_CHR
	call _clear_to
	ld a, "^rx^" :: ld [hl+], a
	ld c, PKT_DATA_SZ
	ld de, _packet_rx
	call draw_packet_buffer

;	ld bc, _statln.line4
;	ld d, STATLN_BG_CHR
;	call _clear_to
;	call draw_statln_sc_sb

	; ld hl, _collection_display_buffer
	; call draw_collection

	call flush_statln
	; call flush_collection
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


; @param HL: &dest
draw_statln_serio:
	ldh a, [hSerStatus]
	call fmt_serio_status
	ldh a, [hSerStatus]
	bit SIOSTB_ENABLE, a
	ret z

	ld a, STATLN_BG_CHR
	ld [hl+], a

	ldh a, [hTimeoutTicks]
	and a
	jr nz, :+
	ld a, [wSerTransferTimeout]
:
	ld b, a
	call utile_print_h8
	ld a, STATLN_BG_CHR
	ld [hl+], a

	ldh a, [hSerTx]
	ld b, a
	ldh a, [hSerRx]
	ld c, a
	jp fmt_txrx


; @param D: value
; @param HL: &dest
; @param BC: &end
_clear_to:
.loop
	ld a, h
	cp b
	jr c, .continue
	ld a, l
	cp c
	ret nc
.continue
	ld a, d
	ld [hl+], a
	jr .loop


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
draw_hshk:
	ld a, [wHshkState]
	call fmt_hshk_status
	ld a, [wHshkCount]
	call fmt_hshk_count
	ret


; @param HL: &dest
draw_packet_sys:
	ld a, [wPacketCheckLoc]
	call fmt_packet_check
	ld a, ":" :: ld [hl+], a
	ld a, [wPacketCheckRem]
	call fmt_packet_check

	ld a, " " :: ld [hl+], a
	ld a, [wPacketStateLoc]
	call fmt_packet_state
	ld a, ":" :: ld [hl+], a
	ld a, [wPacketStateRem]
	call fmt_packet_state

	ret


; @param C: data len
; @param DE: &packet
; @param HL: &dest
draw_packet_buffer:
	ld a, [de]
	inc de
	ld b, a
	call utile_print_h8
	ld a, " " :: ld [hl+], a
:
	ld a, [de]
	inc de
	ld b, a
	call utile_print_h8

	dec c
	jr nz, :-
	ret


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
	cp SIOSTF_XFER_DONE
	jr nz, :+
	ld a, "r"
	ld [hl+], a
	jp utile_print_h8
:
	cp SIOSTF_XFER_TIMEOUT
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


flush_statln:
	ld hl, STATLN_DEST
	ld de, _statln
	ld b, STATLN_LINE_LEN
	call bgmap_buffer_flush
	ld hl, STATLN_DEST + 32 * 1
	ld de, _statln + STATLN_LINE_LEN * 1
	ld b, STATLN_LINE_LEN
	call bgmap_buffer_flush
	ld hl, STATLN_DEST + 32 * 2
	ld de, _statln + STATLN_LINE_LEN * 2
	ld b, STATLN_LINE_LEN
	call bgmap_buffer_flush
	ld hl, STATLN_DEST + 32 * 3
	ld de, _statln + STATLN_LINE_LEN * 3
	ld b, STATLN_LINE_LEN
	call bgmap_buffer_flush
	ld hl, STATLN_DEST + 32 * 4
	ld de, _statln + STATLN_LINE_LEN * 4
	ld b, STATLN_LINE_LEN
	call bgmap_buffer_flush
	ret


flush_collection:
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
