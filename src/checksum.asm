
section "Checksum", rom0

; Calculate a 1 byte checksum from the values in a block of memory.
; The length of the block (value in C) is included in the sum.
; Check data integrity against a precalculated checksum by including
; the sum within the data block. If the result is zero, it's a match.
; The value of the checksum must be zero for the initial calculation.
; @param HL: &data
; @param C: len
; @return A: sum
; @mut: AF, C, HL
Checksum8::
	ld a, c
	and a
	ret z
	; include the length value (it's already in A)
:
	sub [hl]
	inc hl
	dec c
	jr nz, :-
	ret



if def(TESTS)

include "packet.inc"

section "wTestChecksumResults", wram0, align[8]
wTestResults: ds 8 * 256

wTestPacket:
	.check: db
	.data: ds PKT_DATA_SZ



section "test_checksum", rom0
; @param B: fill value
_init_packet:
	ld hl, wTestPacket
	xor a
	ld [hl+], a

	ld a, b
	ld c, PKT_DATA_SZ
:
	ld [hl+], a
	dec c
	jr nz, :-
	ret


; @param B: rand512 offset / 2
_init_packet_from_rand512:
	push de
	ld de, rand512
	ld a, b
	sla a
	add e
	ld e, a
	adc d
	sub e
	ld d, a

	ld hl, wTestPacket
	xor a
	ld [hl+], a

	ld c, PKT_DATA_SZ
:
	ld a, [de] :: inc de
	ld [hl+], a
	dec c
	jr nz, :-
	pop de
	ret


; ImplChecksumTest CHECKSUM, INIT_PACKET
macro ImplChecksumTest
	call \2
	; calc initial checksum, store in packet
	ld hl, wTestPacket
	ld c, PKT_SZ
	call \1
	ld [wTestPacket.check], a
	ld [de], a :: inc de
	; calc check checksum
	ld hl, wTestPacket
	ld c, PKT_SZ
	call \1
	ld [de], a :: inc de
endm


_run_gen:
	ld a, b :: ld [de], a :: inc de ; generation
	ImplChecksumTest Checksum8, _init_packet
	ImplChecksumTest Checksum8, _init_packet_from_rand512
	; padding
	xor a
rept 8 - (1 + 2 * 2)
	ld [de], a :: inc de
endr
	ret


rand512:
	db $E8, $EA, $20, $47, $58, $D0, $58, $7C, $CD, $75, $77, $BF, $75, $4F, $3E, $70
	db $56, $71, $9D, $5B, $A8, $C3, $01, $90, $CA, $BE, $43, $58, $5E, $C8, $49, $56
	db $20, $9D, $06, $4C, $AD, $C3, $D7, $0E, $DF, $83, $3C, $26, $B8, $BC, $A5, $70
	db $D1, $8E, $63, $1A, $E1, $2C, $10, $DC, $94, $E9, $0B, $E7, $1E, $CA, $5B, $0D
	db $D8, $0E, $75, $4A, $FA, $55, $35, $E5, $CD, $6C, $02, $1C, $39, $DE, $7A, $2E
	db $6E, $85, $5C, $D1, $C3, $D7, $E7, $C2, $B5, $F9, $4B, $F5, $DF, $24, $0F, $EA
	db $35, $05, $F9, $D0, $23, $30, $1B, $B2, $87, $42, $D0, $16, $E7, $0F, $14, $38
	db $AC, $BC, $6C, $8A, $E5, $8A, $E5, $A1, $B6, $73, $99, $B0, $F8, $E1, $06, $99
	db $2E, $BF, $40, $13, $AE, $63, $8B, $92, $86, $BE, $26, $CA, $79, $5E, $D8, $2E
	db $F5, $54, $6B, $3B, $8E, $86, $BA, $CF, $22, $16, $D6, $8F, $FD, $E7, $3A, $C9
	db $E6, $19, $B8, $A9, $28, $79, $DD, $15, $12, $95, $1C, $DA, $00, $22, $6A, $45
	db $06, $CA, $FA, $37, $4A, $8A, $E7, $D0, $B4, $CA, $01, $12, $EA, $76, $15, $E1
	db $6A, $E9, $C3, $17, $58, $2D, $BD, $B7, $7C, $10, $21, $E0, $F4, $F8, $63, $D5
	db $A3, $66, $DD, $1B, $D8, $24, $25, $06, $14, $1C, $A7, $BC, $77, $50, $97, $2A
	db $22, $82, $8A, $A4, $60, $9A, $31, $4B, $DC, $FD, $16, $BE, $89, $9A, $0D, $9C
	db $7E, $4F, $CF, $EA, $C3, $70, $C5, $51, $05, $98, $A9, $6D, $C5, $F1, $90, $B2
	db $12, $1D, $75, $51, $74, $FD, $4B, $63, $17, $9E, $7C, $2C, $20, $C6, $93, $E1
	db $2E, $51, $3A, $8E, $18, $74, $1E, $8A, $CD, $C8, $98, $DF, $74, $FB, $89, $35
	db $83, $EE, $9C, $89, $13, $16, $FE, $44, $AB, $FE, $50, $D7, $6C, $F7, $01, $5C
	db $A9, $D2, $AD, $6D, $C2, $89, $E1, $47, $BD, $3E, $5F, $27, $6F, $A2, $81, $2B
	db $61, $CF, $42, $A1, $78, $35, $99, $E8, $2E, $7D, $78, $B0, $68, $00, $5B, $5F
	db $5B, $80, $8F, $C8, $7C, $14, $A0, $DA, $D8, $BF, $7D, $09, $C4, $33, $5F, $28
	db $2F, $B3, $A9, $1C, $C5, $8F, $02, $30, $67, $51, $6B, $B4, $7C, $B1, $2B, $5E
	db $68, $66, $75, $C2, $5C, $A4, $47, $D4, $DE, $D1, $F8, $D1, $F6, $00, $1E, $01
	db $14, $09, $F0, $1D, $56, $08, $2C, $B3, $AE, $B1, $1D, $B5, $11, $8A, $C0, $75
	db $F9, $F6, $47, $A3, $75, $03, $81, $D2, $AC, $4D, $68, $41, $9A, $61, $F3, $E4
	db $EE, $38, $AE, $0E, $90, $69, $AB, $6D, $E0, $D5, $52, $DE, $ED, $DD, $20, $0C
	db $F7, $3D, $A4, $A8, $7A, $BC, $48, $8E, $7C, $78, $E3, $99, $90, $8D, $AE, $75
	db $9B, $68, $28, $F8, $1F, $2E, $20, $05, $AF, $BA, $69, $08, $F5, $2F, $E4, $CC
	db $DE, $13, $B0, $38, $67, $82, $97, $FD, $0E, $8A, $A5, $F2, $06, $D7, $26, $CC
	db $E9, $18, $86, $18, $38, $AB, $5D, $B9, $F5, $E3, $BE, $D6, $B8, $FD, $96, $98
	db $DB, $CE, $9B, $24, $B0, $F8, $C7, $9C, $AA, $89, $B9, $31, $AA, $80, $F9, $05


test_checksum::
	ld de, wTestResults
:
	call _run_gen
	ld a, b
	cp $FF
	ret z
	inc b
	jr nz, :-
	ret


section fragment "tests/runners", rom0
	dw test_checksum


endc ; TESTS

;
; vim:ft=rgbasm
