
include "hw.inc"

section "irq/vblank", rom0[$40]
irq_vblank:
	jp vblank_handler


section "hVBlank", hram
hVBlankDone:: db

hLCDC:: db
hSCY:: db
hSCX:: db


section "vblank", rom0

vblank_init::
	xor a
	ld hl, hVBlankDone
	ld [hl+], a
	ldh a, [rLCDC]
	ld [hl+], a
	xor a
	ld [hl+], a
	ld [hl+], a

	call obuf_init

	ld hl, rIE
	set IEB_VBLANK, [hl]
	ret


vblank_handler:
	push af
	push hl

	ld hl, $FF40 ; LCDC
	ldh a, [hLCDC]
	ld [hl+], a
	inc hl      ; $FF41: rSTAT (skip)
	ldh a, [hSCY]
	ld [hl+], a ; $FF42: rSCY
	ldh a, [hSCX]
	ld [hl+], a ; $FF43: rSCX

	call hOAMCopyRoutine

	ld a, 1
	ldh [hVBlankDone], a

	pop hl
	pop af
	reti


def OBUF_SIZE equ OAM_COUNT * sizeof_OAM_ATTRS

section "wObuf", wram0, align[8]
wObuf:: ds OBUF_SIZE
wObufCount:: db


section "obuf", rom0

obuf_init::
	call obuf_clear
	ld hl, hOAMCopyRoutine
	ld de, startof("obuf_dma_load")
	ld c, sizeof("obuf_dma_load")
.copyOAMRoutineLoop
	ld a, [de]
	inc de
	ld [hl+], a
	dec c
	jr nz, .copyOAMRoutineLoop
	; We directly copy to clear the initial OAM memory, which else contains garbage.
	call hOAMCopyRoutine
	ret


obuf_clear::
	ld c, OBUF_SIZE
	ld hl, wObuf
	ld a, $FF
:
	ld [hl+], a
	dec c
	jr nz, :-
	xor a
	ld [hl], a ; wObufCount
	ret


; @return HL: address of next slot in obuf
; @mut: AF, HL
obuf_next::
assert high(wObuf) == high(wObufCount)
	ld hl, wObufCount
	ld a, [hl]
	cp OAM_COUNT
	jr c, :+
	xor a
	ld [hl], a
:
	inc [hl]
	add a :: add a
	ld l, a ; H is already high(wObuf)
	ret


pushs
section "obuf_dma_load", rom0
; _obuf_dma:
load "ObufDMA", hram
; Copy buffered data to OAM (DMA)
hOAMCopyRoutine::
	ld a, high(wObuf)
	ldh [rDMA], a
	ld a, OAM_COUNT
.wait
	dec a
	jr nz, .wait
	ret
; .end
endl
pops

;
; vim:ft=rgbasm
