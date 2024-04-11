
if def(TESTS)

section "test_dummyplug", rom0
test_dummyplug:
	ret


section fragment "tests/runners", rom0
	dw test_dummyplug


section "tests", rom0
Tests_EntryPoint::
	ld hl, startof("tests/runners")
.loop_runners:
	push hl
	ld a, [hl+]
	ld h, [hl]
	ld l, a
	ld de, :+
	push de
	jp hl
:
	pop hl
	inc hl :: inc hl
	ld a, h
	cp high(startof("tests/runners") + sizeof("tests/runners"))
	jr nz, .loop_runners
	ld a, l
	cp low(startof("tests/runners") + sizeof("tests/runners"))
	jr nz, .loop_runners

.end:
	di
	ld b, b
	halt
	nop
	jr .end


endc ; TESTS

;
; vim:ft=rgbasm
