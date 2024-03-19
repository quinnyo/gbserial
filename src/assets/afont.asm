
/*
src image layout == intended memory layout / charmap:
	00:0123456789ABCDEF
	10:GHIJKLMNOPQRSTUV
	20:WXYZ[\]^_`abcdef
	30:ghijklmnopqrstuv
	40:wxyz{|}~:;<=>?@!
	50:"#$%&'()*+,-./
*/

section "assets/afont.chr", rom0
assets_chr_afont::
	incbin "assets/afont.chr"

section "load_afont", rom0
; assumes LCD is off
load_afont::
	ld de, startof("assets/afont.chr")
	ld bc, sizeof("assets/afont.chr")
	ld hl, $9000
	jp memcpy
