if !def(SERIAL_INC)
def SERIAL_INC equ 1

include "hw.inc"

def SERIO_TIMEOUT_TICKS_DEFAULT equ 8

/* SERIO CONFIG */

def SERIO_CFGB_ROLE    equ SCB_SOURCE ; Set for host (INTCLK)
def SERIO_CFGF_HOST    equ 1 << SERIO_CFGB_ROLE
def SERIO_CFGF_DEFAULT equ 0

/* SERIO STATUS */

def SIOSTB_ENABLE       equ 0
def SIOSTB_QUEUE        equ 2
def SIOSTB_XFER_INFO0   equ 4
def SIOSTB_XFER_ACTIVE  equ 6
def SIOSTB_XFER_ENDED   equ 7

def SIOSTF_DEFAULT    equ 0

def SIOSTF_ENABLED    equ 1 << SIOSTB_ENABLE
def SIOSTF_QUEUE      equ 1 << SIOSTB_QUEUE

def SIOSTF_XFER_ACTIVE  equ 1 << SIOSTB_XFER_ACTIVE
def SIOSTF_XFER_ENDED   equ 1 << SIOSTB_XFER_ENDED

def SIOSTF_XFER_STATE   equ %1100_0000 ; Mask SIOST xfer state bits
def SIOSTF_XFER_INFO    equ %0001_0000 ; Mask SIOST xfer info bits
def SIOSTF_XFER_STATUS  equ (SIOSTF_XFER_STATE | SIOSTF_XFER_INFO)

def SIOSTF_XFER_IDLE      equ 0
def SIOSTF_XFER_DONE      equ SIOSTF_XFER_ENDED ; transfer ended normally
def SIOSTF_XFER_TIMEOUT   equ %1001_0000 ; transfer aborted after timed out


; F.Z: set if using external clock
; @mut: AF
macro IsExternalClock
	ld a, [wSerConfig]
	bit SERIO_CFGB_ROLE, a
endm

; SetTransferStatus XFER_STATUS
; Set xfer state and info
; @mut: AF
macro SetTransferStatus
	ldh a, [hSerStatus]
	and $FF ^ SIOSTF_XFER_STATUS
	or (\1)
	ldh [hSerStatus], a
endm

; GetTransferStatus
; load current transfer status (state + info) into A
; @mut: AF
macro GetTransferStatus
	ldh a, [hSerStatus]
	and SIOSTF_XFER_STATUS
endm

; F.Z: set if transfer is active
; @mut: AF
macro IsTransferActive
	ldh a, [hSerStatus]
	and $FF ^ SIOSTF_QUEUE
	cp SIOSTF_XFER_ACTIVE | SIOSTF_ENABLED
endm

endc ; SERIAL_INC
