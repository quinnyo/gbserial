if !def(PACKET_INC)
def PACKET_INC equ 1


def NAMES_ARE_HARD equ 1


def PKT_CHECK_SZ equ 1
def PKT_DATA_SZ equ 8
def PKT_SZ equ PKT_DATA_SZ + PKT_CHECK_SZ


; PACKET PEER CONTROL MESSAGES

def PKT_CHECK_OK   equ $11
def PKT_CHECK_FAIL equ $22



; PACKET PEER STATE MACHINE STATES

def PKST_MAJ_PREP     equ $00
def _PKST_NULL         equ $00
; Packet under construction or otherwise not ready to transfer packet.
def _PKST_PREP         equ $01
def _PKST_STOPPED      equ $0F

def PKST_MAJ_READY    equ $10
; Ready to send. Packet data is buffered and the packet header has been computed.
def _PKST_READY        equ $10

def PKST_MAJ_XFER     equ $20
; Transfer packet bytes.
def _PKST_XFER         equ $20
; Check for errors in received data before going to appropriate result state.
def _PKST_CHECK        equ $21

def PKST_MAJ_RESULT   equ $40
; Transfer completed without error.
def _PKST_OK           equ $40
; Some error occurred during the transfer.
def _PKST_ERROR        equ $41

def PKST_MAJOR equ $F0
def PKST_MINOR equ $0F

; do not write to tx packet buffer if pkst & this mask
def PKST_TX_LOCKED equ PKST_MAJOR

def PKSTB_MAJ_READY equ 4
def PKSTB_MAJ_XFER equ 5
def PKSTB_MAJ_RESULT equ 6
def PKSTB_ERROR equ 0
def PKSTB_ERROR_TX equ 1
def PKSTB_ERROR_RX equ 2


endc ; PACKET_INC

