if !def(SERIAL_INC)
def SERIAL_INC equ 1


rsreset
def SERIOB_TXRX    rb 1 ; set if (last) job is transmitter
def SERIOB_WORKING rb 1 ; set if job ongoing / waiting for transfer (to complete)
def SERIOB_SYNC    rb 1 ; set when starting a transfer, not cleared
def SERIOB_ERROR   rb 1 ; set if job failed

def SERIOF_TX      equ 1 << SERIOB_TXRX
def SERIOF_WORKING equ 1 << SERIOB_WORKING
def SERIOF_SYNC    equ 1 << SERIOB_SYNC
def SERIOF_ERROR   equ 1 << SERIOB_ERROR
def SERIOF_IDLE    equ 0


endc ; SERIAL_INC
