if !def(SERIAL_INC)
def SERIAL_INC equ 1


rsreset
; Not doing anything. Ready.
def SERIO_IDLE rb 1
; Transfer started/requested.
def SERIO_WORKING rb 1
; Transfer has completed
def SERIO_DONE rb 1
; Transfer timed out before completing
def SERIO_TIMEOUT rb 1


endc ; SERIAL_INC
