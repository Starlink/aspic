        .TITLE  EXISTS
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;PURPOSE
;       This procedure checks to see if a file EXISTS which matches the
;       given file specification
;
;SOURCE
;       EXISTS.MAR in UTILITIES.TLB
;
;ARGUMENTS
;   INPUTS:
;       first argument  Address of a character string descriptor holding
;                       name of file
;   OUTPUTS:
;       second argument Address of longword holding status return value
;                       0 if file EXISTS, 1 otherwise.
;
;AUTHOR
;       D.S. Berry (MAVAD::DSB) 28/3/88
;-----------------------------------------------------------------------
;

;   *** DATA ***

        $RMSDEF                         ; Define all RMS symbols
        .PSECT  DATA,WRT,NOEXE
NAM_BLK:
        $NAM    ESA=EXP_STR,-           ; Expanded buffer address
                ESS=NAM$C_MAXRSS        ; Expanded buffer size
FAB_BLK:
        $FAB    FOP=NAM,-               ; Use NAM block option
                NAM=NAM_BLK             ; Pointer to NAM block
EXP_STR:
        .BLKB   NAM$C_MAXRSS            ; Expanded string buffer

        DESC=4                          ; Set up symbols pointing to
        ISTAT=8                         ; arguments


;  *** CODE ***

        .PSECT  CODE,EXE,NOWRT
        .ENTRY  EXISTS,^M<R2,R3>
        MOVQ    @DESC(AP),R2            ; Get file name descriptor in R2
                                        ; and R3
        MOVL    R3,FAB_BLK+FAB$L_FNA    ; Move file name address to FAB
        CVTLW   R2,FAB_BLK+FAB$B_FNS    ; Move file name size to FAB
        $PARSE  FAB=FAB_BLK             ; Expand file specification
        BLBC    R0,ERROR                ; Jump if error occured
        $SEARCH FAB=FAB_BLK             ; Search for expanded file
        BLBC    R0,ERROR                ; Jump if ANY error occured
        MOVL    #0,@ISTAT(AP)           ; If not, set ISTAT to 0
        RET                             ; and return
ERROR:
        CMPL    R0,#RMS$_FNF            ; If error is RMS$_FNF
        BEQL    EXIT                    ; jump to exit
        CMPL    R0,#RMS$_DNF            ; If error is RMS$_DNF
        BEQL    EXIT                    ; jump to exit
        CMPL    R0,#RMS$_SYN            ; If error is RMS$_SYN
        BEQL    EXIT                    ; jump to exit
        CMPL    R0,#RMS$_NMF            ; If error is RMS$_NMF
        BEQL    EXIT                    ; jump to exit
        CMPL    R0,#RMS$_DNR            ; If error is RMS$_DNR
        BEQL    EXIT                    ; jump to exit
        CMPL    R0,#RMS$_DEV            ; If error is RMS$_DEV
        BEQL    EXIT                    ; jump to exit
        CMPL    R0,#RMS$_NET            ; If error is RMS$_NET
        BEQL    EXIT                    ; jump to exit
        PUSHL   FAB_BLK+FAB$L_STV       ; Push STV and STS on stack
        PUSHL   FAB_BLK+FAB$L_STS       ; in reverse order
        CALLS   #2,G^LIB$SIGNAL         ; Signal error
EXIT:
        MOVL    #1,R0                   ; Clear error status
        MOVL    #1,@ISTAT(AP)           ; Set ISTAT to 1, file not found
        RET                             ; Return
        .END
