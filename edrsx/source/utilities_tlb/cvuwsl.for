        .TITLE CVUWSL
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;PURPOSE
;       Converts an unsigned word value to a signed longword value. To
;       call from fortran, use (for instance):
;
;               ISIGNED=CVUWSL(IUNSGN)
;
;SOURCE
;       CVUWSL.MAR in UTILITIES.TLB
;
;ARGUMENTS (in order of argument list)
;   INPUTS
;       Unsigned word value
;   OUTPUT
;       None (but signed longword value returned as function value)
;
;USED BY
;       EDRSIN
;
;AUTHOR
;       D.S. Berry  (MAVAD::DSB) 26/8/87
;       (based on format conversion routine in INTERIM library)
;---------------------------------------------------------------
;
        INVAL=4
        .ENTRY CVUWSL,^M<>
        MOVZWL @INVAL(AP),R0
        RET
        .END
