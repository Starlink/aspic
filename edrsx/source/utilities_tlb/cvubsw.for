        .TITLE CVUBSW
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;PURPOSE
;       Converts an unsigned byte value to a signed word value. To call
;       from fortran, use (for instance):
;
;               ISIGNED=CVUBSW(IUNSGN)
;
;SOURCE
;       CVUBSW.MAR in UTILITIES.TLB
;
;ARGUMENTS (in order of argument list)
;   INPUTS
;       Unsigned byte value
;   OUTPUT
;       None (but signed word value returned as function value)
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
        .ENTRY CVUBSW,^M<>
        MOVZBW @INVAL(AP),R0
        RET
        .END
