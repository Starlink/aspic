      SUBROUTINE INTERRUPT(NPCTRLC)
C+
C   INTERRUPT
C
C   Returns a pointer to the logical variable reporting the
C   state of control-C.
C
C   Given      /CTCAST/
C   CTRLC_      =.true. if control-C has been typed
C               =.false. otherwise
C
C   Returned   (arguments)
C   NPCTRLC     pointer to CTRLC_
C
C   Returned   /CTCAST/
C   CTRLC_      access to this variable is allowed by this call.
C
C   B.D.Kelly/ROE/28.9.1981
C-

      LOGICAL*2 CTRLC_
      COMMON/CTCAST/CTRLC_

      CALL ROECNTRLC(.TRUE.)
      NPCTRLC=%LOC(CTRLC_)

      END
