      SUBROUTINE SWEEPS
*+
*   SWEEPS.
*
*   Rotate ARGS colour table continuously until CONTROL-C typed.
*
*  Given;
*   None.
*
*  Returned;
*   None.
*
*  Subroutines called;
*   E2D:-   INTERRUPT, SWEEP1.
*   Args:-  SRINIT.
*
*   B.D.Kelly/ROE/18.12.1981
*   A C Davenhall./ROE/                               26/10/82.
*-
      INTEGER NPCTRLC,IFAIL
*
*
      CALL SRINIT (0,.FALSE.,IFAIL)
*
      CALL INTERRUPT(NPCTRLC)
*
      CALL SWEEP1(%VAL(NPCTRLC))
*
      END
