      SUBROUTINE ARGS_RUNPROG(ENTRY)
*+
*  A R G S _ R U N P R O G
*
*  Starts execution of a program loaded in the ARGS graphics store.
*
*  GIVEN:
*         ENTRY (I)  Address in graphics store at which execution is to
*                    start.
*
*  DLT Starlink MAN  SEPT 81
*-
      INTEGER ENTRY

      CALL ARGS_FLUSH(2)

*  Load RUN instruction
      CALL ARGS_PUT1('1600'X)
      CALL ARGS_PUT1(ENTRY)

      CALL SRSEND

      RETURN
      END

