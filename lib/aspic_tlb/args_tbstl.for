
      SUBROUTINE ARGS_TBSTL(NCUR)
*     --------------------------

*+
*
*  ARGS_TBSTL
*
*
*  LOAD TRACKERBALL/CURSOR STATUS READING ROUTINE INTO ARGS
*
*
*  GIVEN:
*
*     NCUR     CURSOR NUMBER (1-4=USER, ELSE=SYSTEM)
*
*
*-

      INTEGER NCUR


*   (IGNORE CURSOR NUMBER .... ONLY SYSTEM CURSOR AVAILABLE)

*   LOAD ARGS PROGRAM
      CALL LOAD_TBST

      END
