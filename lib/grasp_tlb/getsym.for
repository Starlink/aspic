C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      **************
C      *            *
C      * S/R GETSYM *
C      *            *
C      **************
C
C
C   PURPOSE
C      Looks at DCL symbol (of which the Starlink parameters are
C      examples) and gets it value. If not present, or blank, or
C      not as a number, output value is set to 0.0 and the error
C      flag set.
C
C   ARGUMENTS
C  IN
C    SYMB     Character*(*)     The Symbol to be looked at
C  OUT
C    OUTVAL   Real              The value in that Symbol
C    KSTAT    Integer           Error flag.  0=success;1=no such
C                               symbol;2=blanks in Symbol;3=contents
C                               not a number.
C
C   STARLINK PARAMETERS
C          "SYMB"
C
C
C   CALLS
C     Starlink
C       LIB$GET_SYMBOL,SS$_NORMAL
C     Grasp
C       CHARLN
C     Aspic
C       CTOR
C
C
C
C   A.J.PENNY                   RGO                    83-3-17
C
C -----------------------------------------------------------------
C
C
C
      SUBROUTINE GETSYM(SYMB,OUTVAL,KSTAT)
C
C
C
      CHARACTER*(*) SYMB
      CHARACTER*72 VALUE
      INTEGER SS$_NORMAL
C
C
C
      ISTAT = LIB$GET_SYMBOL(SYMB,VALUE)
      IF (.NOT.ISTAT) THEN
         KSTAT = 1
         OUTVAL = 0.0
      ELSE
         KSTAT= 0
         CALL CHARLN(VALUE,LEN)
         IF (LEN.EQ.0) THEN
            KSTAT = 2
            OUTVAL = 0.0
         ELSE
            CALL CTOR(VALUE,OUTVAL,JSTAT)
            IF (JSTAT.NE.0) THEN
               KSTAT = 3
               OUTVAL = 0.0
            ENDIF
         ENDIF
      ENDIF
C
C
C
      END



