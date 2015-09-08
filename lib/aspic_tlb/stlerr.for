      SUBROUTINE STLERR(TEXT,STATUS)
C++
C     STLERR - Starlink Error handler
C
C     This is called by any Starlink routine which sets STATUS
C     non-zero.
C
C     This version will return an appropiate message to the user
C     if the status value is greater than 2 (warning level).
C
C     CALL STLERR(TEXT,STATUS)
C
C     Input arguments:
C     ---------------
C     TEXT:    CHARACTER expression:	Starlink routine name
C     STATUS:  INTEGER expression:	Status value
C
C
C     D.PEARCE  24/JUL/80  VERSION #2
C
C	MODIFIED W.LUPTON RGO OCT 1981 (SUPPRESS MESSAGES FOR DESCRIPTOR
C	ROUTINES WHERE WRITE ACCESS IS DENIED)
C--
C
      IMPLICIT      INTEGER(A-Z)
C
      CHARACTER*(*) TEXT
      INTEGER*4     STATUS
C
      CHARACTER*10  MSG
      DATA MSG/' STATUS=**'/
C
      IF (TEXT(3:6).NE.'DSCR'.AND.STATUS.GT.2) THEN
         CALL ITOC(STATUS,MSG(9:10),IND)
         CALL WRUSER('0---'//TEXT//MSG,IND)
      ENDIF
C
      RETURN
      END
