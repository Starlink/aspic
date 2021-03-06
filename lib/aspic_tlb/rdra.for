        SUBROUTINE RDRA(CHARS,J,K,RA,IERR)

C
C      READ IN A RIGHT ASCENSION VALUE
C      INPUT PARAMETERS:-
C      CHARS - STRING HOLDING STRING REPRESENTING RIGHT ASCENSION
C      J,K   - 1ST & LAST CHARACTERS IN THE STRING DELIMITING
C              THE RIGHT ASCENSION CHARACTERS
C      OUTPUT PARAMETERS:-
C      RA     - DOUBLE PRECISION VALUE OF THE RIGHT ASCENSION
C      IERR  - VALUE DENOTING ERROR.AS RETURNED FROM READPOS
C
C
      DOUBLE PRECISION TWOPI,HALFPI,RDSA,RDST,RDDG
      COMMON/CONVF/TWOPI,HALFPI,RDSA,RDST,RDDG
      CHARACTER*40 STRING
      CHARACTER*(*) CHARS
      DOUBLE PRECISION RA,ANGLE
      INTEGER IERR
      STRING = CHARS(J:K)
      CALL READPOS(STRING,ANGLE,IERR)
      IF (IERR.EQ.1) THEN
        WRITE (6,*) ' Format Error'
        RETURN
      ENDIF
      IF (IERR.EQ.2) THEN
        WRITE (6,*) ' Error - RA is in Hours,Mins. and Secs'
        RETURN
      ENDIF
      RA = ANGLE * 3.6D3 * RDST
      IF (RA.LT.0.0.OR.RA.GT.TWOPI) THEN
        WRITE (6,*) '  Error - RA must be between 0 and 24 Hours'
        IERR = 3
        RETURN
      ENDIF
      RETURN
      END
 
