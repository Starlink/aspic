C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C                     *******************
C                     *                 *
C                     * Program   STRIP *
C                     *                 *
C                     *******************
C
C
C
C          CALLING SEQUENCE:-
C               *** Non-Starlink program   Only called from UNDOC ***
C
C
C          FUNCTION:-
C               It  removes  all  lines  starting  with  one  of  the  form
C-------------- C+++++++++  up  to  one  of the form C------- form a source



      CHARACTER*80 TEXT
      OPEN (UNIT=1,FILE='UNDOCIN',CARRIAGECONTROL='LIST',STATUS='OLD')
      OPEN (UNIT=2,FILE='UNDOCOUT',CARRIAGECONTROL='LIST',STATUS='NEW')
  100 CONTINUE
      READ (1,900,ERR=400,END=500) TEXT
      IPOS=INDEX(TEXT,'++++++')
      IF (IPOS.EQ.0) THEN
         WRITE (2,900) TEXT
         GO TO 100
      END IF
  200 CONTINUE
      READ (1,900,ERR=400,END=500) TEXT
      IPOS=INDEX(TEXT,'------')
      IF (IPOS.EQ.0) THEN
         GO TO 200
      END IF
  300 CONTINUE
      READ (1,900,ERR=400,END=500) TEXT
      WRITE (2,900) TEXT
      GO TO 300
  400  CONTINUE
      PRINT *,'ERROR READING INPUT SOURCE'
  500 CONTINUE
  900 FORMAT (A80)
      END
