      SUBROUTINE STOPIT(ISTAT)
C+
C      THIS A BASIC ERROR HANDLING FOR FATAL ERRORS
C
C      IF ISTAT IS EQUAL TO "ERR_NORMAL" THEN NOTHING HAPPENS
C      IF ISTAT IS EQUAL TO "ERR_FMTCON" NOTHING MUCH HAPPENS
C      OTHERWISE A MESSAGE IS OUTPUT TO THE USER , ALL FRAMES FREED
C      AND THE PROGRAM STOPS.
C                        *
C                       ***
C                      *****
C-      **********************
C
C
C      VERSION #2
C
C      WRITTEN BY K F HARTLEY AT RGO ON 5/5/81
C
      INCLUDE 'INTERIM(ERRPAR)'
C
      IF (ISTAT.EQ.ERR_NORMAL) THEN
         GO TO 200
      END IF
      IF (ISTAT.EQ.1) THEN
         CALL WRUSER('NULL RESPONSE - NOT ALLOWED',IST)
         GO TO 100
      END IF
      IF (ISTAT.EQ.ERR_FMTCON) THEN
         CALL WRUSER('FORMAT CONVERSION HAS TAKEN PLACE',IST)
         GO TO 200
      END IF
      IF (ISTAT.EQ.ERR_FRMNAC) THEN
         CALL WRUSER('DATA COULD NOT BE ACCESSED',IST)
         GO TO 100
      END IF
      IF (ISTAT.EQ.ERR_PARINV) THEN
         CALL WRUSER('INVALID PARAMETER NAME',IST)
         GO TO 100
      END IF
      IF (ISTAT.EQ.ERR_FMTINV) THEN
         CALL WRUSER('INVALID FORMAT CODE',IST)
         GO TO 100
      END IF
      IF (ISTAT.EQ.ERR_DIMINV) THEN
         CALL WRUSER('DIMENSIONS OUT OF RANGE',IST)
         GO TO 100
      END IF
      IF (ISTAT.EQ.ERR_FRMINV) THEN
         CALL WRUSER('FRAME NOT OF TYPE "IMAGE"',IST)
         GO TO 100
      END IF
      IF (ISTAT.EQ.ERR_FMTBAD) THEN
         CALL WRUSER('FORMAT COVERSION FAILED',IST)
         GO TO 100
      END IF
C
C     ALL "BAD" ERRORS COME TO HERE
C
  100 CONTINUE
      CALL FRDATA(' ',IST)
      CALL WRUSER('EXITING FROM PROGRAM',IST)
      CALL EXIT
C
C      GOOD STATUS VALUES (IE ERR_NORMAL AND ERR_FMTCON) COME TO HERE
C
  200 CONTINUE
      RETURN
      END
