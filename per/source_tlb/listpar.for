      PROGRAM LISTPAR
C+
C
C   Program LISTPAR
C
C   This program reads a parameter file and lists the contents
C   to the terminal.
C
C   Written by K.F.Hartley at RGO on 1-Mar-1984
C
C-
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'INTERIM(FMTPAR)'
      INCLUDE 'INTERIM(ERRPAR)'
      INTEGER AXF(2),PP,STATUS
      DOUBLE PRECISION FKNOWN(20),C(21),PHI(20),PARMS(3)
      DOUBLE PRECISION TZERO
      CHARACTER*72 TEXT
C
C   Pick up set of parameters
C
      CALL RDIMAG('PARIN',FMT_DP,2,AXF,I,PP,ISTAT)
      IF(ISTAT.NE.ERR_NORMAL)  THEN
         CALL WRERR('HELLPAR',STATUS)
      ELSE
         IFREQ=AXF(2)
         CALL PER_PUTFRE3(%VAL(PP),AXF,FKNOWN,C,PHI)
C
C      Now pick up the mean value
C
         CALL RDDSCR('PARIN','MEAN',1,TEXT,I,STATUS)
         IF (STATUS.EQ.ERR_NORMAL) THEN
            CALL CTOR(TEXT,TEMP,STATUS)
            C(1) = TEMP
         ELSE
            CALL WRUSER('No mean value found - zero assumed',STATUS)
            C(1) = 0.0
         END IF
C
C      and the epoch to which the phases refer
C
         CALL RDDSCR('PARIN','EPOCH',1,TEXT,I,STATUS)
         IF (STATUS.EQ.ERR_NORMAL) THEN
            CALL CTOR(TEXT,TZERO,STATUS)
         ELSE
            CALL WRUSER('No epoch found - zero assumed',STATUS)
            TZERO=0.0
         END IF
C
C      Finally write the results to the terminal
C
         WRITE (TEXT,910)
  910    FORMAT (14X,'Frequency',6X,'Amplitude',10X,'Phase')
         CALL WRUSER(' ',STATUS)
         CALL WRUSER(TEXT,STATUS)
         CALL WRUSER(' ',STATUS)
         DO I=1,IFREQ
            WRITE (TEXT,920) FKNOWN(I),C(I+1),PHI(I)
  920       FORMAT (11X,3(F12.5,3X))
            CALL WRUSER(TEXT,STATUS)
         END DO
         CALL WRUSER(' ',STATUS)
         WRITE (TEXT,930) TZERO
  930    FORMAT (11X,'The phases are referred to epoch ',F12.5)
         CALL WRUSER(TEXT,STATUS)
         WRITE (TEXT,940) C(1)
  940    FORMAT(11X,'The mean value found is ',F12.5)
         CALL WRUSER(TEXT,STATUS)
         CALL WRUSER(' ',STATUS)
      END IF
C
C   Tidy up and exit
C
      CALL FRDATA(' ',ISTAT)
      CALL EXIT
      END
