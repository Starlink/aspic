      PROGRAM RESTORE
C+
C      Program RESTORE
C
C      This program takes a .BDF file, of the type used by
C      the period finding programs, and writes it as a formatted
C      .DAT file, suitable for editing or printing.
C
C          The input file must be a .BDF file
C          The output file is, by default, of type .DAT
C
C      Written by K.F.Hartley at RGO on 24-Jan-1984
C      (Based on an original by C.D.Pike at RGO)
C
C-
      INCLUDE 'INTERIM(ERRPAR)'
      INCLUDE 'INTERIM(FMTPAR)'
      INTEGER AX(2),PIN,STATUS
      CHARACTER*50 FNAME
C
C   First obtain the input file name
C
  100 CONTINUE
      CALL RDIMAG('INPUT',FMT_DP,2,AX,I,PIN,STATUS)
      IF (STATUS.NE.ERR_NORMAL.OR.I.NE.2) THEN
         CALL WRERR('HELLIN',ISTAT)
         CALL CNPAR('INPUT',STATUS)
         GO TO 100
      END IF
C
C   Then the name of the output file
C
      FNAME='OUTPUT'
      CALL RDKEYC('FNAME',.TRUE.,1,FNAME,I,STATUS)
      OPEN (UNIT=99,TYPE='NEW',NAME=FNAME)
C
C   Now write the output
C
      IDEV=99
      CALL PER_WRITE(%VAL(PIN),AX(1),AX(2),IDEV)
C
C   Tidy up and exit
C
      CLOSE (UNIT=IDEV)
      CALL FRDATA(' ',STATUS)
      CALL EXIT
      END
