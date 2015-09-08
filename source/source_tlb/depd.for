      CHARACTER*10 CLASS,PROG
      CHARACTER*80 RECORD
      INTEGER TRULEN
      EXTERNAL TRULEN
C
C   First decide which file contains the relevant information.
C
      CALL RDKEYC('CLASS',.FALSE.,1,CLASS,I,ISTAT)
      IF (ISTAT.NE.0) THEN
         CALL WRERR('CLERR')
         CALL EXIT
      END IF
C
C   Then try to OPEN it - exit on error.
C   Note that it is of type .HLP
C
      OPEN (UNIT=1,FILE=CLASS//'.HLP',ERR=900,STATUS='OLD')
C
C   Now find out which entry is to be deleted.
C
      CALL RDKEYC('ENTRY',.FALSE.,1,PROG,I,ISTAT)
      IF (ISTAT.NE.0) THEN
        CALL WRERR('PRERR')
         CALL EXIT
      END IF
C
C   The output is to be written to a new generation number
C   of the same file.
C
      OPEN (UNIT=2,FILE=CLASS//'.HLP',STATUS='NEW',CARRIAGECONTROL=
     :      'LIST')
C
C   Decide on the length of the search string, and add 1 to
C   ensure that a spce is included as the last character, to
C   prevent mismatches.
C
      L=TRULEN(PROG) + 1
C
C   First loop around simply copying records, until
C   the record sought is found.
C   Note that it should be of the form
C      "2 prog<sp>"
C
  100 CONTINUE
        READ (1,'(A)',END=800) RECORD
        IF (RECORD(1:1).NE.'2'.OR.
     :       (RECORD(3:L+2).NE.PROG(1:L))) THEN
            L1=TRULEN(RECORD)
            WRITE (2,'(A)') RECORD(1:L1)
            GO TO 100
         ELSE
C
C      Having found the first record simply read records until
C      the start of the next entry is found.
C      This will be signified by a line starting with a 2.
C
            READ (1,'(A)',END=800) RECORD
            DO WHILE (RECORD(1:1).NE.'2')
               READ (1,'(A)',END=800) RECORD
            END DO
C
C         Thereafter simply copy records until the end of file
C         is detected.
C         (ALL reading ends at end-of-file, whether the required
C          entry is found or not.)
C
            L1=TRULEN(RECORD)
            WRITE (2,'(A)') RECORD(1:L1)
  300       CONTINUE
               READ (1,'(A)',END=800) RECORD
               L1=TRULEN(RECORD)
               WRITE (2,'(A)') RECORD(1:L1)
               GO TO 300
  800       CONTINUE
         END IF
      CALL EXIT
  900 CONTINUE
         CALL WRUSER('Failed to find the HELP file',ISTAT)
         CALL EXIT
      END
