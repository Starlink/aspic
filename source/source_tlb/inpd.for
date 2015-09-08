      CHARACTER*9 CLASS,PROG
      CHARACTER*80 RECORD,NEWREC
      CHARACTER*77 STRING
      LOGICAL GREATER,DONE
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
C   Now find out which entry is to be inserted.
C
      CALL RDKEYC('ENTRY',.FALSE.,1,PROG,I,ISTAT)
      IF (ISTAT.NE.0) THEN
        CALL WRERR('PRERR')
         CALL EXIT
      END IF
      L=TRULEN(PROG)
C
C   Now open the file containing the new iformation.
C
      OPEN (UNIT=3,FILE=PROG//'.DOC',STATUS='OLD',ERR=910)
C
C   The output is to be written to a new generation number
C   of the same file.
C
      OPEN (UNIT=2,FILE=CLASS//'.HLP',STATUS='NEW',CARRIAGECONTROL=
     :      'LIST')
C
C   First loop around simply copying records, until
C   a record starting with a 2 is found.
C
      DONE=.FALSE.
  100 CONTINUE
        READ (1,'(A)',END=800) RECORD
        IF (RECORD(1:1).NE.'2') THEN
            L1=TRULEN(RECORD)
            WRITE (2,'(A)') RECORD(1:L1)
            GO TO 100
         ELSE
C
C      Having found an interesting record, check if the
C      new information should go after it.
C
            STRING=RECORD(3:L1)
            GREATER=LGE(PROG(1:L),STRING(1:L))
            IF (GREATER) THEN
               L1=TRULEN(RECORD)
               WRITE (2,'(A)') RECORD(1:L1)
               GO TO 100
            ELSE
  150          CONTINUE
                  READ (3,'(A)',END=160) NEWREC
                  L1=TRULEN(NEWREC)
                  WRITE (2,'(A)') NEWREC(1:L1)
                  GO TO 150
  160          CONTINUE
               DONE=.TRUE.
            END IF
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
            IF (.NOT.DONE) THEN
  810          CONTINUE
               READ (3,'(A)',END=820) NEWREC
               L1=TRULEN(NEWREC)
               WRITE (2,'(A)') NEWREC(1:L1)
               GO TO 810
  820          CONTINUE
            END IF
         END IF
      CALL EXIT
  900 CONTINUE
         CALL WRUSER('Failed to find the HELP file',ISTAT)
         CALL EXIT
  910 CONTINUE
         CALL WRUSER('Failed to find documentation file',ISTAT)
         CALL EXIT
      END
