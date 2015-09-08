      PROGRAM LOCDOC
C+
C
C      Program LOCDOC
C
C      Reads the file LOCAL.HLP and
C         (a) Copies the preamble to a new generation number.
C         (b) Copies the help on each subject into a file of type
C             .EXT (one file for each topic).
C         (c) Creates a file LOCAL.LIS with the names of the .EXT
C             files which have been created.
C
C      It should be used with the procedure COPYDOC to extract
C      ASPIC HELP information from the LOCAL help class into
C      its proper class, prior to release.
C
C      Written by K F Hartley at RGO on 15 November 1982
C
C-


C
C****** Note it needs function from RGOLIB/LIB
C


      CHARACTER*80 RECORD,NAME*24
      INTEGER TRULEN
      OPEN (UNIT=1,FILE='ASPDIR:LOCAL.HLP',STATUS='OLD',
     :      ERR=800,READONLY)
      OPEN (UNIT=3,FILE='LOCAL.LIS',STATUS='NEW',CARRIAGECONTROL='LIST')
      RECORD=' '
      OPEN (UNIT=4,FILE='LOCAL.HLP',STATUS='NEW',CARRIAGECONTROL='LIST')
C
C   Searches the input file for a record starting with a "2",
C   copying the information as it goes.
C   (This has the effect of deleting what follows from the original
C   file).
C
      DO WHILE (RECORD(1:1).NE.'2')
         READ (1,'(A)',END=810) RECORD
         L=TRULEN(RECORD)
         IF (RECORD(1:1).NE.'2') WRITE (4,'(A)') RECORD(1:L)
      END DO
C
C   It loops back to here each time it finds a new piece of help,
C   recognized by a "2" as the first character.
C
  100 CONTINUE
      L=TRULEN(RECORD)
C
C   Create a name for the output file.
C
      NAME=RECORD(3:L)//'.EXT'
      WRITE (3,'(A)') NAME
      OPEN (UNIT=2,FILE=NAME,STATUS='NEW',CARRIAGECONTROL='LIST')
      WRITE (2,'(A)') RECORD(1:L)
      RECORD=' '
C
C      Continue copying information from the .HLP file
C      into this .EXT file until a new HELP section is found.
C
      DO WHILE (RECORD(1:1).NE.'2')
         READ (1,'(A)',END=899) RECORD
         L=TRULEN(RECORD)
         IF (RECORD(1:1).NE.'2') WRITE (2,'(A)') RECORD(1:L)
      END DO
      CLOSE (UNIT=2)
C
C   Go back to open a new file of type .EXT
C
      GO TO 100
C
C   Various error conditions.
C
  800 CONTINUE
         CALL WRUSER('Failed to open LOCAL.HLP',ISTAT)
         GO TO 899
  810 CONTINUE
         CALL WRUSER('Failed to find any program documentation',ISTAT)
         GO TO 899
  899 CONTINUE
C
C   Close all files and exit.
C
      CLOSE (UNIT=1)
      CLOSE (UNIT=2)
      CLOSE (UNIT=3)
      CLOSE (UNIT=4)
      CALL EXIT
      END
