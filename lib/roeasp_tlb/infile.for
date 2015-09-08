        SUBROUTINE INFILE (N1,MAXPTS,NPTS,KEEP)
C+
C       INFILE.
C
C         Subroutine to read in an (equivalent) profile
C         from a VAX VMS file.
C
C         Given;
C         N1 - 2.
C         MAXPTS - Size of file (integer).
C
C         Returned;
C         NPTS - No. of pts. in profile (integer).
C         KEEP - 2D array holding profile (real).
C
C         Structure:-
C
C         Set all elements in array to zero.
C         Do until (valid filename).
C           Obtain filename from user.
C           Atempt to open file.
C         end do
C         Do while (less than permitted no. of records & more data)
C           read record from file.
C         end do
C         Close file.
C
C         Subroutines called; READC, OUTPUT.
C
C         A C Davenhall. /ROE/                                10/12/81.
C         A C Davenhall. /ROE/                                22/7/82.
C-
        REAL KEEP(N1,MAXPTS)
        INTEGER N1,MAXPTS,NPTS
        INTEGER UNIT,STAT1,STAT2
        LOGICAL VALID
        CHARACTER FILNME*30, BUFF*80
C
C         Fortran unit no. for reading file.
C
        DATA UNIT/57/
C
C         Set all the elements in the array to zero.
C
        DO I=1,MAXPTS
          KEEP(1,I)=0.0E0
          KEEP(2,I)=0.0E0
        END DO
C
C         Get a valid filename & open the file.
C
        VALID=.FALSE.
        DO WHILE (.NOT.VALID)
          STAT1=0
          CALL READC ('FILEIN',
     :           ' Enter the name of the file containing the profile;',
     :           ' Albatros','A','Z',FILNME,STAT1)
          OPEN(UNIT=UNIT,FILE=FILNME,STATUS='OLD',IOSTAT=STAT2)
          STAT1=MAX(STAT1,STAT2)
          IF (STAT1.EQ.0) VALID=.TRUE.
        END DO
C
C         Read in the profile.
C
        NPTS=0
        DO WHILE (NPTS.LT.MAXPTS.AND.STAT1.EQ.0)
          NPTS=NPTS+1
          READ(UNIT,*,IOSTAT=STAT1) KEEP(1,NPTS),KEEP(2,NPTS)
        END DO
        IF (STAT1.NE.0) NPTS=NPTS-1
        IF (NPTS.EQ.MAXPTS) THEN
          WRITE(BUFF,2000) NPTS
 2000 FORMAT(1X,'Too many points in profile. Truncated after first ',
     :          I3)
          CALL OUTPUT (BUFF,STAT1)
        END IF
C
C         Close file.
C
        CLOSE(UNIT=UNIT)
        END
