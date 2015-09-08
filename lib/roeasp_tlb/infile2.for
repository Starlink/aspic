        SUBROUTINE INFILE2 (PROMPT,MAXPTS,XPTS,YPTS,NPTS)
C+
C         Subroutine to read in a VAX VMS file consiting of
C         a set of data pairs (of type real), one pair
C         per record.
C
C         Structure:-
C         Do until (valid filename)
C           input filename
C           open file.
C         end do
C         Do while (less than max permitted no. of records & more data)
C           read record from file.
C         end do
C         Close file.
C
C         Given;
C         PROMPT - Prompt to get filename from user (character).
C         MAXPTS - Maximum permitted no. of data points (integer).
C                  (= to size of arrays XPTS,YPTS)
C
C         Returned;
C         XPTS - Array containing radial velocities of observed pts (real).
C         YPTS -   "        "     intensities       "      "     "  (real).
C         NPTS - number of data points (integer).
C
C         A C Davenhall. /ROE/                                        18/12/81.
C-
        REAL XPTS(MAXPTS),YPTS(MAXPTS)
        INTEGER MAXPTS,NPTS
        INTEGER UNIT4,STAT1,STAT2
        LOGICAL VALID
        CHARACTER PROMPT*(*)
        CHARACTER FILNME*80
C
C         Fortran unit no. for reading file;
C
        DATA UNIT4/51/
C
C        Get a valid filename & open the file.
C
        VALID=.FALSE.
        DO WHILE (.NOT.VALID)
          STAT1=0
          CALL READC ('FILEIN',PROMPT,
     :     'ALBATROSS',' ','}',FILNME,STAT1)
          OPEN (UNIT=UNIT4,FILE=FILNME,STATUS='OLD',IOSTAT=STAT2)
          STAT1=MAX(STAT1,STAT2)
          IF (STAT1.EQ.0) VALID=.TRUE.
        END DO
C
C         Read in profile.
C
        NPTS=0
        DO WHILE (NPTS.LT.MAXPTS.AND.STAT1.EQ.0)
          NPTS=NPTS+1
          READ(UNIT4,*,IOSTAT=STAT1) XPTS(NPTS),YPTS(NPTS)
        END DO
        IF (STAT1.NE.0) NPTS=NPTS-1
C
C         Close file
C
        CLOSE (UNIT=UNIT4)
        END
