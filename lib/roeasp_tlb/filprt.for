      SUBROUTINE FILPRT (TITLE1,TITLE2,HEAD1,HEAD2,MAXPTS,NPTS,
     :                   INTEN,RADIUS,STATUS)
C+
C     FILPRT.
C
C     Subroutine to take two arrays representing a profile
C     and list them on the printer, in an economically formatted
C     way, with 3 columns per page.
C
C  Given;
C   TITLE1   (C)  First title to appear on the lineprinter.
C   TITLE2   (C)  Second  "   "    "    "   "       "     .
C   HEAD1    (C)  Heading for first array.
C   HEAD2    (C)     "     "  second  "  .
C   MAXPTS   (I)  Size of arrays.
C   NPTS     (I)  No. of points in the arrays.
C   INTEN    (RA) Intensity for points in profile.
C   RADIUS   (RA) Radius     "    "    "     "   .
C
C  Returned;
C   STATUS   (I)  Return status. = 0 for successful return, otherwise
C                 non zero.
C
C  Structure:-
C   Attempt to open output file.
C   If file opens Ok
C     Obtain the time and date from the system.
C     print the title.
C     process the headings into the appropriate place.
C     print the headings.
C     print the arrays.
C     attempt to close the file (dispose of by printing and deleteing).
C   end if
C   return status=max(open status, close status)
C
C  A C Davenhall./ROE/                                      1/8/82.
C-
      CHARACTER TITLE1*(*),TITLE2*(*),HEAD1*(*),HEAD2*(*)
      INTEGER MAXPTS,NPTS,STATUS
      REAL RADIUS(MAXPTS),INTEN(MAXPTS)
C
      INTEGER OPSTAT,CLSTAT
      INTEGER LEN1,LEN2,STRT1,STOP1,STRT2,STOP2
      INTEGER NLINE,IDIFF,NLINE1,I,II,III
      CHARACTER DATEB*9,TIMEB*8
C
C    Fortran unit no. for writing the file.
C
      INTEGER UNIT
      PARAMETER (UNIT=13)
C
      CHARACTER OUTBUF*(38)
      INTEGER BUFLEN
      PARAMETER (BUFLEN=38)
C
C
C    Attempt to open the output file.
C
      OPSTAT=0
      OPEN (UNIT=UNIT,FILE='FILPRT.TMP',STATUS='NEW',IOSTAT=OPSTAT)
      IF (OPSTAT.EQ.0) THEN
C
C    Obtain the time and date from the system.
C
        CALL DATE (DATEB)
        CALL TIME (TIMEB)
C
C    Write the title.
C
        WRITE(UNIT,2000) TITLE1,TIMEB,DATEB,TITLE2
 2000   FORMAT(1H1,51X,A30,25X,A8,1X,A9//51X,A30//)
C
C    Construct and write the column headings.
C
C    Compute the length of each heading.
C
        CALL LSTCHR (HEAD1,LEN1)
        CALL LSTCHR (HEAD2,LEN2)
        STRT1=21-(LEN1/2)
        STOP1=STRT1+LEN1-1
        STRT2=34-(LEN2/2)
        STOP2=STRT2+LEN2-1
C
C    Stop any of the headings over flowing the buffer.
C
        STRT1=MAX(STRT1,1)
        STRT2=MAX(STRT2,1)
        STOP1=MIN(STOP1,BUFLEN)
        STOP2=MIN(STOP2,BUFLEN)
C
C    Assemble the output buffer.
C
        OUTBUF='  '
        OUTBUF(8:12)='Count'
        OUTBUF(STRT1:STOP1)=HEAD1
        OUTBUF(STRT2:STOP2)=HEAD2
C
C    Write the heading.
C
        WRITE(UNIT,2001) OUTBUF,OUTBUF,OUTBUF
 2001   FORMAT(A38,A38,A38/)
C
C    Finally write out the input arrays, in vertical columns,
C    3 per page.
C
        NLINE=NPTS/3
        IDIFF=NPTS-(NLINE*3)
        NLINE1=NLINE+IDIFF
        DO I=1,NLINE
          II=I+NLINE1
          III=II+NLINE
          WRITE(UNIT,2002) I,RADIUS(I),INTEN(I),II,RADIUS(II),INTEN(II),
     :                     III,RADIUS(III),INTEN(III)
 2002     FORMAT(3(I10,2X,1PE12.3,2X,1PE12.3))
        END DO
        II=NLINE+1
        III=NLINE+2
        IF (IDIFF.GE.1) WRITE(UNIT,2002) II,RADIUS(II),INTEN(II)
        IF (IDIFF.EQ.2) WRITE(UNIT,2002) III,RADIUS(III),INTEN(III)
C
C     Attempt to close the output file. Send to the printer and
C     delete.
C
        CLOSE(UNIT=UNIT,DISPOSE='PRINT/DELETE',IOSTAT=CLSTAT)
      END IF
C
C    Set the return status.
C
      STATUS=MAX(OPSTAT,CLSTAT)
      END
