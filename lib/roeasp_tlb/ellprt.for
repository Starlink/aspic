      SUBROUTINE ELLPRT (PARMS,MAXCNT,ELLPAR,TITLE,STATUS)
C+
C     ELLPRT.
C
C     Subroutine to list the parameters determined for
C     ellipses fitted to a series of contours extracted
C     from a galaxy image. The listing is automatically
C     sent to the printer.
C
C  Given;
C   PARMS  (I)  No. of parameters for each ellipse (=6) .
C   MAXCNT (I)  No. of ellipses fitted.
C   ELLPAR (RA) Array holding parameters.
C   TITLE  (C)  Title for listing.
C
C  Returned;
C   STATUS (I)  Return status. = max(input status, internal status)
C               internal status = 0 for successful completion,
C               otherwise non-zero.
C
C  Subroutines called;
C   None.
C
C  A C Davenhall./ROE/                                   23/9/82.
C-
      INTEGER PARMS,MAXCNT,STATUS
      REAL ELLPAR(PARMS,MAXCNT)
      CHARACTER TITLE*(*)
C
      INTEGER OSTAT,CSTAT
      CHARACTER TIMEB*8,DATEB*9
C
C    Unit no. for fortran I/O.
C
      INTEGER UNIT
      PARAMETER (UNIT=68)
C
      REAL THETA
C
C    No. of degrees in a radian.
C
      REAL RADDEG
      PARAMETER (RADDEG=5.72958E1)
C
C
C    Attempt to open output file.
C
      OSTAT=0
      CSTAT=0
      OPEN (UNIT=UNIT,FILE='ELLPRT.TMP',STATUS='NEW',
     :      IOSTAT=OSTAT)
      IF (OSTAT.EQ.0) THEN
C
C    Obtain the time and date from the system.
C
        CALL TIME (TIMEB)
        CALL DATE (DATEB)
C
C    Write the title.
C
        WRITE(UNIT,2000) TITLE,TIMEB,DATEB
2000    FORMAT(1H1,55X,A20,35X,A8,2X,A9//51X,
     :   'Parameters for fitted Ellipses.'////)
        WRITE(UNIT,2001)
2001    FORMAT(/30X,'Log I',5X,'X Centre',5X,'Y Centre',
     :   2X,'Semi-major',2X,'Ellipticity',5X,
     :   'Orientation'/66X,'Axis')
        DO K=1,MAXCNT
          THETA=ELLPAR(6,K)*RADDEG
          WRITE(UNIT,2002) K,(ELLPAR(I,K),I=1,5),THETA
2002      FORMAT(20X,I3,2X,0PF10.3,2X,F10.3,3X,F10.3,
     :           1X,F10.3,2X,F10.3,6X,F10.3)
        END DO
C
C    Attempt to close the output file and dispose of by sending to 
C    the printer.
C
        CLOSE(UNIT=UNIT,DISPOSE='PRINT/DELETE',IOSTAT=CSTAT)
      END IF
C
C    Set the return status.
C
      STATUS=MAX(STATUS,OSTAT,CSTAT)
      END
