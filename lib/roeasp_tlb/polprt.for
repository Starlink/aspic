      SUBROUTINE POLPRT (TITLE,XEXT,YEXT,COEFX,COEFY,ICELL,
     :                   MAXPTS,NPTS,X,Y,F,FIT,STATUS)
C+
C     POLPRT.
C
C     Subroutine to print the details, principally the
C     extracted points and evaluated fit, of 
C     a polynomial least squares fit to determine the
C     background of an image array.
C
C  Given;
C   TITLE  (C)  Title to appear on the printout.
C   IXEXT  (I)  X extent of the image fitted.
C   IYEXT  (I)  Y   "    "   "    "     "   .
C   COEFX  (I)  No. of X coefficients in the fit.
C   COEFY  (I)  " . "  Y      "       "   "   " .
C   ICELL  (I)  Size of the side of the averaging cell used.
C   MAXPTS (I)  Size of arrays X, Y, F and FIT, below.
C   NPTS   (I)  No. of extracted data points.
C   X      (DA) X coords. of extracted points.
C   Y      (DA) Y   "   . "      "       "   .
C   F      (DA) Background value for extracted points.
C   FIT    (DA) Fitted polynomial evaluated at the extracted point.
C
C  Returned;
C   STATUS  (I)  Return status. = 0 for successful return,
C                otherwise non-zero.
C
C  Subroutines called;
C   System:-   DATE, TIME.
C
C  Structure:-
C   Attempt to open the output file.
C   If file opens Ok then
C     Obtain time and date fcrom system.
C     Print header
C     Do for all points
C       If a page throw is required then
C         throw page.
C         print heading
C       end if
C       Scale coords back to pixel coords.
C       Compute residuals
C       print values for current point
C     end do
C     Attempt to close the file, disposing of by sending to printer.
C   end if
C   return status = max(open status,close status)
C
C  A C Davenhall./ROE/                                 15/8/82.
C-
      INTEGER COEFX,COEFY,MAXPTS,NPTS,STATUS,ICELL,XEXT,YEXT
      DOUBLE PRECISION X(MAXPTS),Y(MAXPTS),F(MAXPTS),FIT(MAXPTS)
      CHARACTER TITLE*(*)
C
      INTEGER PGFULL,LNCNT,PGCNT,PTS
      PARAMETER (PGFULL=70)
C
C    Unit no. for Fortran I/O.
C
      INTEGER UNIT
      PARAMETER (UNIT=28)
C
      REAL DIFF,DIFFMU,INTEN,XX,YY
C
      REAL POGSON
      PARAMETER (POGSON=2.5E0)
C
      INTEGER OSTAT,CSTAT
      CHARACTER DATEB*9,TIMEB*8
C
C
C    Force the number of points to be sensible.
C
      PTS=MIN(NPTS,MAXPTS)
C
C    Attempt to open the output file.
C
      OSTAT=0
      CSTAT=0
      OPEN(UNIT=UNIT,FILE='FITBAK.TMP',STATUS='NEW',IOSTAT=OSTAT)
      IF (OSTAT.EQ.0) THEN
C
C    Obtain the time and date from the system.
C
        CALL DATE (DATEB)
        CALL TIME (TIMEB)
C
C    Print the heading.
C
        WRITE(UNIT,2000) TIMEB,DATEB,TITLE,COEFX,COEFY,ICELL
 2000   FORMAT(1H1,2X,'Page 1',104X,A8,1X,A9/56X,A20//51X,
     :     'Polynomial Background Fitting.',6(/),51X,
     :     'Number of X coefficients = ',I3//51X,
     :     'Number of Y coefficients = ',I3//51X,
     :     'Averaging cell side size = ',I3///)
        WRITE(UNIT,2001)
 2001   FORMAT(25X,'X',10X,'Y',10X,'Observed',8X,'Fitted',
     :     7X,'Intensity',5X,'Residual',5X,'Residual'/
     :     22X,'(Pixels)',3X,'(Pixels)',7X,'value',10X,
     :     'value',33X,'(magnitudes)'/)
C
C    Print points.
C
        PGCNT=1
        LNCNT=20
        DO I=1,PTS
          LNCNT=LNCNT+1
C
C    Perform a pagethrow if necessary.
C
          IF (LNCNT.GE.PGFULL) THEN
            LNCNT=0
            PGCNT=PGCNT+1
            WRITE(UNIT,2002) PGCNT,DATEB,TIMEB
 2002       FORMAT(1H1,2X,'Page',I2,104X,A8,1X,A9/)
            WRITE(UNIT,2001)
          END IF
C
C    Scale the X and Y coords. back to pixel coords.
C
          XX=(X(I)*FLOAT(XEXT))+(FLOAT(XEXT)/2.0E0)
          YY=(Y(I)*FLOAT(YEXT))+(FLOAT(YEXT)/2.0E0)
C
C    Compute the intensity relative to the sky and the residuals.
C
          INTEN=F(I)/FIT(I)
          DIFF=INTEN-1.0E0
          IF (ABS(DIFF).LE.1.0E-5) THEN
            DIFFMU=0.0E0
          ELSE
            DIFFMU=POGSON*ALOG10(ABS(DIFF))
          END IF
C
C    Write out the points.
C
          WRITE(UNIT,2003) I,XX,YY,F(I),FIT(I),INTEN,DIFF,DIFFMU
 2003     FORMAT(15X,I3,0PF10.1,1X,F10.1,7X,1PD10.3,5X,D10.3,2X,
     :      0PF10.3,3X,F10.3,3X,F10.3)
        END DO
C
C    Attempt to close the file.
C
        CLOSE(UNIT=UNIT,DISPOSE='PRINT/DELETE',IOSTAT=CSTAT)
      END IF
C
C    Set the return status.
C
      STATUS=MAX(OSTAT,CSTAT)
      END
