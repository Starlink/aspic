      SUBROUTINE EVABAK (MAXPAR,PARAM,COEFX,COEFY,XEXT,YEXT,
     :                   WORKF,WORKX,WORKPT,WORK,NWORK,
     :                   BAKIMG,STATUS)
C+
C     EVABAK.
C
C     Subroutine to evaluate a polynomial representation of the
C     background for an image using the coefficients obtained
C     by making a least squares fit to the image using NAG
C     routine E02CAF. The polynomial is evaluated using NAG
C     subroutine E02CBF.
C
C  Given;
C   MAXPAR  (I)  Max. permitted no. of parameters in the
C                fiting function.
C   PARAM   (DA) Array holding the function parameters.
C   COEFX   (I)  No. of X coeffs. in the fitting function.
C   COEFY   (I)   "  "  Y   "   . "   "    "        "    .
C                Note:- COEFX + COEFY must be less than MAXPAR.
C   XEXT    (I)  X size of the image to be generated.
C   YEXT    (I)  Y  "   "   "    "   "  "      "    .
C   
C  Used;
C   WORKF   (DA) Work array.
C   WORKX   (DA)  "     "  .
C   WORKPT  (I)  Size of work arrays WORKX and WORKF, must .ge.
C                XEXT.
C   WORK    (DA) Work array.
C   NWORK   (I)  Size of array WORK.
C
C  Returned;
C   BAKIMG  (RA) Image generated from the specified polynomial.
C   STATUS  (I)  Max(input status, E02CBF status). Eo2CBF status = 0
C                for successful return, otherwise non-zero. An
C                explanation of the non-zero returns is given in
C                NAG description of E02CBF, p4.
C  Subroutines called;
C   NAG:-  E02CBF.
C
C  Structure:-
C   Determine YMAX and YMIN.
C   Setup the array of X coords.
C   Find the min. and max. X coords.
C   Do for J=1,y extent
C     evaluate fit to row of x coords.
C     status=max(status, E02CBF status)
C     copy to appropriate row of the image array.
C   end do
C
C  A C Davenhall./ROE/                                10/8/82.
C-
      INTEGER MAXPAR,COEFX,COEFY,XEXT,YEXT,WORKPT,STATUS,
     :        NWORK
      REAL BAKIMG(XEXT,YEXT)
      DOUBLE PRECISION PARAM(MAXPAR),WORKX(WORKPT),WORKF(WORKPT),
     :                  WORK(NWORK)
C
      DOUBLE PRECISION XMIN,XMAX,YMIN,YMAX,YVALUE
      INTEGER XPTS,IFAIL
C
C
C    Determine YMIN and YMAX; the upper and lower bounds
C    for the Y range of the scaled Y coords. spanning the
C    array.
C
      YMIN=(1.0E0-(FLOAT(YEXT)/2.0E0))/FLOAT(YEXT)
      YMAX=(FLOAT(YEXT)-(FLOAT(YEXT)/2.0E0))/FLOAT(YEXT)
C
C    Set up the array of X coords., forcing the number of pts.
C    not to overflow the array.
C
      XPTS=MIN(XEXT,WORKPT)
      DO I=1,XPTS
        WORKX(I)=(FLOAT(I)-(FLOAT(XEXT)/2.0E0))/FLOAT(XEXT)
      END DO
C
C    Setup the min. and max. X coord.
C
      XMIN=WORKX(1)
      XMAX=WORKX(XPTS)
C
C    Evaluate each X row of the background image sequentially.
C
      DO J=1,YEXT
C
C    Evaluate the Y value for this row.
C
        YVALUE=(FLOAT(J)-(FLOAT(YEXT)/2.0E0))/FLOAT(YEXT)
C
C    Compute row of X values.
C
        IFAIL=0
        CALL E02CBF (1,XPTS,COEFX,COEFY,WORKX,
     :               XMIN,XMAX,YVALUE,YMIN,YMAX,WORKF,
     :               PARAM,MAXPAR,WORK,NWORK,IFAIL)
        STATUS=MAX(STATUS,IFAIL)
C
C    Copy to background image array.
C
        DO I=1,XPTS
          BAKIMG(I,J)=WORKF(I)
        END DO
      END DO
      END
