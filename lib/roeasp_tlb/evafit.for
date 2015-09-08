      SUBROUTINE EVAFIT (COEFX,COEFY,MAXPTS,NPTS,X,MAXPAR,PARAM,
     :                   MAXLNE,YLINE,LINEP,XMINP,XMAXP,YVAL,
     :                   NWORK,WORK,FIT,STATUS)
C+
C     EVAFIT.
C
C     Subroutine to evaluate the polynomial fitted to a set
C     of data points by the NAG routine E02CAF. The evaluation
C     is made using the NAG routine E02CBF.
C
C  Given;
C   COEFX  (I)  No. of X coefficients fitted.
C   COEFY  (I)  " . "  Y      "         "   .
C   MAXPTS (I)  Size of arrays X and FIT (below).
C   NPTS   (I)  No. of observed data points.
C   X      (DA) X values for fitted points.
C   MAXPAR (I)  Max. permitted no. of determined parameters 
C               (= size of array PARAM, below).
C   PARAM  (DA) Set of coefficients previously evaluated by E02CAF.
C   MAXLNE (I)  Max. permitted no. of X values for a single distinct
C               Y value (= size of arrays LINEP, XMINP, XMAXP and
C               YVAL, below).
C   YLINE  (I)  No. of distinct Y lanes in the image.
C   LINEP  (IA) No. of X values in each lane.
C   XMINP  (DA) Min. X value of each lane.
C   XMAXP  (DA) Max. "   "   "   "    "  .
C   YVAL   (DA) Y value for each lane.
C   NWORK  (I)  Size of array WORK, below.
C
C  Used;
C   WORK   (DA) Array, size = NWORK.
C
C  Returned;
C   FIT    (DA) Fitted function evaluated for each of the data points.
C   STATUS (I)  Max(input status, E02CBF status).
C               E02CBF status = 0 for successful return, otherwise
C               non. zero. For an explanation of failure statuses
C               see p4. of description of E02CBF in the NAG manual.
C
C  Subroutines called;
C   NAG:-   E02CBF.
C
C  A C Davenhall./ROE/                                       15/8/82.
C-
      INTEGER MAXPTS,NPTS,MAXLNE,NWORK,MAXPAR,COEFX,COEFY,STATUS
      INTEGER YLINE
      DOUBLE PRECISION X(MAXPTS),FIT(MAXPTS)
      DOUBLE PRECISION XMINP(MAXLNE),XMAXP(MAXLNE),YVAL(MAXLNE)
      INTEGER LINEP(MAXLNE)
      DOUBLE PRECISION WORK(NWORK)
      DOUBLE PRECISION PARAM(MAXPAR)
C
      INTEGER FIRST,LAST,IFAIL
      DOUBLE PRECISION XMIN,XMAX,YVALUE,YMIN,YMAX
C
C
C    Find the maximum and minimum Y values in the data set.
C
      YMIN=YVAL(1)
      YMAX=YVAL(1)
      DO I=2,YLINE
        IF (YVAL(I).LT.YMIN) YMIN=YVAL(I)
        IF (YVAL(I).GT.YMAX) YMAX=YVAL(I)
      END DO
C
C    Evaluate the fitted polynomial sequentially for each distinct
C    Y lane.
C
      DO I=1,YLINE
        IF (I.EQ.1) THEN
          FIRST=1
          LAST=LINEP(1)
        ELSE
          FIRST=FIRST+LINEP(I-1)
          LAST=LAST+LINEP(I)
        END IF
        XMIN=XMINP(I)
        XMAX=XMAXP(I)
        YVALUE=YVAL(I)
        IFAIL=0
        CALL E02CBF (FIRST,LAST,COEFX,COEFY,X,XMIN,XMAX,YVALUE,
     :               YMIN,YMAX,FIT,PARAM,MAXPAR,WORK,NWORK,IFAIL)
        STATUS=MAX(STATUS,IFAIL)
      END DO
      END
