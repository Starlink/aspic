      SUBROUTINE SURFIT (X,Y,F,MAXPTS,NPTS,MAXPAR,MAXLNE,NWORK,
     :                   COEFX,COEFY,WEIGHT,LINEP,XMINP,XMAXP,
     :                   YVAL,WORK,PARAM,FIT,IFAIL1,IFAIL2)
C+
C     SURFIT.
C
C     Subroutine to fit a surface, represented as a set of X and Y
C     positions and function values by least squares, using the
C     NAG routine E02CAF. The parameters evaluated for the fit
C     and the fitted function evaluated at each of the data
C     points are returned.
C
C  Given;
C   X      (DA) X values for points to be fitted.
C   Y      (DA) Y   "     "    "    "  "    "   .
C   F      (DA) Observed function value at point (x,y).
C   MAXPTS (I)  Size of arrays X, Y, F, FIT and WEIGHT (below).
C   NPTS   (I)  No. of observed data points.
C   MAXPAR (I)  Max. permitted no. of parameters to be determined
C               (=size of array PARAM).
C   MAXLNE (I)  Max. permitted no. of X values for a single distinct
C               Y value (= size of arrays LINEP, XMINP, XMAXP,
C               YVAL, below).
C   NWORK  (I)  Size of E02CAF work array WORK, below.
C   COEFX  (I)  No. of X coefficients to be fitted.
C   COEFY  (I)  " . "  Y      "       "  "    "   .
C
C  Used;
C   WEIGHT (DA) Array, size = MAXPTS.
C   LINEP  (IA)   "  ,  "   = MAXLNE.
C   XMINP  (DA)   "  ,  "   =   "   .
C   XMAXP  (DA)   "  ,  "   =   "   .
C   YVAL   (DA)   "  ,  "   =   "   .
C   WORK   (DA)   "  ,  "   = NWORK.
C
C  Returned;
C   PARAM  (DA) Set of coefficients evaluated by E02CAF.
C   FIT    (DA) Fitted function evaluated at each of the data points.
C   IFAIL1 (I)  Return status from E02CAF. = 0 for successful
C               return, otherwise non-zero. Explanation of 
C               non-zero returns is given in the NAG description
C               of E02CAF p6.
C   IFAIL2 (I)  Max(input status, E02CBF status). E02CBF is used
C               by routine EVAFIT to evaluate the polynomial. For
C               an explanation of its return statuses see p4. of
C               its description in the NAG manual. IFAIL2 = 0 for
C               a successful return, otherwise non-zero.
C
C  Subroutines called;
C   E2D:-   POLPRE, EVAFIT.
C   NAG:-   E02CAF.
C
C  A C Davenhall./ROE/                                 11/8/82.
C-
      INTEGER MAXPTS,NPTS,MAXLNE,MAXPAR,COEFX,COEFY,
     :        NWORK,IFAIL1,IFAIL2
      INTEGER LINEP(MAXLNE)
      DOUBLE PRECISION X(MAXPTS),Y(MAXPTS),F(MAXPTS),
     :                 WEIGHT(MAXPTS),FIT(MAXPTS)
      DOUBLE PRECISION PARAM(MAXPAR)
      DOUBLE PRECISION XMINP(MAXLNE),XMAXP(MAXLNE),YVAL(MAXLNE)
      DOUBLE PRECISION WORK(NWORK)
C
      DOUBLE PRECISION NUX(1),NUY(1)
      INTEGER YLINE
C
C
C    Convert the data to be fitted into the form required by
C    E02CAF.
C
      CALL POLPRE (X,Y,MAXPTS,NPTS,MAXLNE,YLINE,LINEP,XMINP,
     :             XMAXP,YVAL)
C
C    Set all the weights to unity.
C
      DO I=1,NPTS
        WEIGHT(I)=1.0E0
      END DO
C
C    Make the least squares fit.
C
      IFAIL1=0
      CALL E02CAF (LINEP,YLINE,COEFX,COEFY,X,YVAL,F,WEIGHT,
     :             MAXPTS,PARAM,MAXPAR,
     :             XMINP,XMAXP,NUX,1,NUY,1,WORK,NWORK,IFAIL1)
C
C    Evaluate the fitted function at each of the data points.
C
      IFAIL2=0
      CALL EVAFIT (COEFX,COEFY,MAXPTS,NPTS,X,MAXPAR,PARAM,
     :             MAXLNE,YLINE,LINEP,XMINP,
     :             XMAXP,YVAL,NWORK,WORK,FIT,IFAIL2)
      END
