      SUBROUTINE FITBK1 (XEXT,YEXT,IMAGE,NAVOID,AVPAR,AVOID,COEFX,
     :                   COEFY,ICELL,TITLE,SAVOID,BAKIMG,NORIMG)
C+
C     FITBK1.
C
C     Subroutine to fit a polynomial background to an input
C     image, the fit avoiding a set of predefined exclusion
C     zones. The polynomial evaluated over the image and
C     the image divided by the polynomial are returned to
C     the calling routine.
C
C  Given;
C   XEXT   (I)  X extent of the input image.
C   YEXT   (I)  Y   "    "   "    "     "  .
C   IMAGE  (RA) Input image to be fitted.
C   NAVOID (I)  No. of exclusion zones.
C   AVPAR  (I)  No. of params for each zone (at least 4).
C   AVOID  (IA) Array to hold coords. of exclusion zones.
C   COEFX  (I)  No. of X coefficients to be fitted.
C   COEFY  (I)  " . "  Y      "       "  "    "   .
C   ICELL  (I)  Size averaging cell side.
C   TITLE  (C)  Title to appear on listings etc.
C
C  Used;
C   SAVOID (IA) Array, the same size as AVOID.
C
C  Returned;
C   BAKIMG (RA) Polynomial evaluated over the image.
C   NORIMG (RA) Input image divided by the corresponding
C               point in the evaluated polynomial.
C
C  Subroutines called;
C   Interfaces:-  OUTPUT.
C   Fings:-       WINDOL.
C   Graphics:-    STOPLOT.
C   Args:-        SRINIT, ARGS_OVWRT, ARGS_OVCOL, ARGS_OVGEN,
C                 ARGS_OVCLR.
C   E2D:-         ARGSET, EXTRBK, SURFIT, EVABAK, FLTIMG.
C
C  Structure:-
C   Initialise the Args.
C   Setup for plotting Fings to the appropriate overlay plane.
C   Clear any previous avoidance boxes.
C   Extract points from the background.
C   Perform fit
C   If fit Ok
C     Evaluate the background
C     If background evaluated ok
C       divide polynomial into input image to give normalised image
C       Print results out on the printer and the versatec.
C     else
C       print message
C     end if
C   else
C     print message
C   end if
C
C  A C Davenhall./ROE/                                   17/8/82.
C-
      INTEGER XEXT,YEXT,COEFX,COEFY,ICELL
      REAL IMAGE(XEXT,YEXT),BAKIMG(XEXT,YEXT),
     :     NORIMG(XEXT,YEXT)
      INTEGER NAVOID,AVPAR
      INTEGER AVOID(NAVOID,AVPAR),SAVOID(NAVOID,AVPAR)
      CHARACTER TITLE*(*)
C
C    Variables to control graphics plotted to the Args.
C
      CHARACTER COLOUR*1
      PARAMETER (COLOUR='Y')
      INTEGER PLANE
      PARAMETER (PLANE=11)
C
      INTEGER MAXPTS,NPTS
      PARAMETER (MAXPTS=4100)
      DOUBLE PRECISION X(MAXPTS),Y(MAXPTS),F(MAXPTS),
     :                 FIT(MAXPTS),WEIGHT(MAXPTS)
C
      INTEGER MAXPAR
      PARAMETER (MAXPAR=30)
      DOUBLE PRECISION PARAM(MAXPAR)
C
      INTEGER MAXLNE
      PARAMETER (MAXLNE=70)
      DOUBLE PRECISION XMINP(MAXLNE),XMAXP(MAXLNE),YVAL(MAXLNE)
      INTEGER LINEP(MAXLNE)
C
      INTEGER NWORK
      PARAMETER (NWORK=5500)
      DOUBLE PRECISION WORK(NWORK)
C
      INTEGER WORKPT
      PARAMETER (WORKPT=1024)
      DOUBLE PRECISION WORKX(WORKPT),WORKF(WORKPT)
      REAL WORK1(WORKPT),WORK2(WORKPT),WORK3(WORKPT)
C
      INTEGER IFAIL,IFAIL1,IFAIL2,BKSTAT
C
C
C    Initialise the Args.
C
      CALL SRINIT (0,.FALSE.,IFAIL)
C
C    Set up for plotting Fings graphics to the Args.
C
      CALL ARGSET (0)
      CALL WINDOL (0.0E0,5.11E2,0.0E0,5.11E2)
      CALL ARGS_OVWRT (PLANE)
      CALL ARGS_OVCOL (PLANE,COLOUR)
      CALL ARGS_OVGEN ('W')
C
C    Clear any previous extraction cells from the Args screen.
C
      CALL ARGS_OVCLR (PLANE)
C
C    Extract points from the background.
C
      CALL EXTRBK (IMAGE,XEXT,YEXT,ICELL,NAVOID,NAVOID,
     :              AVOID,MAXPTS,SAVOID,NPTS,X,Y,F)
C
C    Terminate fings graphics.
C
      CALL STOPLOT
C
C    Perform fit.
C
      CALL OUTPUT (
     : ' Please wait. Fit being made now.',IOSTAT)
      IFAIL1=0
      IFAIL2=0
      CALL SURFIT (X,Y,F,MAXPTS,NPTS,MAXPAR,MAXLNE,NWORK,COEFX,
     :             COEFY,WEIGHT,LINEP,XMINP,XMAXP,YVAL,WORK,PARAM,
     :             FIT,IFAIL1,IFAIL2)
C
C    Proceed if the fit has been completed successfully.
C
      IF (IFAIL1.EQ.0.AND.IFAIL2.EQ.0) THEN
C
C    Evaluate the background.
C
        BKSTAT=0
        CALL EVABAK (MAXPAR,PARAM,COEFX,COEFY,XEXT,YEXT,WORKF,
     :               WORKX,WORKPT,WORK,NWORK,BAKIMG,BKSTAT)
        IF (BKSTAT.EQ.0) THEN
C
C    Send results to the printer and Versatec and evaluate the
C    normalised image.
C
          CALL OUTPUT (
     :     ' Successful fit. Hardcopies now being produced.',
     :       IOSTAT)
          CALL FLTIMG (XEXT,YEXT,IMAGE,BAKIMG,TITLE,COEFX,COEFY,
     :                 ICELL,MAXPTS,NPTS,X,Y,F,FIT,WORKPT,
     :                 WORK1,WORK2,WORK3,NORIMG)
          CALL OUTPUT (' Hard copies produced.',IOSTAT)
        ELSE
          CALL OUTPUT (
     :  ' ***ERROR Unable to evaluate Polynomial background.',
     :    IOSTAT)
        END IF
      ELSE
        CALL OUTPUT (
     :  ' ***ERROR Fit to background unsuccessful.',IOSTAT)
      END IF
C
C    Clear the extraction cells from the Args screen.
C
      CALL ARGS_OVCLR (PLANE)
      END
