      SUBROUTINE FLTIMG (XEXT,YEXT,IMAGE,BAKIMG,TITLE,
     :                   COEFX,COEFY,ICELL,MAXPTS,NPTS,
     :                   X,Y,F,FIT,WORKPT,WORK1,WORK2,WORK3,
     :                   NORIMG)
C+
C    FLTIMG.
C
C    Subroutine to normalise an image array by dividing
C    a fitted polynomial into the original image.
C    Various other details are also taken care of;
C    Details of fitted background are sent to the printer
C    and a countour map, plus slices through the 
C    background are plotted on the Versatec.
C
C  Given;
C   XEXT   (I)  X extent of the image
C   YEXT   (I)  Y   "    "   "    "  .
C   IMAGE  (RA) Original image array.
C   BAKIMG (RA) Fitted background array.
C   TITLE  (C)  title to appear on listings and contour maps.
C   COEFX  (I)  No. of X coefficients in the fit.
C   COEFY  (I)  " . "  Y      "       "   "   " .
C   ICELL  (I)  Size of the side of the averaging cell used.
C   MAXPTS (I)  Size of arrays X, Y, F and FIT below.
C   NPTS   (I)  no. of extracted data points used in fit.
C   X      (DA) X coords. of extracted points.
C   Y      (DA) Y   "   . "     "        "   .
C   F      (DA) Background value for extracted points.
C   FIT    (DA) Fitted polynomial evaluated at the extracted point.
C   WORKPT (I)  Size of arrays WORK1, WORK2 and WORK3, below.
C
C  Used;
C   WORK1  (RA) Work array, size = WORKPT.
C   WORK2  (RA)  "     "  ,  "   =   "   .
C   WORK3  (RA)  "     "  ,  "   =   "   .
C
C  Returned;
C   NORIMG (RA) Image array containing the original image
C               divided by the fitted background.
C
C  Subroutines called;
C   Interfaces:- OUTPUT.
C   E2D:-        DIV1, POLPRT, VERPLT, VRSTRP.
C
C  A C Davenhall./ROE/                          16/8/82.
C-
      INTEGER XEXT,YEXT,COEFX,COEFY,ICELL,MAXPTS,NPTS,WORKPT
      REAL IMAGE(XEXT,YEXT),BAKIMG(XEXT,YEXT),
     :     NORIMG(XEXT,YEXT)
      DOUBLE PRECISION X(MAXPTS),Y(MAXPTS),F(MAXPTS),
     :                 FIT(MAXPTS)
      REAL WORK1(WORKPT),WORK2(WORKPT),WORK3(WORKPT)
      CHARACTER TITLE*(*)
C
      INTEGER IOSTAT,PRSTAT
C
C    Number of contours to appear on the map of the
C    fitted background.
C
      INTEGER CONT
      PARAMETER (CONT=5)
C
      INTEGER LANE
C
C
C    Divide the fitted polynomial into the original image
C    to give the normalised image.
C
      CALL DIV1 (XEXT,YEXT,IMAGE,XEXT,YEXT,BAKIMG,
     :           XEXT,YEXT,NORIMG)
C
C    Send the details of the points used to make the background
C    fit to the printer.
C
      CALL POLPRT (TITLE,XEXT,YEXT,COEFX,COEFY,ICELL,
     :             MAXPTS,NPTS,X,Y,F,FIT,PRSTAT)
      IF (PRSTAT.NE.0) CALL OUTPUT (
     :    ' ***ERROR Unable to send output to the printer.',
     :      IOSTAT)
C
C    Plot a contour map of the evaluated background on the
C    Versatec.
C
      CALL VERPLT (BAKIMG,XEXT,YEXT,CONT,TITLE)
C
C    Plot slices through the middle of the arrays showing
C    the original image and the fit superposed.
C
C    First parallel to the X axis.
C
      LANE=XEXT/2
      CALL VRSTRP (XEXT,YEXT,IMAGE,BAKIMG,1,LANE,WORKPT,
     :             WORK1,WORK2,WORK3)
C
C    Parallel to Y axis.
C
      LANE=XEXT/2
      CALL VRSTRP (XEXT,YEXT,IMAGE,BAKIMG,2,LANE,
     :             WORKPT,WORK1,WORK2,WORK3)
      END
