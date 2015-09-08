      SUBROUTINE EXTRBK (IMAGE,XEXT,YEXT,ICELL,MAXAVD,NAVOID,
     :                   AVOID,MAXPTS,SAVOID,NPTS,X,Y,F)
C+
C     EXTRBK.
C
C     Subroutine to extract a set of background values
C     from an image array, avoiding a set of pre-determined
C     avoidance boxes.
C
C  Given;
C   IMAGE  (RA)  Image array.
C   XEXT   (I)   X size of image array.
C   YEXT   (I)   Y  "   "    "     "  .
C   ICELL  (I)   Size of a side of the (square) averaging
C                cell for extracting the background values.
C   MAXAVD (I)   Max. permitted no. of avoidance boxes.
C   NAVOID (I)   Actual no.  avoidance boxes.
C   AVOID  (IA)  Array holding coords. of avoidance boxes.
C   MAXPTS (I)   Size of arrays to hold extracted points
C                (below).
C
C  Used;
C   SAVOID (IA)  Scaled avoidance boxes. Dimensionality the
C                same as AVOID.
C
C  Returned;
C   NPTS   (I)   No. of extracted background points.
C   X      (DA)  X coords. of extdacted background points.
C   Y      (DA)  Y   "   . "     "          "        "   .
C   F      (DA)  Background value for extdacted points.
C
C  Subroutines called;
C   E2D:-       FORBID, MEDBOX.
C   Graphics:-  DRABOX.
C   Args:-      ARGS_NUMIM, ARGS_PTOA.
C
C  Structure:-
C   Convert the coords. of the avoidance boxes to coordinates
C    scaled to the cellsize.
C   do xcoord=1,xmax,incrementing by the cellsize
C     do ycoord=1,ymax,incrementing by the cellsize
C       if outside all avoidance boxes
C         extract the median value for this cell
C         draw a box on the Args corresponding to the box being
C           extracted.
C         Compute the coords of the box centre.
C       end if
C     end do
C   end do
C   For all extracted points
C     scale coords to range for -1.0 --> +1.0
C   end for.
C
C  A C Davenhall./ROE/                                  10/8/82.
C  A C Davenhall./ROE/ {Modified to use Args database}. 18/3/83.
C-
      INTEGER XEXT,YEXT,ICELL,MAXAVD,NAVOID,MAXPTS
      DOUBLE PRECISION X(MAXPTS),Y(MAXPTS),F(MAXPTS)
      REAL  IMAGE(XEXT,YEXT)
      INTEGER AVOID(MAXAVD,4),SAVOID(MAXAVD,4)
C
      INTEGER IC,JC,JTOP,ITOP,JBASE,IBASE
      INTEGER IMGNO,ITOPA,JTOPA,IBASEA,JBASEA,ARGSTT
      INTEGER XLIM,YLIM
      LOGICAL ZONE
      REAL MEDIAN
C
C
C    Obtain the number of the last image plotted on the Args.
C
      CALL ARGS_NUMIM (IMGNO)
C
C    Convert the coordinates of the avoidance boxes from
C    pixel coords. to coords. scaled by the cellsize.
C
      DO I=1,NAVOID
        DO J=1,4
          SAVOID(I,J)=AVOID(I,J)/ICELL
          IF (J.EQ.3.OR.J.EQ.4) SAVOID(I,J)=SAVOID(I,J)+1
        END DO
      END DO
C
C    Extract medians corresponding to all the points
C    inside the cells.
C
      NPTS=0
C
      IF (MOD(XEXT,ICELL).EQ.0) THEN
        XLIM=XEXT
      ELSE
        XLIM=XEXT-ICELL
      END IF
C
      IF (MOD(YEXT,ICELL).EQ.0) THEN
        YLIM=YEXT
      ELSE
        YLIM=YEXT-ICELL
      END IF
C
      DO J=1,YLIM,ICELL
        DO I=1,XLIM,ICELL
C
C    Compute the coords. of this cell scaled to the averaging
C    cellsize.
C     
          JC=J/ICELL
          IC=I/ICELL
C
C    Determine whether or not this cell lies within an
C    avoidance box.
C
          CALL FORBID (JC,IC,SAVOID,NAVOID,MAXAVD,ZONE)
C
C    Proceed to compute values for the cell if it lies outside
C    all zones.
C
          IF (.NOT.ZONE) THEN
C
C    Compute the median.
C
            JBASE=J
            IBASE=I
            JTOP=J+ICELL-1
            ITOP=I+ICELL-1
            CALL MEDBOX (IMAGE,XEXT,YEXT,IBASE,ITOP,JBASE,JTOP,
     :                   MEDIAN)
C
C    Compute the coords. of the centre of the box
C    and save the value found for the median.
C
            NPTS=NPTS+1
            X(NPTS)=FLOAT(I)+(FLOAT(ICELL)/2.0E0)-5.0E-1
            Y(NPTS)=FLOAT(J)+(FLOAT(ICELL)/2.0E0)-5.0E-1
            F(NPTS)=MEDIAN
C
C    Convert from pixel to Args coords. and draw the cell.
C
            CALL ARGS_PTOA (IMGNO,IBASE-1,JBASE-1,IBASEA,JBASEA,ARGSTT)
            CALL ARGS_PTOA (IMGNO,ITOP-1,JTOP-1,ITOPA,JTOPA,ARGSTT)
            CALL DRABOX (IBASEA,JBASEA,ITOPA,JTOPA)
          END IF
        END DO
      END DO
C
C    Scale the coords to range from -1.0 --> +1.0.
C
      DO I=1,NPTS
        X(I)=(X(I)-(FLOAT(XEXT)/2.0E0))/FLOAT(XEXT)
        Y(I)=(Y(I)-(FLOAT(YEXT)/2.0E0))/FLOAT(YEXT)
      END DO
      END
