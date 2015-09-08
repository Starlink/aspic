      SUBROUTINE DRAXIS (XMIN,XMAX,YMIN,YMAX,LABELX,LABELY)
C+
C      Subroutine to define a plotting space (ie. call WINDOL)
C      and draw a set of axes from (XMIN,YMIN) to (XMAX,YMAX).
C        The plotting space exceeds the X and Y ranges by 0.2
C      in each dimension in order to allow room for annotation.
C
C  Given;
C  XMIN   (R)   Minimum X coord. of axis.
C  XMAX   (R)   Maximum X   "  . "   "  .
C  YMIN   (R)   Minimum Y   "  . "   "  .
C  YMAX   (R)   Maximum Y   "  . "   "  .
C  LABELX (C)   Label for X axis.
C  LABELY (C)     "    "  Y  "  .
C
C  Returned; None.
C
C  Subroutines called;
C  WINDOL, AXIS.
C
C  A C Davenhall./ROE/                             4/2/82.
C-
      REAL XMIN,XMAX,YMIN,YMAX
      CHARACTER LABELX*(*),LABELY*(*)
C
      REAL RANGE,XMIN1,XMAX1,YMIN1,YMAX1
      RANGE=XMAX-XMIN
      XMIN1=XMIN-(0.2*RANGE)
      XMAX1=XMAX+(0.2*RANGE)
      RANGE=YMAX-YMIN
      YMIN1=YMIN-(0.2*RANGE)
      YMAX1=YMAX+(0.2*RANGE)
      CALL WINDOL (XMIN1,XMAX1,YMIN1,YMAX1)
      CALL AXIS (XMIN,XMAX,YMIN,YMAX,LABELX,LABELY)
      END
