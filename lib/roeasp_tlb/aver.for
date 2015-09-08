      SUBROUTINE AVER (XEXT,YEXT,ARRAY,XBASE,YBASE,XTOP,YTOP,
     :                  AVRAGE)
C+
C     AVER.
C
C     Subroutine to find the mean of a rectangular patch of an array.
C
C  Given;
C   XEXT   (I)  X size of image array.
C   YEXT   (I)  Y  "   "    "     "  .
C   ARRAY  (RA) Image array.
C   XBASE  (I)  Lower X coord. of region to be averaged.
C   YBASE  (I)    "   Y   "  . "    "    "  "     "    .
C   XTOP   (I)  Upper X   "  . "    "    "  "     "    .
C   YTOP   (I)    "   Y   "  . "    "    "  "     "    .
C
C  Returned;
C   AVRAGE (R)  Mean value of the array in the selected region.
C
C  Subroutines called;
C   None.
C
C  A C Davenhall./ROE/                                   25/9/82.
C-
      INTEGER XEXT,YEXT,XBASE,YBASE,XTOP,YTOP
      REAL ARRAY(XEXT,YEXT)
      REAL AVRAGE
C
      INTEGER NPTS
      DOUBLE PRECISION SUM
C
C
      SUM=0.0D0
      NPTS=0
C
      DO J=YBASE,YTOP
        DO I=XBASE,XTOP
          NPTS=NPTS+1
          SUM=SUM+ARRAY(I,J)
        END DO
      END DO
C
      AVRAGE=SUM/FLOAT(NPTS)
C
      END
