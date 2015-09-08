      SUBROUTINE STDEV (XEXT,YEXT,ARRAY,XBASE,YBASE,XTOP,YTOP,
     :                  AVRAGE,STDERR)
C+
C     STDEV.
C
C     Subroutine to compute the standard error on the mean
C     from a patch of a 2D array.
C
C  Given;
C   XEXT   (I)  X size of array.
C   YEXT   (I)  Y  "   "    "  .
C   ARRAY  (I)  Array.
C   XBASE  (I)  Lower X coord. of required region.
C   YBASE  (I)    "   Y   "  . "     "       "   .
C   XTOP   (I)  Upper X   "  . "     "       "   .
C   YTOP   (I)    "   Y   "  . "     "       "   .
C   AVRAGE (R)  Mean already found for required region.
C
C  Returned;
C   STDERR (R)  Standard error on mean in the required region.
C
C  Subrouties called;
C   None.
C
C  A C Davenhall./ROE/                               28/10/82.
C-
      INTEGER XEXT,YEXT,XBASE,YBASE,XTOP,YTOP
      REAL ARRAY(XEXT,YEXT)
      REAL AVRAGE,STDERR
C
      DOUBLE PRECISION SUM
      INTEGER NPTS
C
C
      SUM=0.0D0
      NPTS=0
C
      DO J=YBASE,YTOP
        DO I=XBASE,XTOP
          NPTS=NPTS+1
          SUM=SUM+((ARRAY(I,J)-AVRAGE)**2)
        END DO
      END DO
C
      STDERR=SQRT(SUM/FLOAT(NPTS*(NPTS-1)))
C
      END
