      SUBROUTINE POISON (IXEXT,IYEXT,IMAGE)
C+
C      POISON.
C
C      Subroutine to add Poisson errors to an image array.
C     (Gaussian noise with a std. deviation = sq. root of
C     the signal).
C
C  Given;
C  IXEXT  (I)  X size of image.
C  IYEXT  (I)  Y  "   "    "  .
C  IMAGE  (RA) Image array.
C
C  Returned;
C  IMAGE  (RA) Image array modified by addition of noise to each
C              pixel significantly greater than zero.
C
C  Subroutine called;
C  NAG:   G05DDF.
C
C  A C Davenhall./ROE/                                   4/3/82.
C  based on a routine by R. S. Stobie./ROE/.
C-
      INTEGER IXEXT,IYEXT
      REAL IMAGE(IXEXT,IYEXT)
C
C 
      DOUBLE PRECISION SIGNAL,STD
      REAL THRESH
      PARAMETER (THRESH=1.0E-10)
C
C
      DO J=1,IYEXT
        DO I=1,IXEXT
          IF (IMAGE(I,J).GE.THRESH) THEN
            SIGNAL=IMAGE(I,J)
            STD=DSQRT(SIGNAL)
            IMAGE(I,J)=G05DDF (SIGNAL,STD)
          END IF
        END DO
      END DO
      END
