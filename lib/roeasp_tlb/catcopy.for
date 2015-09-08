      SUBROUTINE CATCOPY (MAXSTR,NSTAR,XCORD,YCORD,APMAG,CATAL)
C+
C      CATCOPY.
C
C      Subroutine to copy individual arrays containing the positions
C     and apparent magnitudes of a list of stars into a single
C     "catlogue" array.
C
C  Given;
C  MAXSTR (I)  Max. permitted no. of stars (= size of arrays
C              XCORD, YCORD and APMAG, below).
C  NSTAR  (I)  Number of stars.
C  XCORD  (RA) X positions of stars.
C  YCORD  (RA) Y    "      "    "  .
C  APMAG  (RA) Apparent magnitudes of stars.
C
C  Returned;
C  CATAL  (RA) Singe array containing star paramters.
C
C  Subroutines called;  None.
C
C  A C Davenhall./ROE/                           4/3/82.
C  A C Davenhall./ROE/           {Modified}     11/8/82.
C-
      INTEGER MAXSTR,NSTAR
      REAL XCORD(MAXSTR),YCORD(MAXSTR),APMAG(MAXSTR)
      REAL CATAL(3,NSTAR)
C
C
      INTEGER ISTAR
C
C
      ISTAR=MIN(NSTAR,MAXSTR)
      DO I=1,ISTAR
        CATAL(1,I)=XCORD(I)
        CATAL(2,I)=YCORD(I)
        CATAL(3,I)=APMAG(I)
      END DO
      END
