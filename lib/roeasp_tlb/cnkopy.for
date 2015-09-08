      SUBROUTINE CNKOPY (MAXPAR,MAXPTS,MAXCNT,ELLPAR1,LOGI1,
     :                   NUMCNT,ELLPAR2,LOGI2)
C+
C     CNKOPY.
C
C     Subroutine to copy work arrays to output arrays for
C     subroutine CNTEXTS.
C
C  Given;
C   MAXPAR (I)  No. of parameters for each point in contour (=2).
C   MAXPTS (I)  Max. no. of points in each contour.
C   MAXCNT (I)  Max. no. of contours.
C   ELLPAR1 (RA) work array holding contours.
C   LOGI1  (RA) Work array holding log I values.
C   NUMCNT (I)  Actual no. of contours found.
C
C  Returned;
C   ELLPAR2 (RA) Output array holding contours.
C   LOGI2  (RA) Output array holding Log I values.
C
C  Subroutines called;
C   None.
C
C  A C Davenhall./ROE/                                   25/9/82.
C-
      INTEGER MAXPAR,MAXPTS,MAXCNT,NUMCNT
      REAL  ELLPAR1(MAXPAR,MAXPTS,MAXCNT),
     :      ELLPAR2(MAXPAR,MAXPTS,NUMCNT),
     :      LOGI1(MAXCNT),
     :      LOGI2(NUMCNT)
C
      DO K=1,NUMCNT
        DO J=1,MAXPTS
          DO I=1,MAXPAR
            ELLPAR2(I,J,K)=ELLPAR1(I,J,K)
          END DO
        END DO
        LOGI2(K)=LOGI1(K)
      END DO
      END
